# Joypad `/OE` (`joypad1_oe` / `joypad2_oe`) vs. zero-page and stack-page accesses

Research-only. No code changed. Primary sources: local code and the local
NESdev wiki mount at `nesdev-wiki/wikipages/` (cited as
`nesdev-wiki/wikipages/<Page>.xhtml`, i.e. https://www.nesdev.org/wiki/<Page>).

## 1. What the fields currently do in `nes_core/src/nes.rs`

- Fields: `nes_core/src/nes.rs:33-34` — `joypad1_oe: bool`, `joypad2_oe: bool`,
  initialised `false` in `new_with_mmc3_irq_override` (`nes_core/src/nes.rs:96-97`).
  Doc comment (`nes_core/src/nes.rs:30-32`): they model NES-001 hardware where
  `/OE` stays asserted across contiguous reads of the same register, so the
  4021 shift register clocks once per contiguous set, not once per read.
- Generic `read` (`nes_core/src/nes.rs:247-253`): at the top of every read,
  `std::mem::replace(&mut self.joypad1_oe, address == 0x4016)` (and likewise
  `joypad2_oe` / `0x4017`). So **any** non-`$4016` read — including a generic
  `read(0x0000)` or `read(0x0100)` — deasserts (clears) the line, and the
  previous value is passed as `!prev` (`clock`) to
  `Controller::read_strobed` (`nes_core/src/nes/controller.rs:51-71`):
  `clock == false` repeats the same shift-register stage instead of advancing.
- Any write: `latch_write` (`nes_core/src/nes.rs:66-70`) sets
  `open_bus = value` and clears **both** OE lines. Both the generic `write`
  and the fast-path writes go through it.
- Fast-path zero/stack **writes** (`nes_core/src/nes.rs:331-340`,
  `write_zero_page` / `write_stack_page`): call `latch_write`, so they clear
  both OE lines — same as a generic store. Covered by
  `zero_page_store_clears_joypad_oe_lines` (`nes_core/src/nes/tests.rs:175-213`).
- Fast-path zero/stack **reads** (`nes_core/src/nes.rs:321-329`,
  `read_zero_page` / `read_stack_page`): call only `latch_read`
  (`nes_core/src/nes.rs:54-57`, which sets `open_bus`) and **do not touch
  either OE field**. They therefore preserve a "contiguous" state across a
  zero/stack read. This is the asymmetry under investigation.
- `peek` does not touch the fields on any route (it is not a bus cycle).

Hardware meaning intended: `/OE1` / `/OE2` are the 2A03 controller-port
output-enable pins, asserted only while the CPU reads `$4016` / `$4017`
respectively; on NES-001 (and AV Famicom) that assertion spans a whole
contiguous run of same-register reads, and the controller clocks once per run.
On the RF Famicom it instead clocks every read cycle — the code models only
the NES-001 variant (`doc/dma.md:314-316`, the local copy of the wiki DMA page
`nesdev-wiki/wikipages/DMA.xhtml`).

## 2. Does hardware require an update on zero-page / stack-page access?

Yes for reads; already done for writes. There is no zero/stack exemption.

- `/OE` activation is address-specific: "Reading `$4016` and `$4017` activates
  the `/OE1` and `/OE2` signals, respectively" —
  `nesdev-wiki/wikipages/Input_devices.xhtml` (https://www.nesdev.org/wiki/Input_devices).
  No other address activates them.
- The controller clock line is defined combinationally:
  "The CLK line for controller port is `R/W nand (ADDRESS == $4016/$4017)`
  (i.e., CLK is low only when reading `$4016/$4017`, since R/W high means
  read). When this transitions from high to low, the buffer … latches … and
  when it transitions from low to high, the shift register … shifts one bit."
  — `nesdev-wiki/wikipages/Controller_reading.xhtml`
  (https://www.nesdev.org/wiki/Controller_reading), "Clock timing".
  `nesdev-wiki/wikipages/Standard_controller.xhtml` agrees: CLK "is normally
  held high, and becomes low during a read of `$4016` or `$4017`; when the
  read is finished, it returns high, triggering the shift"
  (https://www.nesdev.org/wiki/Standard_controller).
  Hence **any** bus cycle whose address is not `$4016`/`$4017` (reads
  included), or any write cycle (`R/W` low), drives CLK high / deasserts `/OE`.
- Zero page (`$0000-$00FF`) and stack page (`$0100-$01FF`) are ordinary
  internal RAM (`$0000-$1FFF`, 2 KiB mirrored) —
  `nesdev-wiki/wikipages/CPU_memory_map.xhtml`
  (https://www.nesdev.org/wiki/CPU_memory_map). A zero/stack read is a real
  CPU bus cycle presenting `$00xx` / `$01xx` with `R/W` high, so the NAND
  above deasserts `/OE`. Stack push/pull, zero-page RMW dummy reads, and
  OAM DMA reads from page 0/1 (which in this codebase also go through the
  fast path, `nes_core/src/dma_bus.rs:170-171`) are all such cycles.
- `doc/dma.md:314`: "These output enables remain asserted the entire CPU
  cycle and even across adjacent cycles if they're both reading the same
  joypad register." Contiguity is defined purely by consecutive same-register
  read cycles; an intervening zero/stack read breaks it, exactly like an
  intervening read of any other address.
- Open bus is a separate effect and is **not** the issue: zero/stack reads do
  latch `open_bus` correctly via `latch_read`, matching the wiki's CPU
  open-bus rule that a read returns/latches the last value read
  (`nesdev-wiki/wikipages/Open_bus_behavior.xhtml`,
  https://www.nesdev.org/wiki/Open_bus_behavior). The missing effect is
  only the `/OE` deassertion. (This also contradicts the repo's own
  `GLOSSARY.md:264` "Fast Path Contract", which requires fast and generic
  routes to apply identical observable side effects and "cannot drift" —
  for reads it currently lists only the open-bus latch, omitting `/OE`.)

## 3. Verdict

| Access | Code today | Hardware | Verdict |
|---|---|---|---|
| Generic `read`/`write` to `$0000-$00FF` / `$0100-$01FF` | Clears OE on read (via `replace`) and on write (via `latch_write`) | Must deassert | **Correct as-is** |
| Fast `write_zero_page` / `write_stack_page` | Clears both OE (`latch_write`) | Must deassert | **Correct as-is** |
| Fast `read_zero_page` (`$0000-$00FF`) | Preserves OE | Must deassert | **Bug** |
| Fast `read_stack_page` (`$0100-$01FF`) | Preserves OE | Must deassert | **Bug** |

Failing sequence (both pages): `read(0x4016)` → `read_zero_page(..)` (or
`read_stack_page(..)`) → `read(0x4016)`. Hardware: the middle read deasserts
`/OE`, so the second `$4016` read is a fresh falling edge and clocks the
shift register. Code: the second read sees `prev_joypad1_oe == true` and
repeats the same bit. The generic equivalent (`read(0x0000)` in between,
as used in `nes_core/src/nes/tests.rs:91-99`) behaves correctly, proving
the two routes have drifted.

Practical reachability is low but real: in a normal instruction stream an
opcode fetch (a generic read of PC) usually intervenes between two `$4016`
reads anyway, and the fast paths are also used by OAM DMA page-0/1 reads
(`nes_core/src/dma_bus.rs:170-171`), which are genuine bus cycles. So this
is a latent spec/parity bug rather than a known game-breaking one — but it
is a bug under the repo's own Fast Path Contract and under the cited
hardware definition.

## 4. Exact fix (not applied — research only)

In `nes_core/src/nes.rs`, in the `Mcu` impl for `NesMcu`:

- `read_zero_page` (`nes_core/src/nes.rs:321-324`) and `read_stack_page`
  (`nes_core/src/nes.rs:326-329`): clear both lines before latching, e.g.
  add `self.joypad1_oe = false; self.joypad2_oe = false;` ahead of the
  `latch_read(value)` call in each. (Do not move this into `latch_read`
  itself: the generic `$4016`/`$4017` read sets the line via `replace`
  before dispatch and must keep it set.)
- Optionally update `GLOSSARY.md` ("Fast Path Contract") to state that a
  fast read also deasserts both joypad `/OE` lines, and add tests mirroring
  `zero_page_store_clears_joypad_oe_lines` for the read routes (zero-page
  and stack-page variants, both ports).
