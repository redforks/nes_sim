# Bus cycle classification as one exhaustive match

The CPU's bus-cycle classification was duplicated across two hand-maintained, non-exhaustive views of `Microcode`: `is_write_operation` (a `matches!` list — unlisted variants silently classify as non-write) and `Cpu::dma_halt_bus_addr` (a ~65-arm match ending in `_ => None` — unlisted read variants silently classify as internal). The two already disagreed on nine RMW variants, masked only by the call ordering inside `NesDmaSupport::try_freeze` (`can_pause` before `dma_halt_bus_addr`, an invariant spanning two files with no enforcement), and the `_ => None` arm had produced live latent drifts reachable by a DMC freeze. We decided to collapse both views into one exhaustive match, `Cpu::pending_bus_cycle() -> BusCycle`, where `BusCycle = Read(ReadAddress) | Write | Internal` and `ReadAddress = ProgramCounter | Latch(u16) | Stack(u16) | Fixed(u16)` — every arm that carries an address is already resolved, so the halt-repeat contract ("what bus cycle would hardware drive for the pending cycle") has exactly one home; `can_pause` and `dma_halt_bus_addr` become projections of it, `is_write_operation` is deleted, and the `Write` arm of the address projection is `unreachable!()` so reordering the two DMC calls crashes instead of silently swallowing writes.

## Considered options

- **Classification carried on the variant**: rejected — touches all ~60 `Microcode` variants and every builder, and `exec` remains a second per-variant encoding, so the consistency burden survives the refactor.
- **Classification derived from builder data** (per-position in the opcode table): rejected — consumers query the pending queue-front, which includes dynamically pushed ops (`SkipDetectInterrupt` from taken branches, `PushStatus`/`PushStack` from interrupt sequences, the page-crossed LAS refetch); a positional table can't answer those and collapses into per-variant classification with a parallel structure to keep in sync.
- **Test-only pinning** (keep both matches, add the golden): rejected — the mask-order invariant stays unenforced, and the drift fixes have no home to fix into.

## Drift the match fixes

Under the exact-address rule, applying the contract surfaced more wrong arms than the three the candidate ticket named; all are fixed by the same match:

- `IndexedH` and `IndexedHAndJump` — the (ind) high-byte read increments the low byte without carry; the classifier repeats the adjusted address.
- `LoadNmiPcL/H`, `LoadIrqPcL/H`, `LoadResetPcL/H` — fixed vector reads ($FFFA/B, $FFFE/F, $FFFC/D), previously "internal". `LoadIrqPcL`'s ADR-0006 hijack site may redirect the read inside exec; the declared default cycle is the IRQ vector.
- `IndexedXWithOp`/`IndexedYWithOp` — a read op with `FirstClock` reads the operand at the indexed address when no page is crossed; crossing or `FirstClockAlways` (every store) drives the dummy read at (old high | new low), matching `absolute_indexed_with_op_generic` exactly.
- Stack pops (`Plp`, `PopPcL`, `PopPcH`, `PopStack`) — read at $0100 | (SP + 1), previously "internal".
- `LoadPcAbsoluteH` (the JSR high-byte fetch) and `ZeroPageIndexedX/Y` (the index-add cycle: hardware reads the unindexed address) — previously PC-classified or internal, per the hardware cycle.
- Standalone `Microcode::Las` — queued only as the page-crossed LAS abs,y refetch cycle; classified as the latch read hardware drives there, even though exec performs register math on a stale ALU (pre-existing exec defect, unchanged here — the with-op cycle's read is what normally feeds it).

## Consequences

- The classifier may be richer than `exec`'s bus activity by design: `Microcode::Nop` executes nothing but classifies as a program-counter read (implied ops, stack ops, branch add cycles, the RESET dead cycles). Known per-variant limitation: the NOP-addr modes (0x04/0x0C/0x14/…) end in a final dummy read of the effective address on hardware, but a per-variant classifier cannot see sequence context and repeats PC there.
- `GOLDEN_BUS_CYCLE` walks a stub CPU (pc = 0x1234, ab = 0x56FF so the no-carry increment is observable, sp = 0xFD, x = y = 0) over all 256 opcode sequences and `InterruptSequences::{BRK, NMI, IRQ, RESET}`, pinning every cycle's classification. Invariants: at most two consecutive write cycles per opcode sequence — the RMW double-write (old value, then the modified one) — with BRK's three-push interrupt entry as the sole table exception; fixed reads are exactly the six vector constants; `Nop` pinned as its own case rather than swept under a broad fetch invariant.
- The seam (`NesDmaSupport::try_freeze`/`dma_halt_bus_addr`) is unchanged — two adapters (real CPU, test mock) keep it real; only the classification feeding it unifies.
- Golden-table drift is only legitimate alongside a green `cpu_interrupts_v2` run in the same change (same rule as ADR-0006).
- Vocabulary lives in CONTEXT.md under "CPU Bus Cycles": bus cycle classification, halt-repeat read.
