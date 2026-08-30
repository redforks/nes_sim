# Exec drives bus-cycle classification (revises ADR-0007)

ADR-0007 collapsed two hand-maintained, non-exhaustive `Microcode` classifiers (`is_write_operation` and `Cpu::dma_halt_bus_addr`) into one exhaustive `pending_bus_cycle() -> BusCycle` match. The classifier still lived beside `Microcode::exec` — a second per-variant encoding that had to stay in sync. Nine RMW variants already disagreed, and the `_ => None` arm had produced latent DMC-halt drifts. The golden table pinned the drift, but the two encodings could still diverge.

This ADR makes `exec` the single source of truth: every `Microcode::exec` drives its own bus access. `Microcode::Nop` drives a `read_pc_byte()` (the hardware dummy fetch of implied ops, JSR cycle 3, interrupt/RESET dead cycles, branch cycle 3); `ZeroPageIndexedX/Y` drives `read_zero_page(low)` before the index add; `Las` drives `load_alu()` before the `A=X=S=M&S` defect; dedicated `DummyReadAt(u16)` drives the branch page-cross fixup `(old PCH | new PBL)` and `DummyStackRead` drives `$0100|SP` (no SP change) for `RTS`/`RTI`/`PLP` dead stack reads; `OpAfterAddressing::Nop` becomes `LoadIntoAlu(Mem)` so `NOP abs`/`zp` read their operands. `BusCycle` is then derived from the driven access (`Read(Latch)`, `Read(Stack)`, `Write`, `Internal`) instead of a parallel classifier.

## Considered options

- **Keep ADR-0007's parallel classifier**: leaves `exec` and `pending_bus_cycle` as two encodings to keep in sync; the NOP dummy-read and branch-fixup address mismatches remain latent.
- **Per-position builder data**: collapses on dynamically pushed ops (`SkipDetectInterrupt`, interrupt sequences) which have no table position.
- **This ADR (exec drives)**: `bus_cycle` is a pure view of `exec`'s driven access; the hardware dummy reads that the old classifier missed (ZP, branch, stack) are now driven and classified.

## Decision

`Microcode::exec` drives every classified bus cycle; `Cpu::bus_cycle()` matches on `Microcode` to return the driven `BusCycle`. New variants `DummyReadAt`/`DummyStackRead` exist only to make the driven address explicit. `GOLDEN_BUS_CYCLE` updated for `0x04/0x44/0x64 -> [PC,AB]`, `0x0C -> [PC,PC,AB]`, `0x14/… -> [PC,AB,AB]`, `0x28 -> [PC,STK_DUMMY,STK]`, `0x40 -> [PC,STK_DUMMY,STK,STK,STK]`, `0x60 -> [PC,STK_DUMMY,STK,STK,I]`. `Nop` split: `Microcode::Nop` is PC-only; stack/branch fixup have dedicated variants.

## Consequences

- `Las` defect fixed, `Nop` operand reads fixed, ZP dummy reads fixed, branch fixup and RTS/RTI/PLP stack dummies fixed; `Pla`/`Plp` pull-address resequencing left as a one-cycle-early stack read (both stack RAM, side-effect-free) to avoid `irq_inhibit` risk.
- `bus_cycle` can no longer drift from `exec`; DMC halt decisions consume the driven classification.
- Vocabulary lives in CONTEXT.md under "CPU Bus Cycles": bus cycle classification (exec drives it).
