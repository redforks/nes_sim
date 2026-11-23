# #223: $8EB6 420/423-dot stretches — confirmation of #209, STA $2007 status (#210)

Parent: #217 (spec), part of #200. No behavior change in nescli/nes_core.
All numbers below reproduced from existing traces; no new probe added,
no `[DEBUG-` tag present (`grep -rn "DEBUG-" nes_core nescli` empty).

## 1. Loop/record split reproduced

Source: `/tmp/nesdump/nes208b.txt` (`[DEBUG-208]` PC trace) via
`/tmp/nesdump/209/perframe.py` and `seqcmp.py`.

nescli frames 15–17 (routine NMIs): 111 `$8EB6` hits each =
100 gaps of 54 dots + 7 gaps of 423 + 3 gaps of 420 + one ~79713-dot
main-game gap. fceux NMIs 9–20 show the same multiset (3×420, 7×423,
100×54) in the same cyclic record order; the sequence phase differs only
because the frame cut lands at a different point in the cycle.

Budget per routine NMI (routine 15231 dots entry→`DEC $077F`, normal 5190):

- Span first→last `$8EB6` hit: 100×54 + 7×423 + 3×420 = 9621 dots.
- Plus 51-dot last iteration (BNE not taken): 9672 dots of the
  15231 − 5190 = 10041-dot routine-minus-normal delta.
- Loop iterations: 100×54 + 11×51 = 5961 dots ≈ 59%.
- Record paths: 7×372 + 3×369 = 3711 dots ≈ 37%.
- Residual unattributed: 369 dots (open, same as #209 reported).

## 2. Game-code confirmation (#209)

Confirmed at ROM level (`Super Mario Bros. (World).nes`, NROM-256,
PRG file offset 16 + (addr − 0x8000)):

- `$8EB6`: `B0 01 C8 B1 00 8D 07 20 CA D0 F5` = BCS, INY, LDA ($00),Y,
  STA $2007, DEX, BNE $8EB6. Full iteration 18 cycles = 54 dots;
  last iteration 17 cycles = 51 dots.
- Post-loop record path `$8EC1`–`$8EB5` verified byte-for-byte against
  the #209 decode, including `PHA` ($8E9F), `LDA $0778` ($8EA0),
  `ORA #$04` ($8EA3), `BCS $8EA9` ($8EA5), `AND #$FB` ($8EA7),
  `JSR $8EED`, and `PLA / ASL / BCC $8EB3 / ORA #$02 / INY /
  LSR / LSR / TAX` ($8EAC–$8EB5).
- 420 vs 423: the `BCS` at `$8EA5` tests bit 7 of the record's third
  byte (via the `ASL`). Taken = 420 dots; not taken runs `AND #$FB`
  (+1 cycle) = 423 dots.

No correction to #209. Minor addition: #209 described only the
BCC-taken path at `$8EAE`; the ROM also contains the BCC-not-taken
bytes `ORA #$02 / INY` ($8EB0–$8EB2). Both measured 420/423 gaps match
the decoded costs, so this changes nothing.

Record for #209: confirmation holds; #209 is already closed with the
finding comment.

## 3. STA $2007 status (#210): closed, cause found

Verified from the same trace (consecutive `8EBB→8EBC` events):

- Frames 14–18: all 12 dots (78 + 111×4 readings).
- Frame 45: 17×12 dots + 15×15 dots (32 loop hits).
- Frame 46: 4×12 dots.

The 15-dot readings are the `LDA ($00),Y` page-cross penalty, not a
`STA $2007` cost: nes_core `cpu/microcode.rs` wires
`LDA_INDIRECT_INDEXED_Y` to `CrossPageBehavior::FirstClock`, and
`cpu.rs` `indexed_with_op_cycle` emits the dummy-read bus cycle on a
page cross (54 → 57 dots per iteration). fceux does the same
(`GetIYRD` + `ADDCYC(1)` in `src/x6502.cpp`). The PPU `$2007` write arm
(`nes_core/src/nes/ppu.rs` `write_ppureg`, `0x2007` case) takes no clock
and adds no dots; STA absolute is always 4 cycles = 12 dots.

Status: closed with cause found; #210 is already closed with this
finding comment. Caveat carried over from #210: pointer values ($8CEC,
Y=$14..$22) come from fceux, not from nescli memory.

## Reproduction commands

```sh
cd /tmp/nesdump/209 && python3 perframe.py   # gap histogram per frame
cd /tmp/nesdump/209 && python3 seqcmp.py     # nescli vs fceux gap order
grep -rn "DEBUG-" nes_core nescli            # must print nothing
cargo test --lib --workspace                 # 982 passed, 0 failed
```

Worktree: `/tmp/nes217-223`, branch `ticket/223-8eb6`, base `6e6237f`.
