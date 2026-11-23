# Bounty Fix

**Reward:** $77.00
**Issue:** https://github.com/redforks/nes_sim/issues/200
**Repo:** https://github.com/redforks/nes_sim

---

## Issue

Wayfinder: nescli vs fceux SMB frame divergence (root-cause report)

## Destination

Root-cause report explaining every frame divergence between nescli and fceux 2.6.6 for the 600-frame SMB movie (`Super Mario Bros. (World).nes` + `happylee-supermariobros,warped.fm2`, `--palette scripts/frame-hash/fceux_parity/fceux-default.pal`). No fix committed.

## Notes

- Domain: `GLOSSARY.md`, `docs/adr/`, `nes_core/AGENTS.md`, `nescli/AGENTS.md`.
- Reference emulator: `/usr/bin/fceux` 2.6.6. Source at `/home/forks/nes/fceux` (commit `1e1168d`) for code reading only.
- Frame harness (moved from `/tmp/nesdump` into repo, uncommitted): `scripts/frame-hash/fceux_parity/`. Contains `fceux_dump.lua` (`gui.gdscreenshot(true)`), `bin/ffmpeg` (fake ffmpeg, RGB frame dump via `DUMP_DIR`), `convert_gd.py`, `compare.py`, `align.py`, `fceux-default.pal`. Run fceux with `SDL_VIDEODRIVER=dummy QT_QPA_PLATFORM=offscreen`.
- Instrumentation: tag every probe `[DEBUG-xxxx]`; revert and confirm with `grep` before closing a Task ticket.
- Baseline facts: 158/600 frames identical; nes intermission frames 50–196 vs fceux 51–177; from frame 197 nes = fceux offset 19 for frames 195–216, diverges at 217 (Mario sprite).

## Decisions so far

<!-- one line per closed ticket: [title](link): gist -->
- [Wayfinder: instrument intermission timer and NMI counts](https://github.com/redforks/nes_sim/issues/203): same 21-frame prescaler period in both emulators; nescli first tick at +20 after load vs fceux +1, so every tick is 18 frames later. nescli prescaler shows shifted steps at samples 46–51 (0, 2, 0, 2 decrements), fceux none.
- Grilling [#204](https://github.com/redforks/nes_sim/issues/204), round 1 (user "yes"): main cause is the prescaler phase (every nescli tick 18 frames late); 1-frame earlier load treated as a start-up-offset symptom, not a separate ticket; #205 aligns traces by the measured offset and does not wait for the NMI cause.
- Grilling #204, round 2 (user "yes"): leading hypothesis is that nescli NMI is taken late, crossing the frame boundary on frames 46–51. CPU NMI detector is the first suspect. Checked in a separate task, [#206](https://github.com/redforks/nes_sim/issues/206), not blocking #205.
- #204 closed (user "yes").
- [#206](https://github.com/redforks/nes_sim/issues/206) result (closed, user "yes"): NMI entry is not late. One NMI per frame in both emulators, scanline 241 dot 22–30. The 0/2 `$077F` pattern is a decrement-path issue: nescli decrements at sl 257–259 and at sl 0–14 of the next frame. Mid-frame writes come from the NMI handler's own `DEC $077F` at `$8105`. In nescli ~15% of NMIs run past scanline 261, so the decrement lands in the next sample. That gives the 0/2 pattern. fceux never crosses (591/591 same frame). See [#207](https://github.com/redforks/nes_sim/issues/207) (closed, user "yes").
- [#205](https://github.com/redforks/nes_sim/issues/205) result (closed, user "yes"): the jump lag is state-timed, not input-timed. The movie button stream is identical, with nescli reading it 2 frames earlier in frame index. The press that makes fceux jump at frame 199 lands in nescli at 197, when Mario is absent (`$CE=$00` until 214), so it is ignored. The intermission offset (#203) is the cause.
- [#208](https://github.com/redforks/nes_sim/issues/208) result (closed, user "Go ahead"): NES-time handler length is identical. fceux NMI→`$8105` delta is 15234 dots (nescli 15231) on routine NMIs and 5190 (nescli 5190) on normal ones, measured in a scratch fceux build. The "crossing" is a frame-cut artifact: nescli's `process_frame` returns at sl 261, while fceux's `FCEUPPU_Loop` call starts at sl 240 and runs vblank plus the visible lines, so the DEC lands in the same fceux call. This supersedes the "fceux never crosses" reading in #206. Follow-ups: [#209](https://github.com/redforks/nes_sim/issues/209) (420-dot gaps in the `$8EB6` loop), [#210](https://github.com/redforks/nes_sim/issues/210) (`STA $2007` 12 vs 15 dots).
- [#209](https://github.com/redforks/nes_sim/issues/209) finding (open, pending user confirmation): the 420/423-dot stretches in the `$8EB6` loop are game code, the per-record header path of the PPU-buffer routine (`$8EC1-$8EF3` plus `$8EAC-$8EB5`, 369 or 372 dots). fceux runs the same instructions with the same per-instruction dot costs and record order. The handoff's "420s are the bulk of the extra time" is wrong: loop iterations are about 59% of the routine-minus-normal delta, records about 37%. Details in https://github.com/redforks/nes_sim/issues/209#issuecomment-7092629781
- Frame harness promoted from `/tmp/nesdump` into `scripts/frame-hash/fceux_parity/` (user request, this session; was Out of scope as Q4). Copied, not moved: `/tmp/nesdump` still holds the dumps and the `tools/fceux_parity/` original remains untracked. Not committed.
- [Wayfinder: does frame-cut difference (sl 261 vs sl 240) explain 0/2 $077F and intermission gap](https://github.com/redforks/nes_sim/issues/211) (closed, AFK): frame cut explains the 0/2 `$077F` pattern. Sampling nescli at sl 240 (fceux cut) removes all 0/2 pairs (non-1 steps 131→6, twos 63→0; fceux 8, twos 0). Intermission end gap does not move: load 50 (fceux 51), end 197 (fceux 178), length 147 vs 127, unchanged by the cut. The 20-frame gap is the `$077F` prescaler phase against the load, not the sample boundary. Fix not started.
- [Wayfinder: is 1-frame 00-state difference at intermission end separate from phase cause](https://github.com/redforks/nes_sim/issues/212) (closed, AFK): not a separate cause. The `00` transient at intermission end is seen only by the nescli sl 261 sample. With the sl 240 cut and the 19-frame phase shift, the tick train and end match fceux exactly. Load 1 frame earlier in nescli (50 vs 51) is not phase-explained; unresolved.

- [Wayfinder: is background drift after frame 217 separate cause or Mario timing result](https://github.com/redforks/nes_sim/issues/213) (closed, AFK): not separate. Background pixels match fceux whenever Mario screen X (`$86`) and scroll (`$071C`) match (0 px diff, 40 pairs, x96–255 y40–200). Scroll onset is later in nescli: fceux scrolls from 231 when X reaches `$53`; nescli `$071C` stays 0 and its Mario walks slower (stall at X=`$28` for 215–226). Why the stall is not answered here (see #214).

## Not yet specified

<!-- empty: all four fog items graduated to tickets #211–#214 -->

## Out of scope

- Fixing any divergence: destination is the root-cause report only.





---

## Process

### 1. Orient yourself first
Before touching any code:
- Read the README and any relevant docs
- Run: find . -type f | grep -v node_modules | grep -v .git | grep -v dist | head -60
- Grep for the key terms from the issue title to find the relevant files
- Read the files around the bug — understand what's currently happening

### 2. Find and read the tests
- Look for test files near the relevant code (*.test.ts, *.spec.js, __tests__, etc.)
- Read them — they tell you what behavior is expected
- Understand what a passing fix looks like before writing a line

### 3. Implement the fix
- Make the MINIMAL change that fixes the issue — nothing more
- Do not refactor, rename, reorganize, or clean up unrelated code
- Do not add dependencies unless the issue explicitly requires it
- Do not modify config files (package.json, tsconfig.json, etc.) unless that IS the fix
- One logical change, one commit

### 4. Verify
- Run the test suite: check package.json scripts for "test", "lint", "typecheck"
  Common commands: npm test, yarn test, pnpm test, pytest, go test ./..., cargo test
- If TypeScript is present: npx tsc --noEmit
- Fix any test failures your change caused — if a test was wrong before, update it

### 5. Commit and document
- git add -A
- git commit -m "fix: Wayfinder: nescli vs fceux SMB frame divergence (root-cause report)"
- Write AGENT_SUMMARY.md with:
  - Root cause: what was wrong and why
  - Fix: what you changed and the reasoning
  - Verification: how to confirm it works
  - Concerns: anything a reviewer should know

---

## Stop conditions

If ANY of these are true, write "UNCERTAIN: [reason]" at the top of AGENT_SUMMARY.md, make NO code changes, and stop:
- The fix requires changing more than ~5 files
- You cannot determine what the correct behavior should be
- The issue requires a database migration, API key, or external service
- You find 3+ open PRs already attempting this fix (high competition)
- The repo is not in a language/framework you can reason about confidently

A clean UNCERTAIN is better than a bad PR.
