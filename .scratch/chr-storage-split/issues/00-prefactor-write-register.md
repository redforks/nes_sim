Status: ready-for-agent

# P0: Add `write_register` to `ChrStorage` trait

## Parent

PRD: `.scratch/chr-storage-split/PRD.md`

## What to build

Add a `write_register(&mut self, addr: u16, value: u8)` method to the existing `ChrStorage` trait with a default no-op implementation (`{}`). This is a purely additive change — no existing code uses the new method, and no existing code needs to change. Both existing implementations (`DirectChr`, `WindowedChr`) inherit the default.

The new method is a forward-looking addition. In subsequent slices, mapper-specific ChrStorage implementations will override it to decode CPU bus writes and update their internal banking state.

## Acceptance criteria

- [ ] `ChrStorage` trait has `write_register(&mut self, addr: u16, value: u8)` with default `{}`
- [ ] `DirectChr` and `WindowedChr` compile with the new trait method (inheriting the default)
- [ ] All existing tests pass without modification (`cargo test`)

## Blocked by

None — can start immediately.
