# Interrupt recognition window as microcode-table data

The CPU's interrupt poll/hijack schedule was implicit: the poll point was keyed on queue-length arithmetic (`queue.len() == 1 && InterruptSequences::is_end(queue[0])`), NMI-hijack eligibility came from two sites with different consumption protocols (`push_status`'s bare `take_nmi_pending` vs `load_irq_pcl`'s `take_hijack_pending` with dot-lags 5/7 expressed backwards from the vector fetch), and five of the previous thirty commits re-derived parts of this contract. We decided to declare the recognition window as companion data on `InterruptSequences::{RESET, NMI, IRQ, BRK}` — final-cycle poll suppression plus hijack sites with deadlines in dots from sequence start — and to run every hijack check through one stamped edge-consumption protocol (`consumed_through` recorded at every site; the bare `take_nmi_pending` deleted), with each site's eligibility rule declared as data: the `PushStatus` site consumes only the sampler-latched pending edge (stamping just the consumed edge, so newer un-latched rises stay eligible for the later site), while the vector-decision site lets the newest unconsumed assertion decide. Deadline values are captured from the current blargg-green behavior, and the equivalence bar is observable, not protocol-level: a table-walk test pins each opcode's poll cycles against a golden table, with `cpu_interrupts_v2` as the behavioral gate. The branch poll shift (`request_detect_interrupt` tri-state + `SkipDetectInterrupt`) stays a named runtime mechanism — its cycle depends on runtime data, so a static declaration would be consumed by nothing — pinned by the same golden table.

## Considered options

- **Marker micro-ops** (`Poll`/`HijackCheck` ops inside the sequences): rejected — every queued micro-op consumes exactly one CPU cycle, so zero-cost markers either add a spurious cycle or need same-tick executor fusion, which is hidden scheduling again.
- **Enriched op payloads only** (`decision_lag` on `PushStatus`, arithmetic otherwise kept): rejected — deadlines expressed backwards from the vector fetch still shift silently when an op is inserted.
- **Test-only pinning** (no data change): rejected — the contract stays implicit, and Candidate 4's line-level handshake would inherit the fog.

## Consequences

- Candidate 4 (line-level interrupt handshake between PPU, Tick, CPU) integrates with the unified edge-consumption protocol instead of redesigning it.
- A hijack arms the NMI window for its continuation tail — the tail is the NMI sequence's final cycles, so its final-cycle poll is suppressed by declaration.
- Golden-table drift is only legitimate alongside a green `cpu_interrupts_v2` run in the same change.
- Vocabulary lives in CONTEXT.md under "CPU Interrupt Recognition": interrupt recognition window, poll point, hijack deadline, edge consumption.
