# VRC4 IRQ enable/acknowledge model

**Consequences:**
A pending IRQ with interrupts disabled is unreachable, because every disable path acknowledges on the way down; an acknowledge leaves a half-elapsed scanline interval frozen rather than restarted, so re-enabling resumes the previous phase; and back-to-back acknowledges are idempotent because 'A' persists until the next Control write. Two wiki readings are observationally equivalent through the register/tick surface and are settled by convention rather than test evidence: reload-only-when-'E'-set and prescaler-reset-on-every-write versus their 'E'-conditioned alternatives differ only while disabled, when nothing can observe the counter or phase.

**Rejected alternatives:**
A literal up-counter `L..=FF` (observably identical, larger diff over verified behavior); clearing 'A' on acknowledge (the "move" reading of the wiki — indistinguishable except by ack-ack sequences, trivially switchable if a ROM ever cares); exact 114/114/113 prescaler phasing (deferred — see the hardware-accuracy map's "Residual VRC prescaler phasing" item; the uniform 341-dot accumulator is the wiki's own recommended approximation).

Source: nesdev wiki dump `../nesdevwiki.zip:wikipages/VRC_IRQ.xhtml` (rev 19514). Decision provenance: issue #2 on the Hardware-accuracy fix decisions map.
