//! MMC5 dedicated audio unit (ticket #83, spec #74 audio paragraphs).
//!
//! Two pulse channels without sweep plus the full PCM channel, fed by the
//! mapper's `$5000-$5015` register writes and ticked once per CPU cycle
//! alongside the APU core. Behavior follows the NESdev MMC5-audio page:
//!
//! * Pulses match the APU pulse channels except: `$5001/$5005` have no
//!   effect (no sweep unit), timer values below 8 do **not** silence the
//!   channel (ultrasonic frequencies play), and envelope + length counters
//!   both run at a fixed 240 Hz — the length clock at twice the APU rate.
//! * PCM (`$5010` mode/IRQ, `$5011` 8-bit DAC): in write mode the CPU
//!   writes samples to `$5011`; in read mode any `$8000-$BFFF` read feeds
//!   its byte to the DAC ("write-by-read"). Assigning `$00` leaves the DAC
//!   unchanged and trips the IRQ instead, so a `$00`-terminated ROM stream
//!   ends itself. `$5010` bit 7 enables the IRQ; reading `$5010` reports
//!   `trip AND enable` in bit 7 and acknowledges (clears the trip).
//! * `$5015` mirrors the APU status register for the two pulse enables
//!   (write) and their length-counter status (read).
//!
//! Out of scope (spec #74): MMC5A extras and the `$5800-$5BFF` debug meter.

use crate::nes::apu::ExpansionMix;
use crate::nes::apu::helper::{Divider, Envelope, LengthControl, Sequencer};
use crate::nes::apu::registers::{LengthTimerHigh3Bits, PulseControlBits};

/// NTSC CPU clock in Hz: the audio unit ticks once per CPU cycle.
const CPU_CLOCK_HZ: u32 = 1_789_773;
/// MMC5 envelope/length rate in Hz: fixed 240 Hz, no frame sequencer.
const FRAME_HZ: u32 = 240;

/// APU duty patterns, shared with the MMC5 pulses (the reversed analog
/// polarity is removed by the mixer's DC filter, so the digital patterns
/// match). Same table as `apu::pulse::PULSE_DUTY_TABLE`.
const PULSE_DUTY_TABLE: [[u8; 8]; 4] = [
    [0, 1, 0, 0, 0, 0, 0, 0],
    [0, 1, 1, 0, 0, 0, 0, 0],
    [0, 1, 1, 1, 1, 0, 0, 0],
    [1, 0, 0, 1, 1, 1, 1, 1],
];

/// One MMC5 pulse channel: an APU pulse without the sweep unit and without
/// the sub-8 period silence rule.
#[derive(Debug)]
pub struct Mmc5Pulse {
    timer: Divider<u16>,
    divider_2: Divider<u8>,
    length: LengthControl,
    envelope: Envelope,
    sequencer: Sequencer<u8>,
}

impl Mmc5Pulse {
    pub fn new() -> Self {
        Self {
            timer: Divider::new(u16::MAX),
            divider_2: Divider::new(1),
            length: LengthControl::default(),
            envelope: Envelope::default(),
            sequencer: Sequencer::new(&PULSE_DUTY_TABLE[0]),
        }
    }

    pub fn set_enabled(&mut self, enabled: bool) {
        self.length.set_enabled(enabled);
    }

    /// `$5000/$5004`: duty, length-halt/loop, envelope mode/volume.
    pub fn write_control(&mut self, value: u8) {
        let bits = PulseControlBits::from_bytes([value]);
        self.envelope.config(value);
        self.length.set_halt(bits);
        self.sequencer
            .replace_items(&PULSE_DUTY_TABLE[bits.duty() as usize]);
    }

    /// `$5002/$5006`: timer low byte.
    pub fn write_timer_low(&mut self, value: u8) {
        self.timer.set_period_low(value);
    }

    /// `$5003/$5007`: timer high 3 bits + length load; restarts the
    /// envelope and the duty phase like the APU counterpart.
    pub fn write_timer_high(&mut self, value: u8) {
        let bits = LengthTimerHigh3Bits::from_bytes([value]);
        self.timer.set_period_high(bits.high3());
        self.length.load(bits);
        self.envelope.request_reset();
        self.sequencer.reset();
    }

    /// Advance the timer one CPU cycle (same divide-by-2 cadence as APU).
    pub fn tick_timer(&mut self) {
        if self.timer.tick() && self.divider_2.tick() {
            self.sequencer.tick();
        }
    }

    /// Fixed-240 Hz frame tick: envelope **and** length together, so the
    /// length counter runs at twice the APU length rate.
    pub fn tick_frame(&mut self) {
        self.envelope.tick();
        self.length.tick();
    }

    pub fn output(&self) -> u8 {
        // No sweep mute and no sub-8 silence: only the duty step and the
        // length counter gate the output.
        let gate = (self.sequencer.output() != 0) && !self.length.is_zero();
        self.envelope.output() * gate as u8
    }

    /// Length-counter status for `$5015` reads.
    pub fn length_active(&self) -> bool {
        !self.length.is_zero()
    }
}

impl Default for Mmc5Pulse {
    fn default() -> Self {
        Self::new()
    }
}

/// MMC5 PCM channel: `$5010` mode/IRQ register plus the `$5011` 8-bit DAC.
#[derive(Debug)]
pub struct Mmc5Pcm {
    /// `$5010` bit 0: false = write mode (`$5011` DAC), true = read mode
    /// (`$8000-$BFFF` reads feed the DAC). Power-on default is read mode.
    read_mode: bool,
    /// `$5010` bit 7 (write): IRQ enable.
    irq_enabled: bool,
    /// Latched by a `$00` DAC assignment, cleared by a nonzero one or by
    /// reading `$5010`. The cart IRQ line is `trip AND enable`.
    irq_trip: bool,
    /// `$5011` DAC latch (8-bit; writes of `$00` never land here).
    dac: u8,
}

impl Mmc5Pcm {
    pub fn new() -> Self {
        Self {
            read_mode: true,
            irq_enabled: false,
            irq_trip: false,
            dac: 0,
        }
    }

    pub fn read_mode(&self) -> bool {
        self.read_mode
    }

    /// `$5010` write: bit 7 = IRQ enable, bit 0 = read/write mode select.
    /// The trip latch is untouched (a nonzero DAC write clears it instead).
    pub fn write_mode(&mut self, value: u8) {
        self.irq_enabled = value & 0x80 != 0;
        self.read_mode = value & 0x01 != 0;
    }

    /// `$5010` read: bit 7 = `trip AND enable`, bit 0 = mode readback.
    /// Acknowledges (clears) the trip.
    pub fn read_mode_status(&mut self) -> u8 {
        let status = self.peek_mode_status();
        self.irq_trip = false;
        status
    }

    /// Side-effect-free `$5010` read (debugger peek).
    pub fn peek_mode_status(&self) -> u8 {
        ((self.irq_trip && self.irq_enabled) as u8) << 7 | self.read_mode as u8
    }

    /// `$5011` CPU write: ignored entirely in read mode, otherwise a DAC
    /// assignment (`$00` trips the IRQ instead of landing).
    pub fn write_dac(&mut self, value: u8) {
        if !self.read_mode {
            self.assign_dac(value);
        }
    }

    /// Stream "write-by-read": a `$8000-$BFFF` read in read mode assigns
    /// its byte to the DAC (`$00` trips the IRQ instead).
    pub fn feed_read(&mut self, value: u8) {
        self.assign_dac(value);
    }

    fn assign_dac(&mut self, value: u8) {
        if value == 0 {
            self.irq_trip = true;
        } else {
            self.dac = value;
            self.irq_trip = false;
        }
    }

    pub fn output(&self) -> u8 {
        self.dac
    }

    pub fn irq_pending(&self) -> bool {
        self.irq_trip && self.irq_enabled
    }
}

impl Default for Mmc5Pcm {
    fn default() -> Self {
        Self::new()
    }
}

/// The dedicated MMC5 audio unit: both pulses plus PCM, fed by mapper
/// register writes and ticked once per CPU cycle.
#[derive(Debug, Default)]
pub struct Mmc5Audio {
    pulse1: Mmc5Pulse,
    pulse2: Mmc5Pulse,
    pcm: Mmc5Pcm,
    /// Fractional accumulator for the fixed 240 Hz envelope/length rate:
    /// `+= 240` per CPU cycle, one frame tick per `1_789_773`.
    frame_acc: u32,
}

impl Mmc5Audio {
    pub fn new() -> Self {
        Self::default()
    }

    /// Route a `$5000-$5015` mapper register write to the audio unit.
    /// `$5001/$5005` (no sweep unit) and the unused `$5008-$500F` /
    /// `$5012-$5014` slots are accepted and ignored.
    pub fn write(&mut self, address: u16, value: u8) {
        match address {
            0x5000 => self.pulse1.write_control(value),
            0x5002 => self.pulse1.write_timer_low(value),
            0x5003 => self.pulse1.write_timer_high(value),
            0x5004 => self.pulse2.write_control(value),
            0x5006 => self.pulse2.write_timer_low(value),
            0x5007 => self.pulse2.write_timer_high(value),
            0x5010 => self.pcm.write_mode(value),
            0x5011 => self.pcm.write_dac(value),
            0x5015 => {
                self.pulse1.set_enabled(value & 0x01 != 0);
                self.pulse2.set_enabled(value & 0x02 != 0);
            }
            _ => {}
        }
    }

    /// `$5010` (PCM mode/IRQ, acknowledges the IRQ) and `$5015` (pulse
    /// length status) reads; `None` for every other address (open bus).
    pub fn read(&mut self, address: u16) -> Option<u8> {
        match address {
            0x5010 => Some(self.pcm.read_mode_status()),
            0x5015 => Some(self.status()),
            _ => None,
        }
    }

    /// Side-effect-free `$5010`/`$5015` read (debugger peek: no IRQ ack).
    pub fn peek(&self, address: u16) -> Option<u8> {
        match address {
            0x5010 => Some(self.pcm.peek_mode_status()),
            0x5015 => Some(self.status()),
            _ => None,
        }
    }

    fn status(&self) -> u8 {
        (self.pulse1.length_active() as u8) | ((self.pulse2.length_active() as u8) << 1)
    }

    /// Advance one 240 Hz frame tick directly (envelope + length on both
    /// pulses). The production path reaches this through `tick`'s
    /// accumulator; test-only exact-rate seam.
    #[cfg(test)]
    pub fn tick_frame(&mut self) {
        self.pulse1.tick_frame();
        self.pulse2.tick_frame();
    }

    /// Advance one CPU cycle: both pulse timers plus the shared 240 Hz
    /// envelope/length accumulator.
    pub fn tick(&mut self) {
        self.pulse1.tick_timer();
        self.pulse2.tick_timer();
        self.frame_acc = self.frame_acc.wrapping_add(FRAME_HZ);
        while self.frame_acc >= CPU_CLOCK_HZ {
            self.frame_acc -= CPU_CLOCK_HZ;
            self.pulse1.tick_frame();
            self.pulse2.tick_frame();
        }
    }

    /// Report a `$8000-$FFFF` byte read for PCM stream read mode: only
    /// `$8000-$BFFF` reads while read mode is selected feed the DAC.
    pub fn notify_prg_read(&mut self, address: u16, value: u8) {
        if self.pcm.read_mode() && (0x8000..=0xbfff).contains(&address) {
            self.pcm.feed_read(value);
        }
    }

    /// Current channel levels for the APU mixer.
    pub fn expansion_mix(&self) -> ExpansionMix {
        ExpansionMix {
            pulse1: self.pulse1.output(),
            pulse2: self.pulse2.output(),
            pcm: self.pcm.output(),
        }
    }

    /// PCM cart IRQ line: `trip AND enable`.
    pub fn irq_pending(&self) -> bool {
        self.pcm.irq_pending()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Pulse 1 with duty 2 (50 %), constant volume 15, shortest timer:
    /// `$5000 = 10_11_1111`, `$5002 = 0`, `$5003 = length idx << 3`.
    /// Returns the audio unit plus the timer period in CPU cycles per step.
    fn loud_pulse(period: u16, length_idx: u8) -> (Mmc5Audio, u32) {
        let mut audio = Mmc5Audio::new();
        audio.write(0x5015, 0x01);
        audio.write(0x5000, 0xbf);
        audio.write(0x5002, (period & 0xff) as u8);
        audio.write(0x5003, (length_idx << 3) | ((period >> 8) as u8 & 0x07));
        (audio, 2 * (period as u32 + 1))
    }

    /// Run a free-running pulse onto a high duty step: like the APU, the
    /// timer counter is not reloaded by register writes, so a fresh unit
    /// needs up to ~2*65536 ticks for the first wrap; steady state after.
    fn run_to_high(audio: &mut Mmc5Audio) {
        let mut waited = 0;
        while audio.expansion_mix().pulse1 != 15 {
            audio.tick();
            waited += 1;
            assert!(waited < 400_000, "pulse never sounded");
        }
    }

    /// Eight consecutive duty steps must trace a rotation of the 50 % duty
    /// pattern, proving the 2*(period+1) step cadence from an unknown phase.
    fn assert_duty2_cycles(audio: &mut Mmc5Audio, step: u32) {
        let mut seen = [0u8; 8];
        seen[0] = audio.expansion_mix().pulse1;
        for slot in seen.iter_mut().skip(1) {
            for _ in 0..step {
                audio.tick();
            }
            *slot = audio.expansion_mix().pulse1;
        }
        let pattern = [0, 15, 15, 15, 15, 0, 0, 0];
        assert!(
            (0..8).any(|r| (0..8).all(|i| seen[i] == pattern[(i + r) % 8])),
            "unexpected duty trace: {seen:?}"
        );
    }

    // Both pulse channels render correct pitch (spec #74 story 23, ticket
    // #83): the duty sequencer advances once per 2*(period+1) CPU cycles,
    // tracing the 50 % pattern [0,1,1,1,1,0,0,0].
    #[test]
    fn pulse_pitch_follows_timer_period() {
        let (mut audio, step) = loud_pulse(8, 1);
        run_to_high(&mut audio);
        assert_duty2_cycles(&mut audio, step);
    }

    // Timer values below 8 do NOT silence MMC5 pulses (unlike the APU):
    // period 0 still steps the sequencer every 2 cycles and sounds.
    #[test]
    fn pulse_period_below_8_still_sounds() {
        let (mut audio, _) = loud_pulse(0, 1);
        run_to_high(&mut audio);
        assert_duty2_cycles(&mut audio, 2);
    }

    // `$5001/$5005` have no effect: no sweep unit exists, so writing them
    // never changes the period or the output.
    #[test]
    fn sweep_registers_have_no_effect() {
        let (mut audio, step) = loud_pulse(8, 1);
        let (mut fresh, _) = loud_pulse(8, 1);
        audio.write(0x5001, 0xff);
        audio.write(0x5005, 0x88);
        // Lockstep from power-on (identical tick counts, identical phase):
        // the sweep writes must not disturb the trace at any point,
        // including through the initial timer transient.
        for _ in 0..step * 8 + 400_000 {
            assert_eq!(audio.expansion_mix().pulse1, fresh.expansion_mix().pulse1);
            audio.tick();
            fresh.tick();
        }
        // ... and the survivor is tracing the correct duty cycle.
        run_to_high(&mut audio);
        assert_duty2_cycles(&mut audio, step);
    }

    // Envelope (spec #74 story 23, ticket #83): duty 3 starts on a HIGH
    // step, so the envelope level is observable with zero timer ticks —
    // no sequencer phase or frame-accumulator remainder involved.
    #[test]
    fn pulse_envelope_constant_decay_loop_and_clamp() {
        // Constant volume 7 holds across frame ticks.
        let mut audio = Mmc5Audio::new();
        audio.write(0x5015, 0x01);
        audio.write(0x5000, 0xd7);
        audio.write(0x5002, 0x00);
        audio.write(0x5003, 0x08);
        assert_eq!(audio.expansion_mix().pulse1, 7);
        for _ in 0..10 {
            audio.tick_frame();
        }
        assert_eq!(audio.expansion_mix().pulse1, 7);

        // Decay mode (bit 4 clear), envelope period 0: 15 down one per
        // tick. The restart lands on the next frame tick.
        audio.write(0x5000, 0xc0);
        audio.write(0x5003, 0x08);
        audio.tick_frame();
        assert_eq!(audio.expansion_mix().pulse1, 15);
        audio.tick_frame();
        audio.tick_frame();
        audio.tick_frame();
        assert_eq!(audio.expansion_mix().pulse1, 12);
        // Length (idx 1 = 254, halt clear) is still loaded.
        assert_eq!(audio.read(0x5015), Some(0x01));

        // Loop flag wraps 0 back to 15 (halt bit doubles as loop).
        audio.write(0x5000, 0xe0);
        audio.write(0x5003, 0x08);
        // One landing tick (restart -> 15) plus 15 decrements to 0.
        for _ in 0..16 {
            audio.tick_frame();
        }
        assert_eq!(audio.expansion_mix().pulse1, 0);
        audio.tick_frame();
        assert_eq!(audio.expansion_mix().pulse1, 15);

        // Without loop the envelope clamps at 0 while length stays active.
        audio.write(0x5000, 0xc0);
        audio.write(0x5003, 0x08);
        for _ in 0..25 {
            audio.tick_frame();
        }
        assert_eq!(audio.expansion_mix().pulse1, 0);
        assert_eq!(audio.read(0x5015), Some(0x01));
    }

    // Length: `$5003` loads the counter from the shared length table and
    // each 240 Hz frame tick decrements it — envelope AND length together,
    // so the length clock runs at twice the APU length rate by
    // construction. Length table index 1 = 254 frames.
    #[test]
    fn pulse_length_clocks_each_frame_tick() {
        let mut audio = Mmc5Audio::new();
        audio.write(0x5015, 0x01);
        audio.write(0x5000, 0x9f);
        audio.write(0x5002, 0x00);
        audio.write(0x5003, 0x08);
        assert_eq!(audio.read(0x5015), Some(0x01));
        for _ in 0..253 {
            audio.tick_frame();
        }
        assert_eq!(audio.read(0x5015), Some(0x01));
        audio.tick_frame();
        assert_eq!(audio.read(0x5015), Some(0x00));
    }

    // The shared frame accumulator ticks envelope+length at exactly
    // 240 Hz: from a fresh unit, one second of CPU cycles (1_789_773)
    // delivers exactly 240 frame ticks, then 14 more frames exhaust a
    // 254-count length load. Deterministic: the accumulator starts at 0.
    #[test]
    fn frame_cadence_is_240hz() {
        let mut audio = Mmc5Audio::new();
        audio.write(0x5015, 0x01);
        audio.write(0x5000, 0x9f);
        audio.write(0x5002, 0x00);
        audio.write(0x5003, 0x08);
        for _ in 0..1_789_773 {
            audio.tick();
        }
        // 254 - 240 = 14 counts left: still active.
        assert_eq!(audio.read(0x5015), Some(0x01));
        for _ in 0..14 * 7458 {
            audio.tick();
        }
        assert_eq!(audio.read(0x5015), Some(0x00));
    }

    // `$5015` write enables gate the length load: a disabled channel loads
    // nothing and reads back clear.
    #[test]
    fn pulse_enable_gates_length_load() {
        let mut audio = Mmc5Audio::new();
        audio.write(0x5000, 0xbf);
        audio.write(0x5002, 0x00);
        audio.write(0x5003, 0x08);
        assert_eq!(audio.read(0x5015), Some(0x00));
        audio.write(0x5015, 0x01);
        audio.write(0x5003, 0x08);
        assert_eq!(audio.read(0x5015), Some(0x01));
        // Disabling silences immediately.
        audio.write(0x5015, 0x00);
        assert_eq!(audio.read(0x5015), Some(0x00));
        assert_eq!(audio.expansion_mix().pulse1, 0);
    }

    // Second pulse channel (`$5004-$5007`) behaves identically to the
    // first (duty 3 reads the envelope with zero timer ticks).
    #[test]
    fn pulse2_mirrors_pulse1() {
        let mut audio = Mmc5Audio::new();
        audio.write(0x5015, 0x02);
        audio.write(0x5004, 0xdf);
        audio.write(0x5006, 0x00);
        audio.write(0x5007, 0x08);
        assert_eq!(audio.expansion_mix().pulse2, 15);
        assert_eq!(audio.read(0x5015), Some(0x02));
    }

    // PCM write mode: `$5011` sets the 8-bit DAC; `$00` leaves the DAC
    // alone and trips the IRQ instead.
    #[test]
    fn pcm_dac_write_mode_and_zero_trips_irq() {
        let mut audio = Mmc5Audio::new();
        // Write mode with the IRQ enabled so the trip is observable.
        audio.write(0x5010, 0x80);
        // No trip latched yet: bit 7 reads clear even though enabled.
        assert_eq!(audio.peek(0x5010), Some(0x00));

        audio.write(0x5011, 0xab);
        assert_eq!(audio.expansion_mix().pcm, 0xab);
        assert!(!audio.irq_pending());

        audio.write(0x5011, 0x00);
        assert_eq!(audio.expansion_mix().pcm, 0xab);
        assert!(audio.irq_pending());
    }

    // PCM IRQ: `$5010` bit 7 enables; a nonzero DAC assignment clears a
    // pending trip; reading `$5010` reports `trip AND enable` and acks.
    #[test]
    fn pcm_irq_enable_and_ack() {
        let mut audio = Mmc5Audio::new();
        audio.write(0x5010, 0x80);
        audio.write(0x5011, 0x00);
        assert!(audio.irq_pending());

        // Reading reports the trip and acknowledges it.
        assert_eq!(audio.read(0x5010), Some(0x80));
        assert!(!audio.irq_pending());
        assert_eq!(audio.read(0x5010), Some(0x00));

        // A nonzero DAC write also clears a pending trip.
        audio.write(0x5011, 0x00);
        assert!(audio.irq_pending());
        audio.write(0x5011, 0x55);
        assert!(!audio.irq_pending());

        // Disabled IRQ never shows, even tripped; peek does not ack, so
        // re-enabling reveals the still-latched trip.
        audio.write(0x5010, 0x00);
        audio.write(0x5011, 0x00);
        assert!(!audio.irq_pending());
        assert_eq!(audio.peek(0x5010), Some(0x00));
        assert!(!audio.irq_pending());
        audio.write(0x5010, 0x80);
        assert!(audio.irq_pending());
        assert_eq!(audio.peek(0x5010), Some(0x80));
        assert!(audio.irq_pending());
        // ... while a real read acknowledges it.
        assert_eq!(audio.read(0x5010), Some(0x80));
        assert!(!audio.irq_pending());
    }

    // PCM read (stream) mode: `$5011` writes are ignored; `$8000-$BFFF`
    // reads feed the DAC, `$00` tripping the IRQ; other windows never feed.
    #[test]
    fn pcm_stream_read_mode() {
        let mut audio = Mmc5Audio::new();
        audio.write(0x5010, 0x81);
        assert_eq!(audio.peek(0x5010), Some(0x01));

        audio.write(0x5011, 0x77);
        assert_eq!(audio.expansion_mix().pcm, 0x00);

        audio.notify_prg_read(0x8000, 0x77);
        audio.notify_prg_read(0xbfff, 0x34);
        assert_eq!(audio.expansion_mix().pcm, 0x34);
        assert!(!audio.irq_pending());

        // Outside `$8000-$BFFF` (vectors, RAM windows) never feeds.
        audio.notify_prg_read(0xc000, 0x99);
        audio.notify_prg_read(0x7fff, 0x99);
        assert_eq!(audio.expansion_mix().pcm, 0x34);

        // A `$00` byte trips the IRQ instead of landing in the DAC.
        audio.notify_prg_read(0x8123, 0x00);
        assert_eq!(audio.expansion_mix().pcm, 0x34);
        assert!(audio.irq_pending());
        assert_eq!(audio.read(0x5010), Some(0x81));
    }

    // Power-on state matches the MMC5A defaults: read mode (`$01`), DAC
    // silent, no IRQ, pulses disabled with clear length status.
    #[test]
    fn power_on_state() {
        let mut audio = Mmc5Audio::new();
        assert_eq!(audio.read(0x5010), Some(0x01));
        assert_eq!(audio.read(0x5015), Some(0x00));
        assert_eq!(
            audio.expansion_mix(),
            ExpansionMix {
                pulse1: 0,
                pulse2: 0,
                pcm: 0,
            }
        );
        assert!(!audio.irq_pending());
        // Unknown audio-window addresses read as open bus (`None`).
        assert_eq!(audio.read(0x5001), None);
        assert_eq!(audio.read(0x5012), None);
        assert_eq!(audio.peek(0x5009), None);
    }
}
