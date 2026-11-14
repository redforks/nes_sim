/// nes Controller struct that represent a controller of nes.
pub struct AController {
    strobe: bool,
    /// current button state
    bits: u8,
    /// snapshot of button state frozen when strobe fell; drives reads
    /// until the next falling edge
    locked_bits: u8,
    /// the position of the bit that is currently being read
    bit_position: u8,
    /// Joypad /OE line for this pad: true while /OE stays asserted
    /// across contiguous reads of its register on NES-001 hardware,
    /// clocking the shift register once per contiguous set rather than
    /// once per read.
    pub(crate) oe: bool,
}

#[allow(clippy::new_without_default)]
impl AController {
    pub fn new() -> AController {
        AController {
            strobe: false,
            bits: 0,
            locked_bits: 0,
            bit_position: 0,
            oe: false,
        }
    }

    fn reset_for_read(&mut self) {
        self.bit_position = 0;
        self.locked_bits = self.bits;
    }

    /// Hard reset (power cycle): the strobe line drops and the shift
    /// register rewinds. Physically held buttons stay held, so `bits`
    /// is preserved — only the sampled/shifted state returns to power-up.
    pub fn reset_hard(&mut self) {
        self.strobe = false;
        self.bit_position = 0;
        self.locked_bits = self.bits;
        self.oe = false;
    }

    fn peek(&self) -> u8 {
        let pressed = if self.strobe {
            self.bits & Button::A as u8 != 0
        } else if self.bit_position < 8 {
            (self.locked_bits >> self.bit_position) & 1 != 0
        } else {
            true
        };

        if pressed { 0x41 } else { 0x40 }
    }

    /// Read the shift register. `oe` is false on the first read of a
    /// contiguous set: the controller sees one shift per contiguous set
    /// of reads, not one per CPU cycle.
    pub(crate) fn read_strobed(&mut self) -> u8 {
        let clock = !self.oe;
        let pressed = if self.strobe {
            self.bits & Button::A as u8 != 0
        } else if self.bit_position < 8 {
            if clock {
                let r = (self.locked_bits >> self.bit_position) & 1 != 0;
                self.bit_position = self.bit_position.saturating_add(1);
                r
            } else {
                // /OE stayed asserted: the controller keeps driving the
                // same shift-register stage it already output.
                (self.locked_bits >> (self.bit_position - 1)) & 1 != 0
            }
        } else {
            true
        };

        if pressed { 0x41 } else { 0x40 }
    }

    pub fn press(&mut self, btn: Button) {
        self.bits |= btn as u8;
    }

    pub fn release(&mut self, btn: Button) {
        self.bits &= !(btn as u8);
    }
}

/// NES Zapper light gun, controller port 2.
///
/// Reads via $4017: bit 4 = trigger (held), bit 3 = light sense
/// (0 = light detected at the current beam position, 1 = none). Light
/// sensing samples the rendered framebuffer through a small aperture around
/// the aim point, gated by where the electron beam has already drawn this
/// frame (ported from tetanes-core `Zapper::light_sense`).
pub struct Zapper {
    connected: bool,
    x: u16,
    y: u16,
    radius: u16,
    /// Remaining CPU cycles until a pulled trigger reads released.
    trigger_cycles: u32,
}

/// A pulled trigger reads held for ~100 ms after the pull (NTSC CPU clock /
/// 10), regardless of how long the button is physically pressed.
pub(crate) const ZAPPER_TRIGGER_RELEASE_DELAY: u32 = 178_977;

const SCREEN_WIDTH: u16 = 256;
const SCREEN_HEIGHT: u16 = 240;

#[allow(clippy::new_without_default)]
impl Zapper {
    pub fn new() -> Zapper {
        Zapper {
            connected: false,
            x: 0,
            y: 0,
            radius: 3,
            trigger_cycles: 0,
        }
    }

    pub fn set_connected(&mut self, connected: bool) {
        self.connected = connected;
    }

    pub fn aim(&mut self, x: u16, y: u16) {
        self.x = x;
        self.y = y;
    }

    /// Hard reset (power cycle): aborts an in-flight trigger hold. The
    /// aim point and connection are physical, so they are preserved.
    pub fn reset_hard(&mut self) {
        self.trigger_cycles = 0;
    }

    /// Pull the trigger. A no-op while a previous hold is still in flight:
    /// hardware releases on a fixed timer, not when the button comes up.
    pub fn trigger(&mut self) {
        if self.trigger_cycles == 0 {
            self.trigger_cycles = ZAPPER_TRIGGER_RELEASE_DELAY;
        }
    }

    /// Advance one CPU cycle.
    pub fn clock(&mut self) {
        self.trigger_cycles = self.trigger_cycles.saturating_sub(1);
    }

    /// Port-2 bits at the given beam position. `brightness(x, y)` reports
    /// the R+G+B luminance of the rendered pixel there.
    pub fn read(&self, scanline: u16, dot: u16, brightness: impl Fn(u16, u16) -> u32) -> u8 {
        if !self.connected {
            return 0x00;
        }
        let trigger_bit = if self.trigger_cycles > 0 { 0x10 } else { 0x00 };
        trigger_bit | self.light_sense(scanline, dot, brightness)
    }

    fn light_sense(&self, scanline: u16, dot: u16, brightness: impl Fn(u16, u16) -> u32) -> u8 {
        let min_y = self.y.saturating_sub(self.radius);
        let max_y = (self.y + self.radius).min(SCREEN_HEIGHT - 1);
        let min_x = self.x.saturating_sub(self.radius);
        let max_x = (self.x + self.radius).min(SCREEN_WIDTH - 1);
        for y in min_y..=max_y {
            // Phosphor persistence: a row senses only once the beam has
            // passed it, and keeps glowing for ~20 scanlines afterwards.
            if scanline < y || scanline - y > 20 {
                continue;
            }
            for x in min_x..=max_x {
                // On the aim row itself, pixels ahead of the beam are not
                // drawn yet.
                if scanline == y && dot <= x {
                    continue;
                }
                if brightness(x, y) >= 85 {
                    return 0x00; // light detected
                }
            }
        }
        0x08 // no light
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum Button {
    A = 1,
    B = 0x2,
    Select = 0x4,
    Start = 0x8,
    Up = 0x10,
    Down = 0x20,
    Left = 0x40,
    Right = 0x80,
}

pub struct Controller {
    /// The first controller.
    pub a: AController,
    /// The second controller.
    pub b: AController,
    /// The Zapper light gun, read through port 2 ($4017).
    pub zapper: Zapper,
}

#[allow(clippy::new_without_default)]
impl Controller {
    pub fn new() -> Controller {
        Controller {
            a: AController::new(),
            b: AController::new(),
            zapper: Zapper::new(),
        }
    }

    /// Hard reset (power cycle): both pads and the Zapper return to
    /// power-up shift state. Soft reset leaves controllers untouched.
    pub fn reset_hard(&mut self) {
        self.a.reset_hard();
        self.b.reset_hard();
        self.zapper.reset_hard();
    }
}

impl Controller {
    pub(crate) fn peek(&self, address: u16) -> u8 {
        match address {
            0x4016 => self.a.peek(),
            0x4017 => self.b.peek(),
            _ => 0,
        }
    }

    pub(crate) fn write(&mut self, value: u8) {
        let new_strobe = value & 1 != 0;
        // Only a high-to-low transition freezes button state and
        // rewinds the poll; writes that keep strobe low are no-ops.
        if self.a.strobe && !new_strobe {
            self.a.reset_for_read();
            self.b.reset_for_read();
        }
        self.a.strobe = new_strobe;
        self.b.strobe = new_strobe;
    }
}

#[cfg(test)]
mod tests;
