use crate::mcu::Mcu;
use std::cell::Cell;

/// APU (Audio Processing Unit) state and frame counter logic.
///
/// Extracted from `NesMcu` so `Machine` can tick it directly instead of
/// delegating through the `Mcu` trait.
pub struct NesApu {
    pub(crate) frame_counter_interrupt: Cell<bool>,
    // APU state for length counter timing
    apu_cycle: u64,
    apu_even_cycle: bool, // APU is clocked every OTHER CPU cycle
    frame_counter: u8,
    frame_counter_mode: bool,
    length_counters: [u8; 4], // pulse1, pulse2, triangle, noise
    length_counter_halt: [bool; 4],
    channel_enabled: [bool; 4],
}

impl Default for NesApu {
    fn default() -> Self {
        NesApu {
            frame_counter_interrupt: Cell::new(false),
            apu_cycle: 0,
            apu_even_cycle: false,
            frame_counter: 0,
            frame_counter_mode: false,
            length_counters: [0; 4],
            length_counter_halt: [false; 4],
            channel_enabled: [false; 4],
        }
    }
}

impl NesApu {
    // Length counter lookup table (index is high 3 bits of length counter load)
    const LENGTH_TABLE: [u8; 32] = [
        10, 254, 20, 2, 40, 4, 80, 6, 160, 8, 60, 10, 14, 12, 26, 14, 12, 16, 24, 18, 48, 20, 96,
        22, 192, 24, 72, 26, 16, 28, 32, 30,
    ];

    /// Tick the APU frame counter and length counters.
    /// The APU is clocked every OTHER CPU cycle.
    /// Returns true if a frame interrupt should occur.
    pub fn tick(&mut self) -> bool {
        // Toggle the even/odd cycle flag
        self.apu_even_cycle = !self.apu_even_cycle;

        // Only process the APU frame counter on even cycles (every other CPU cycle)
        if !self.apu_even_cycle {
            return false;
        }

        self.apu_cycle += 1;

        let cycle_mod = self.apu_cycle % 14915;
        let is_clock_point = cycle_mod == 3728 || cycle_mod == 7456 || cycle_mod == 11185;

        let step = if cycle_mod < 3728 {
            0
        } else if cycle_mod < 7456 {
            1
        } else if cycle_mod < 11185 {
            2
        } else {
            3
        };

        if is_clock_point {
            self.frame_counter = step;

            let should_clock_length = if self.frame_counter_mode {
                true
            } else {
                step != 3
            };

            if should_clock_length {
                for i in 0..4 {
                    if self.channel_enabled[i]
                        && !self.length_counter_halt[i]
                        && self.length_counters[i] > 0
                    {
                        self.length_counters[i] -= 1;
                    }
                }
            }

            if self.frame_counter == 3 && !self.frame_counter_mode {
                self.frame_counter_interrupt.set(true);
                return true;
            }
        }

        false
    }

    pub fn request_irq(&self) -> bool {
        self.frame_counter_interrupt.get()
    }
}

impl Mcu for NesApu {
    fn read(&self, address: u16) -> u8 {
        match address {
            0x4015 => {
                // APU status register - return length counter status
                self.frame_counter_interrupt.replace(false);
                let mut status = 0u8;
                for i in 0..4 {
                    if self.length_counters[i] > 0 && self.channel_enabled[i] {
                        status |= 1 << i;
                    }
                }
                if self.frame_counter_interrupt.get() {
                    status |= 0x40;
                }
                status
            }
            _ => 0,
        }
    }

    fn write(&mut self, address: u16, value: u8) {
        match address {
            0x4015 => {
                // APU status register - enable/disable channels
                for i in 0..4 {
                    self.channel_enabled[i] = value & (1 << i) != 0;
                    if !self.channel_enabled[i] {
                        self.length_counters[i] = 0;
                    }
                }
            }
            0x4017 => {
                // Frame counter register
                self.frame_counter_mode = value & 0x80 != 0;
                self.frame_counter_interrupt
                    .replace(value & 0b1100_0000 == 0);
                self.apu_cycle = 0;
                self.apu_even_cycle = false;
                self.frame_counter = 0;
                if !self.frame_counter_mode {
                    for i in 0..4 {
                        if self.channel_enabled[i]
                            && !self.length_counter_halt[i]
                            && self.length_counters[i] > 0
                        {
                            self.length_counters[i] -= 1;
                        }
                    }
                }
            }
            // Length counter load registers (high byte)
            0x4003 | 0x4007 | 0x400B | 0x400F => {
                let channel = ((address - 0x4003) / 4) as usize;
                let index = ((value & 0xF8) >> 3) as usize;
                self.length_counters[channel] = Self::LENGTH_TABLE[index];
            }
            // Halt flag registers (low byte with length counter halt bit)
            0x4000 | 0x4004 | 0x4008 | 0x400C => {
                let channel = ((address - 0x4000) / 4) as usize;
                self.length_counter_halt[channel] = value & 0x20 != 0;
            }
            _ => {} // Ignore other APU register writes (handled by apu.rs channel structs)
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_default() {
        let apu = NesApu::default();
        assert!(!apu.request_irq());
        assert_eq!(apu.apu_cycle, 0);
        assert!(!apu.frame_counter_mode);
    }

    #[test]
    fn test_frame_counter_interrupt_on_write() {
        let mut apu = NesApu::default();
        // Writing 0x00 to 0x4017 (4-step mode, interrupt enabled)
        apu.write(0x4017, 0b0000_0000);
        assert!(apu.frame_counter_interrupt.get());

        // Writing 0xC0 clears interrupt flag
        apu.write(0x4017, 0b1100_0000);
        assert!(!apu.frame_counter_interrupt.get());
    }

    #[test]
    fn test_channel_enable_disable() {
        let mut apu = NesApu::default();
        // Enable all channels
        apu.write(0x4015, 0x0F);
        assert!(apu.channel_enabled[0]);
        assert!(apu.channel_enabled[1]);
        assert!(apu.channel_enabled[2]);
        assert!(apu.channel_enabled[3]);

        // Disable all channels
        apu.write(0x4015, 0x00);
        assert!(!apu.channel_enabled[0]);
        assert!(!apu.channel_enabled[1]);
        assert!(!apu.channel_enabled[2]);
        assert!(!apu.channel_enabled[3]);
    }

    #[test]
    fn test_status_register_read_clears_interrupt() {
        let apu = NesApu::default();
        apu.frame_counter_interrupt.set(true);
        assert!(apu.request_irq());

        let _status = apu.read(0x4015);
        assert!(!apu.frame_counter_interrupt.get());
    }

    #[test]
    fn test_tick_returns_false_on_odd_cycles() {
        let mut apu = NesApu::default();
        // First tick: apu_even_cycle toggles to true, so we process
        // But cycle_mod = 1 is not a clock point, so returns false
        let result = apu.tick();
        assert!(!result);
    }

    #[test]
    fn test_length_counter_load() {
        let mut apu = NesApu::default();
        // Enable channel 0
        apu.write(0x4015, 0x01);
        // Load length counter for pulse 1 ($4003)
        // value 0xF8 -> index = (0xF8 & 0xF8) >> 3 = 0x1F = 31 -> LENGTH_TABLE[31] = 30
        apu.write(0x4003, 0xF8);
        assert_eq!(apu.length_counters[0], 30);
    }
}
