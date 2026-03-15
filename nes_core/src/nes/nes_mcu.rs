use crate::ines::INesFile;
use crate::mcu::{MachineMcu, Mcu, RamMcu};
use crate::nes::lower_ram::LowerRam;
use crate::nes::mapper;
use crate::nes::mapper::Cartridge;
use crate::nes::ppu::{Mirroring, Ppu, PpuTrait};
use image::RgbaImage;
use log::info;
use std::cell::Cell;

pub struct NesMcu {
    lower_ram: LowerRam,
    ppu: Ppu,
    after_ppu: RamMcu<0x20>,
    cartridge: Box<dyn Cartridge>,
    frame_counter_interrupt: Cell<bool>,
    dmc_interrupt: Cell<bool>,
    nmi_pending: Cell<bool>,
    // APU state for length counter timing
    apu_cycle: u64,
    apu_even_cycle: bool, // APU is clocked every OTHER CPU cycle
    frame_counter: u8,
    frame_counter_mode: bool,
    length_counters: [u8; 4], // pulse1, pulse2, triangle, noise
    length_counter_halt: [bool; 4],
    channel_enabled: [bool; 4],
}

pub fn build(file: &INesFile) -> impl Mcu + use<> {
    let cartridge = mapper::create_cartridge(file);
    let mut ppu = Ppu::default();
    ppu.set_mirroring(if file.header().ver_or_hor_arrangement {
        Mirroring::Vertical
    } else {
        Mirroring::Horizontal
    });

    NesMcu {
        lower_ram: LowerRam::new(),
        ppu,
        after_ppu: RamMcu::start_from(0x4000, [0; 0x20]),
        cartridge,
        frame_counter_interrupt: Cell::new(false),
        dmc_interrupt: Cell::new(false),
        nmi_pending: Cell::new(false),
        apu_cycle: 0,
        apu_even_cycle: false,
        frame_counter: 0,
        frame_counter_mode: false,
        length_counters: [0; 4],
        length_counter_halt: [false; 4],
        channel_enabled: [false; 4],
    }
}

impl NesMcu {
    // Length counter lookup table (index is high 3 bits of length counter load)
    const LENGTH_TABLE: [u8; 32] = [
        10, 254, 20, 2, 40, 4, 80, 6, 160, 8, 60, 10, 14, 12, 26, 14, 12, 16, 24, 18, 48, 20, 96,
        22, 192, 24, 72, 26, 16, 28, 32, 30,
    ];

    fn ppu_dma(&mut self, address: u8) {
        info!("ppu dma");
        let addr = (address as u16) << 8;
        let mut buf = [0x00u8; 0x100];
        for (i, item) in buf.iter_mut().enumerate() {
            *item = self.read(addr + i as u16);
        }
        self.ppu.oam_dma(&buf);
    }
}

impl Mcu for NesMcu {
    fn read(&self, address: u16) -> u8 {
        match address {
            0x0000..=0x1fff => self.lower_ram.read(address),
            0x2000..=0x3fff => self.ppu.read(self.cartridge.pattern_ref(), address),
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
                if self.dmc_interrupt.get() {
                    status |= 0x80;
                }
                status
            }
            0x4000..=0x401f => self.after_ppu.read(address),
            0x4020..=0xffff => self.cartridge.read(address),
        }
    }

    fn write(&mut self, address: u16, value: u8) {
        match address {
            0x0000..=0x1fff => self.lower_ram.write(address, value),
            0x2000..=0x3fff => {
                if self.ppu.write(address, value) {
                    self.nmi_pending.set(true);
                }
            }
            0x4014 => self.ppu_dma(value),
            0x4015 => {
                // APU status register - enable/disable channels and clear DMC interrupt
                self.channel_enabled[0] = value & 0x01 != 0;
                self.channel_enabled[1] = value & 0x02 != 0;
                self.channel_enabled[2] = value & 0x04 != 0;
                self.channel_enabled[3] = value & 0x08 != 0;
                self.dmc_interrupt.replace(false);
                // If a channel is disabled, its length counter is cleared
                for i in 0..4 {
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
                // Reset frame counter on write
                self.apu_cycle = 0;
                self.apu_even_cycle = false;
                self.frame_counter = 0;
                // In 4-step mode, immediately clock length counters when $4017 is written
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
            0x4003 => {
                // Pulse 1 length counter load (always loads, even if channel disabled)
                self.after_ppu.write(address, value);
                let index = ((value & 0xF8) >> 3) as usize;
                self.length_counters[0] = Self::LENGTH_TABLE[index];
            }
            0x4007 => {
                // Pulse 2 length counter load (always loads, even if channel disabled)
                self.after_ppu.write(address, value);
                let index = ((value & 0xF8) >> 3) as usize;
                self.length_counters[1] = Self::LENGTH_TABLE[index];
            }
            0x400B => {
                // Triangle length counter load (always loads, even if channel disabled)
                self.after_ppu.write(address, value);
                let index = ((value & 0xF8) >> 3) as usize;
                self.length_counters[2] = Self::LENGTH_TABLE[index];
            }
            0x400F => {
                // Noise length counter load (always loads, even if channel disabled)
                self.after_ppu.write(address, value);
                let index = ((value & 0xF8) >> 3) as usize;
                self.length_counters[3] = Self::LENGTH_TABLE[index];
            }
            // Halt flag registers (low byte with length counter halt bit)
            0x4000 | 0x4004 | 0x4008 | 0x400C => {
                // Duty cycle / length counter halt
                self.after_ppu.write(address, value);
                let channel = ((address - 0x4000) / 4) as usize;
                self.length_counter_halt[channel] = value & 0x20 != 0;
            }
            0x4000..=0x401f => self.after_ppu.write(address, value),
            0x4020..=0xffff => self.cartridge.write(&mut self.ppu, address, value),
        }
    }

    fn get_ppu(&mut self) -> &mut dyn PpuTrait {
        &mut self.ppu
    }

    fn get_machine_mcu(&mut self) -> &mut dyn MachineMcu {
        self
    }

    fn request_irq(&self) -> bool {
        self.frame_counter_interrupt.get() || self.dmc_interrupt.get()
    }

    fn tick_ppu(&mut self) -> bool {
        let tick_nmi = self.ppu.tick();
        let write_nmi = self.nmi_pending.replace(false);
        tick_nmi || write_nmi
    }

    fn tick_apu(&mut self) -> bool {
        self.tick_apu()
    }
}

impl NesMcu {
    /// Tick the APU frame counter and length counters
    /// The APU is clocked every OTHER CPU cycle
    /// Returns true if a frame interrupt should occur
    pub fn tick_apu(&mut self) -> bool {
        // Toggle the even/odd cycle flag
        self.apu_even_cycle = !self.apu_even_cycle;

        // Only process the APU frame counter on even cycles (every other CPU cycle)
        if !self.apu_even_cycle {
            return false;
        }

        self.apu_cycle += 1;

        // APU frame counter in 4-step mode:
        // The frame counter has a period of 14915 APU cycles (29830 CPU cycles)
        // According to NESdev wiki, clock points are at:
        // - APU cycle 3728: step 0 (clock length counters)
        // - APU cycle 7456: step 1 (clock length counters)
        // - APU cycle 11185: step 2 (clock length counters)
        // - APU cycle 14914: step 3 (DON'T clock length counters)
        // Note: The wiki shows these as cumulative counts, not intervals

        let cycle_mod = self.apu_cycle % 14915;
        let is_clock_point = cycle_mod == 3728 || cycle_mod == 7456 || cycle_mod == 11185;

        // Calculate which step we're on (0-3)
        let step = if cycle_mod < 3728 {
            0
        } else if cycle_mod < 7456 {
            1
        } else if cycle_mod < 11185 {
            2
        } else {
            3
        };

        // Update frame counter when we clock
        if is_clock_point {
            self.frame_counter = step;

            // In 4-step mode, clock length counters on steps 0, 1, 2 (NOT step 3)
            // In 5-step mode, clock length counters on steps 0, 1, 2, 3
            let should_clock_length = if self.frame_counter_mode {
                // 5-step mode - clock on all steps
                true
            } else {
                // 4-step mode - don't clock on step 3
                step != 3
            };

            if should_clock_length {
                // Clock length counters for each channel
                // But only if channel is enabled and halt flag is not set
                for i in 0..4 {
                    if self.channel_enabled[i]
                        && !self.length_counter_halt[i]
                        && self.length_counters[i] > 0
                    {
                        self.length_counters[i] -= 1;
                    }
                }
            }

            // Frame interrupt occurs on step 3 (last step of 4-step sequence)
            if self.frame_counter == 3 && !self.frame_counter_mode {
                self.frame_counter_interrupt.set(true);
                return true;
            }
        }

        false
    }
}

impl MachineMcu for NesMcu {
    fn render(&mut self) -> &RgbaImage {
        self.ppu.render(self.cartridge.pattern_ref())
    }
}

#[cfg(test)]
mod tests;
