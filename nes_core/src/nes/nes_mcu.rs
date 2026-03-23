use crate::ines::INesFile;
use crate::mcu::{Mcu, RamMcu};
use crate::nes::lower_ram::LowerRam;
use crate::nes::mapper;
use crate::nes::mapper::Cartridge;
use crate::nes::ppu::{Mirroring, Ppu};
use crate::render::Render;
use log::info;
use std::cell::Cell;

pub struct NesMcu {
    lower_ram: LowerRam,
    ppu: Ppu,
    after_ppu: RamMcu<0x20>,
    cartridge: Box<dyn Cartridge>,
    frame_counter_interrupt: Cell<bool>,
    vblank_started: bool,
    // APU state for length counter timing
    apu_cycle: u64,
    apu_even_cycle: bool, // APU is clocked every OTHER CPU cycle
    frame_counter: u8,
    frame_counter_mode: bool,
    length_counters: [u8; 4], // pulse1, pulse2, triangle, noise
    length_counter_halt: [bool; 4],
    channel_enabled: [bool; 4],
}

pub fn build(file: &INesFile) -> NesMcu {
    build_with_renderer(file, None)
}

/// Build a NesMcu with a custom renderer
///
/// # Parameters
/// - `file`: The iNES file to load
/// - `renderer`: Optional custom renderer. If None, uses default ImageRender.
pub fn build_with_renderer(file: &INesFile, renderer: Option<Box<dyn Render>>) -> NesMcu {
    let cartridge = mapper::create_cartridge(file);
    let mut ppu = Ppu::default();
    ppu.set_mirroring(if file.header().ver_or_hor_arrangement {
        Mirroring::Vertical
    } else {
        Mirroring::Horizontal
    });

    // Set custom renderer if provided
    if let Some(r) = renderer {
        ppu.set_renderer(r);
    }

    NesMcu {
        lower_ram: LowerRam::new(),
        ppu,
        after_ppu: RamMcu::start_from(0x4000, [0; 0x20]),
        cartridge,
        frame_counter_interrupt: Cell::new(false),
        vblank_started: false,
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

    pub fn take_vblank(&mut self) -> bool {
        let val = self.vblank_started;
        self.vblank_started = false;
        val
    }

    /// Tick PPU by one dot. Returns true if NMI should be triggered.
    /// Pattern data is passed through from the cartridge for rendering.
    /// Tick PPU by one dot. Returns true if NMI should be triggered.
    /// Pattern data is passed through from the cartridge for rendering.
    pub fn tick_ppu(&mut self) -> bool {
        self.ppu.tick(self.cartridge.pattern_ref())
    }

    /// Tick APU frame counter. Returns true if frame IRQ should be triggered.
    /// Tick APU frame counter. Returns true if frame IRQ should be triggered.
    pub fn tick_apu(&mut self) -> bool {
        // Toggle the even/odd cycle flag
        self.apu_even_cycle = !self.apu_even_cycle;

        // Only process the APU frame counter on even cycles (every other CPU cycle)
        if !self.apu_even_cycle {
            return false;
        }

        self.apu_cycle = self.apu_cycle.wrapping_add(1);

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
            if step == 3 && !self.frame_counter_mode {
                self.frame_counter_interrupt.set(true);
                return true;
            }
        }

        false
    }
}

impl Mcu for NesMcu {
    fn read(&mut self, address: u16) -> u8 {
        match address {
            0x0000..=0x1fff => self.lower_ram.read(address),
            0x2000..=0x3fff => self.ppu.read(self.cartridge.pattern_ref(), address),
            0x4015 => {
                // APU status register - return length counter status
                self.frame_counter_interrupt.replace(false);
                let mut status = 0u8;
                let length_counters = &self.length_counters;
                let channel_enabled = &self.channel_enabled;
                for i in 0..4 {
                    if length_counters[i] > 0 && channel_enabled[i] {
                        status |= 1 << i;
                    }
                }
                if self.frame_counter_interrupt.get() {
                    status |= 0x40;
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
                self.ppu.write(address, value);
            }
            0x4014 => self.ppu_dma(value),
            0x4015 => {
                // APU status register - enable/disable channels
                let channel_enabled = &mut self.channel_enabled;
                let length_counters = &mut self.length_counters;
                channel_enabled[0] = value & 0x01 != 0;
                channel_enabled[1] = value & 0x02 != 0;
                channel_enabled[2] = value & 0x04 != 0;
                channel_enabled[3] = value & 0x08 != 0;
                // If a channel is disabled, its length counter is cleared
                for i in 0..4 {
                    if !channel_enabled[i] {
                        length_counters[i] = 0;
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
                    let channel_enabled = &self.channel_enabled;
                    let length_counter_halt = &self.length_counter_halt;
                    let length_counters = &mut self.length_counters;
                    for i in 0..4 {
                        if channel_enabled[i] && !length_counter_halt[i] && length_counters[i] > 0 {
                            length_counters[i] -= 1;
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
            0x4020..=0xffff => {
                // Cartridge now takes &mut self and &mut Ppu
                self.cartridge.write(&mut self.ppu, address, value)
            }
        }
    }

    fn request_irq(&self) -> bool {
        self.frame_counter_interrupt.get()
    }

    fn ppu_status(&self) -> (u16, u16) {
        self.ppu.scanline_and_dot()
    }
}

#[cfg(test)]
mod tests;
