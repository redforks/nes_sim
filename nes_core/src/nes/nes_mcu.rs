use crate::ines::INesFile;
use crate::mcu::{Mcu, RamMcu};
use crate::nes::lower_ram::LowerRam;
use crate::nes::mapper;
use crate::nes::mapper::Cartridge;
use crate::nes::ppu::{Mirroring, Ppu};
use crate::render::Render;
use log::info;
use std::cell::{Cell, RefCell};

pub struct NesMcu {
    lower_ram: LowerRam,
    ppu: Ppu,
    after_ppu: RamMcu<0x20>,
    cartridge: Box<dyn Cartridge>,
    frame_counter_interrupt: Cell<bool>,
    vblank_started: Cell<bool>,
    // APU state for length counter timing
    apu_cycle: Cell<u64>,
    apu_even_cycle: Cell<bool>, // APU is clocked every OTHER CPU cycle
    frame_counter: Cell<u8>,
    frame_counter_mode: Cell<bool>,
    length_counters: RefCell<[u8; 4]>, // pulse1, pulse2, triangle, noise
    length_counter_halt: RefCell<[bool; 4]>,
    channel_enabled: RefCell<[bool; 4]>,
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
    let ppu = Ppu::default();
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
        vblank_started: Cell::new(false),
        apu_cycle: Cell::new(0),
        apu_even_cycle: Cell::new(false),
        frame_counter: Cell::new(0),
        frame_counter_mode: Cell::new(false),
        length_counters: RefCell::new([0; 4]),
        length_counter_halt: RefCell::new([false; 4]),
        channel_enabled: RefCell::new([false; 4]),
    }
}

impl NesMcu {
    // Length counter lookup table (index is high 3 bits of length counter load)
    const LENGTH_TABLE: [u8; 32] = [
        10, 254, 20, 2, 40, 4, 80, 6, 160, 8, 60, 10, 14, 12, 26, 14, 12, 16, 24, 18, 48, 20, 96,
        22, 192, 24, 72, 26, 16, 28, 32, 30,
    ];

    fn ppu_dma(&self, address: u8) {
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
                let length_counters = self.length_counters.borrow();
                let channel_enabled = self.channel_enabled.borrow();
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

    fn write(&self, address: u16, value: u8) {
        match address {
            0x0000..=0x1fff => self.lower_ram.write(address, value),
            0x2000..=0x3fff => {
                self.ppu.write(address, value);
            }
            0x4014 => self.ppu_dma(value),
            0x4015 => {
                // APU status register - enable/disable channels
                let mut channel_enabled = self.channel_enabled.borrow_mut();
                let mut length_counters = self.length_counters.borrow_mut();
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
                self.frame_counter_mode.set(value & 0x80 != 0);
                self.frame_counter_interrupt
                    .replace(value & 0b1100_0000 == 0);
                // Reset frame counter on write
                self.apu_cycle.set(0);
                self.apu_even_cycle.set(false);
                self.frame_counter.set(0);
                // In 4-step mode, immediately clock length counters when $4017 is written
                if !self.frame_counter_mode.get() {
                    let channel_enabled = self.channel_enabled.borrow();
                    let length_counter_halt = self.length_counter_halt.borrow();
                    let mut length_counters = self.length_counters.borrow_mut();
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
                self.length_counters.borrow_mut()[0] = Self::LENGTH_TABLE[index];
            }
            0x4007 => {
                // Pulse 2 length counter load (always loads, even if channel disabled)
                self.after_ppu.write(address, value);
                let index = ((value & 0xF8) >> 3) as usize;
                self.length_counters.borrow_mut()[1] = Self::LENGTH_TABLE[index];
            }
            0x400B => {
                // Triangle length counter load (always loads, even if channel disabled)
                self.after_ppu.write(address, value);
                let index = ((value & 0xF8) >> 3) as usize;
                self.length_counters.borrow_mut()[2] = Self::LENGTH_TABLE[index];
            }
            0x400F => {
                // Noise length counter load (always loads, even if channel disabled)
                self.after_ppu.write(address, value);
                let index = ((value & 0xF8) >> 3) as usize;
                self.length_counters.borrow_mut()[3] = Self::LENGTH_TABLE[index];
            }
            // Halt flag registers (low byte with length counter halt bit)
            0x4000 | 0x4004 | 0x4008 | 0x400C => {
                // Duty cycle / length counter halt
                self.after_ppu.write(address, value);
                let channel = ((address - 0x4000) / 4) as usize;
                self.length_counter_halt.borrow_mut()[channel] = value & 0x20 != 0;
            }
            0x4000..=0x401f => self.after_ppu.write(address, value),
            0x4020..=0xffff => {
                // Cartridge now takes &self and &Ppu
                self.cartridge.write(&self.ppu, address, value)
            }
        }
    }

    fn request_irq(&self) -> bool {
        self.frame_counter_interrupt.get()
    }

    fn tick_ppu(&self) -> bool {
        let result = self.ppu.tick(self.cartridge.pattern_ref());
        if result.vblank_started {
            self.vblank_started.set(true);
        }
        result.nmi
    }

    fn take_vblank(&self) -> bool {
        self.vblank_started.replace(false)
    }

    fn tick_apu(&self) -> bool {
        // Toggle the even/odd cycle flag
        let old = self.apu_even_cycle.get();
        self.apu_even_cycle.set(!old);

        // Only process the APU frame counter on even cycles (every other CPU cycle)
        if !self.apu_even_cycle.get() {
            return false;
        }

        let old_cycle = self.apu_cycle.get();
        self.apu_cycle.set(old_cycle + 1);

        // APU frame counter in 4-step mode:
        // The frame counter has a period of 14915 APU cycles (29830 CPU cycles)
        // According to NESdev wiki, clock points are at:
        // - APU cycle 3728: step 0 (clock length counters)
        // - APU cycle 7456: step 1 (clock length counters)
        // - APU cycle 11185: step 2 (clock length counters)
        // - APU cycle 14914: step 3 (DON'T clock length counters)
        // Note: The wiki shows these as cumulative counts, not intervals

        let cycle = self.apu_cycle.get();
        let cycle_mod = cycle % 14915;
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
            self.frame_counter.set(step);

            // In 4-step mode, clock length counters on steps 0, 1, 2 (NOT step 3)
            // In 5-step mode, clock length counters on steps 0, 1, 2, 3
            let frame_counter_mode = self.frame_counter_mode.get();
            let should_clock_length = if frame_counter_mode {
                // 5-step mode - clock on all steps
                true
            } else {
                // 4-step mode - don't clock on step 3
                step != 3
            };

            if should_clock_length {
                // Clock length counters for each channel
                // But only if channel is enabled and halt flag is not set
                let channel_enabled = self.channel_enabled.borrow();
                let length_counter_halt = self.length_counter_halt.borrow();
                let mut length_counters = self.length_counters.borrow_mut();
                for i in 0..4 {
                    if channel_enabled[i] && !length_counter_halt[i] && length_counters[i] > 0 {
                        length_counters[i] -= 1;
                    }
                }
            }

            // Frame interrupt occurs on step 3 (last step of 4-step sequence)
            if step == 3 && !frame_counter_mode {
                self.frame_counter_interrupt.set(true);
                return true;
            }
        }

        false
    }
}

#[cfg(test)]
mod tests;
