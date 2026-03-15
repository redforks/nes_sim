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
                // Pulse 1 length counter load
                self.after_ppu.write(address, value);
                let index = ((value & 0xF8) >> 3) as usize;
                self.length_counters[0] = Self::LENGTH_TABLE[index];
            }
            0x4007 => {
                // Pulse 2 length counter load
                self.after_ppu.write(address, value);
                let index = ((value & 0xF8) >> 3) as usize;
                self.length_counters[1] = Self::LENGTH_TABLE[index];
            }
            0x400B => {
                // Triangle length counter load
                self.after_ppu.write(address, value);
                let index = ((value & 0xF8) >> 3) as usize;
                self.length_counters[2] = Self::LENGTH_TABLE[index];
            }
            0x400F => {
                // Noise length counter load
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
    /// Returns true if a frame interrupt should occur
    pub fn tick_apu(&mut self) -> bool {
        self.apu_cycle += 1;

        // APU frame counter in 4-step mode clocks at cycles:
        // 1, 7459, 14917, 22375 (no length counter clock at 22375)
        // Then repeats: 29833, 37291, 374749, 44707 ...

        // Calculate which step we're on based on apu_cycle
        // Frame counter in 4-step mode has period of 29831 cycles (7458*3 + 7457)
        // Clocks at: 7458, 14916, 22374 (then wraps to 0)
        let step = if self.apu_cycle < 7458 {
            0 // Step 0 from reset until first clock
        } else if self.apu_cycle < 14916 {
            1 // Step 1
        } else if self.apu_cycle < 22374 {
            2 // Step 2
        } else if self.apu_cycle < 29831 {
            3 // Step 3 (no length counter clock)
        } else {
            // After first full sequence, the period is 29831 cycles
            let cycle_in_sequence = (self.apu_cycle - 29831) % 29831;
            if cycle_in_sequence < 7458 {
                0
            } else if cycle_in_sequence < 14916 {
                1
            } else if cycle_in_sequence < 22374 {
                2
            } else {
                3
            }
        };

        // Detect clock points: cycles 7458, 14916, 22374, then 29830 (wraps), etc.
        // Note: We DON'T clock at cycle 1 because we already clocked immediately when $4017 was written
        let is_clock_point = if self.apu_cycle <= 22374 {
            self.apu_cycle >= 7458
                && (self.apu_cycle == 7458 || self.apu_cycle == 14916 || self.apu_cycle == 22374)
        } else {
            // After first full sequence, the period is 29831 cycles
            let cycle_in_sequence = (self.apu_cycle - 29831) % 29831;
            cycle_in_sequence == 7458 - 29831
                || cycle_in_sequence == 14916 - 29831
                || cycle_in_sequence == 22374 - 29831
                || cycle_in_sequence == 0
            // Simplify: check if we're at a clock point in the next sequence
        };

        // Update frame counter when we clock
        if is_clock_point {
            self.frame_counter = step;

            // In 4-step mode, clock length counters on steps 0, 1, 2 (NOT step 3)
            // In 5-step mode, clock length counters on steps 0, 1, 2, 3
            let should_clock_length = if self.frame_counter_mode {
                // 5-step mode
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
mod tests {
    use super::*;

    struct MockCartridge {
        prg_rom: [u8; 0x8000],
        chr_rom: [u8; 0x2000],
    }

    impl MockCartridge {
        fn new() -> Self {
            MockCartridge {
                prg_rom: [0; 0x8000],
                chr_rom: [0; 0x2000],
            }
        }
    }

    impl Cartridge for MockCartridge {
        fn read(&self, address: u16) -> u8 {
            if address >= 0x8000 {
                self.prg_rom[(address - 0x8000) as usize]
            } else {
                0
            }
        }

        fn write(&mut self, _ppu: &mut Ppu, _address: u16, _value: u8) {}

        fn pattern_ref(&self) -> &[u8] {
            &self.chr_rom
        }
    }

    #[test]
    fn test_lower_ram_mirroring() {
        let mut mcu = NesMcu {
            lower_ram: LowerRam::new(),
            ppu: Ppu::default(),
            after_ppu: RamMcu::start_from(0x4000, [0; 0x20]),
            cartridge: Box::new(MockCartridge::new()),
            frame_counter_interrupt: Cell::new(false),
            dmc_interrupt: Cell::new(false),
            nmi_pending: Cell::new(false),
            apu_cycle: 0,
            frame_counter: 0,
            frame_counter_mode: false,
            length_counters: [0; 4],
            length_counter_halt: [false; 4],
            channel_enabled: [false; 4],
        };

        // Write to address 0x0000
        mcu.write(0x0000, 0x42);
        assert_eq!(mcu.read(0x0000), 0x42);

        // Should be mirrored to 0x0800, 0x1000, 0x1800
        assert_eq!(mcu.read(0x0800), 0x42);
        assert_eq!(mcu.read(0x1000), 0x42);
        assert_eq!(mcu.read(0x1800), 0x42);
    }

    #[test]
    fn test_ppu_register_access() {
        let mut mcu = NesMcu {
            lower_ram: LowerRam::new(),
            ppu: Ppu::default(),
            after_ppu: RamMcu::start_from(0x4000, [0; 0x20]),
            cartridge: Box::new(MockCartridge::new()),
            frame_counter_interrupt: Cell::new(false),
            dmc_interrupt: Cell::new(false),
            nmi_pending: Cell::new(false),
            apu_cycle: 0,
            frame_counter: 0,
            frame_counter_mode: false,
            length_counters: [0; 4],
            length_counter_halt: [false; 4],
            channel_enabled: [false; 4],
        };

        // PPU registers are at 0x2000-0x3FFF
        // Just verify we can access them without panicking
        let _val = mcu.read(0x2000);
        mcu.write(0x2001, 0xFF);
    }

    #[test]
    fn test_apu_register_access() {
        let mut mcu = NesMcu {
            lower_ram: LowerRam::new(),
            ppu: Ppu::default(),
            after_ppu: RamMcu::start_from(0x4000, [0; 0x20]),
            cartridge: Box::new(MockCartridge::new()),
            frame_counter_interrupt: Cell::new(false),
            dmc_interrupt: Cell::new(false),
            nmi_pending: Cell::new(false),
            apu_cycle: 0,
            frame_counter: 0,
            frame_counter_mode: false,
            length_counters: [0; 4],
            length_counter_halt: [false; 4],
            channel_enabled: [false; 4],
        };

        // APU registers are at 0x4000-0x401F
        mcu.write(0x4000, 0x5A);
        assert_eq!(mcu.read(0x4000), 0x5A);

        mcu.write(0x401F, 0xAB);
        assert_eq!(mcu.read(0x401F), 0xAB);
    }

    #[test]
    fn test_cartridge_prg_rom_access() {
        let mut cart = MockCartridge::new();
        cart.prg_rom[0] = 0xEA; // NOP opcode

        let mcu = NesMcu {
            lower_ram: LowerRam::new(),
            ppu: Ppu::default(),
            after_ppu: RamMcu::start_from(0x4000, [0; 0x20]),
            cartridge: Box::new(cart),
            frame_counter_interrupt: Cell::new(false),
            dmc_interrupt: Cell::new(false),
            nmi_pending: Cell::new(false),
            apu_cycle: 0,
            frame_counter: 0,
            frame_counter_mode: false,
            length_counters: [0; 4],
            length_counter_halt: [false; 4],
            channel_enabled: [false; 4],
        };

        assert_eq!(mcu.read(0x8000), 0xEA);
    }

    #[test]
    fn test_frame_counter_interrupt_flag() {
        let mcu = NesMcu {
            lower_ram: LowerRam::new(),
            ppu: Ppu::default(),
            after_ppu: RamMcu::start_from(0x4000, [0; 0x20]),
            cartridge: Box::new(MockCartridge::new()),
            frame_counter_interrupt: Cell::new(true),
            dmc_interrupt: Cell::new(false),
            nmi_pending: Cell::new(false),
            apu_cycle: 0,
            frame_counter: 0,
            frame_counter_mode: false,
            length_counters: [0; 4],
            length_counter_halt: [false; 4],
            channel_enabled: [false; 4],
        };

        assert!(mcu.request_irq());
    }

    #[test]
    fn test_dmc_interrupt_flag() {
        let mcu = NesMcu {
            lower_ram: LowerRam::new(),
            ppu: Ppu::default(),
            after_ppu: RamMcu::start_from(0x4000, [0; 0x20]),
            cartridge: Box::new(MockCartridge::new()),
            frame_counter_interrupt: Cell::new(false),
            dmc_interrupt: Cell::new(true),
            nmi_pending: Cell::new(false),
            apu_cycle: 0,
            frame_counter: 0,
            frame_counter_mode: false,
            length_counters: [0; 4],
            length_counter_halt: [false; 4],
            channel_enabled: [false; 4],
        };

        assert!(mcu.request_irq());
    }

    #[test]
    fn test_no_interrupt_when_both_false() {
        let mcu = NesMcu {
            lower_ram: LowerRam::new(),
            ppu: Ppu::default(),
            after_ppu: RamMcu::start_from(0x4000, [0; 0x20]),
            cartridge: Box::new(MockCartridge::new()),
            frame_counter_interrupt: Cell::new(false),
            dmc_interrupt: Cell::new(false),
            nmi_pending: Cell::new(false),
            apu_cycle: 0,
            frame_counter: 0,
            frame_counter_mode: false,
            length_counters: [0; 4],
            length_counter_halt: [false; 4],
            channel_enabled: [false; 4],
        };

        assert!(!mcu.request_irq());
    }

    #[test]
    fn test_get_ppu() {
        let mut mcu = NesMcu {
            lower_ram: LowerRam::new(),
            ppu: Ppu::default(),
            after_ppu: RamMcu::start_from(0x4000, [0; 0x20]),
            cartridge: Box::new(MockCartridge::new()),
            frame_counter_interrupt: Cell::new(false),
            dmc_interrupt: Cell::new(false),
            nmi_pending: Cell::new(false),
            apu_cycle: 0,
            frame_counter: 0,
            frame_counter_mode: false,
            length_counters: [0; 4],
            length_counter_halt: [false; 4],
            channel_enabled: [false; 4],
        };

        let _ppu = mcu.get_ppu();
        // Just verify we can access PPU without panicking
    }

    #[test]
    fn test_frame_counter_write() {
        let mut mcu = NesMcu {
            lower_ram: LowerRam::new(),
            ppu: Ppu::default(),
            after_ppu: RamMcu::start_from(0x4000, [0; 0x20]),
            cartridge: Box::new(MockCartridge::new()),
            frame_counter_interrupt: Cell::new(false),
            dmc_interrupt: Cell::new(false),
            nmi_pending: Cell::new(false),
            apu_cycle: 0,
            frame_counter: 0,
            frame_counter_mode: false,
            length_counters: [0; 4],
            length_counter_halt: [false; 4],
            channel_enabled: [false; 4],
        };

        // Write to frame counter at 0x4017
        mcu.write(0x4017, 0b0000_0000);
        assert!(mcu.frame_counter_interrupt.get());

        mcu.write(0x4017, 0b1100_0000);
        assert!(!mcu.frame_counter_interrupt.get());
    }

    #[test]
    fn test_status_register_read_clears_flags() {
        let mcu = NesMcu {
            lower_ram: LowerRam::new(),
            ppu: Ppu::default(),
            after_ppu: RamMcu::start_from(0x4000, [0; 0x20]),
            cartridge: Box::new(MockCartridge::new()),
            frame_counter_interrupt: Cell::new(true),
            dmc_interrupt: Cell::new(false),
            nmi_pending: Cell::new(false),
            apu_cycle: 0,
            frame_counter: 0,
            frame_counter_mode: false,
            length_counters: [0; 4],
            length_counter_halt: [false; 4],
            channel_enabled: [false; 4],
        };

        assert!(mcu.frame_counter_interrupt.get());

        // Reading 0x4015 should clear frame counter interrupt
        let _status = mcu.read(0x4015);
        assert!(!mcu.frame_counter_interrupt.get());
    }

    #[test]
    fn test_dmc_clear() {
        let mut mcu = NesMcu {
            lower_ram: LowerRam::new(),
            ppu: Ppu::default(),
            after_ppu: RamMcu::start_from(0x4000, [0; 0x20]),
            cartridge: Box::new(MockCartridge::new()),
            frame_counter_interrupt: Cell::new(false),
            dmc_interrupt: Cell::new(true),
            nmi_pending: Cell::new(false),
            apu_cycle: 0,
            frame_counter: 0,
            frame_counter_mode: false,
            length_counters: [0; 4],
            length_counter_halt: [false; 4],
            channel_enabled: [false; 4],
        };

        assert!(mcu.dmc_interrupt.get());

        // Writing to 0x4015 should clear DMC interrupt
        mcu.write(0x4015, 0);
        assert!(!mcu.dmc_interrupt.get());
    }

    #[test]
    fn test_tick_ppu() {
        let mut mcu = NesMcu {
            lower_ram: LowerRam::new(),
            ppu: Ppu::default(),
            after_ppu: RamMcu::start_from(0x4000, [0; 0x20]),
            cartridge: Box::new(MockCartridge::new()),
            frame_counter_interrupt: Cell::new(false),
            dmc_interrupt: Cell::new(false),
            nmi_pending: Cell::new(false),
            apu_cycle: 0,
            frame_counter: 0,
            frame_counter_mode: false,
            length_counters: [0; 4],
            length_counter_halt: [false; 4],
            channel_enabled: [false; 4],
        };

        // tick_ppu should return false when no NMI pending
        let result = mcu.tick_ppu();
        assert!(!result);
    }

    #[test]
    fn test_memory_mapping() {
        let mut mcu = NesMcu {
            lower_ram: LowerRam::new(),
            ppu: Ppu::default(),
            after_ppu: RamMcu::start_from(0x4000, [0; 0x20]),
            cartridge: Box::new(MockCartridge::new()),
            frame_counter_interrupt: Cell::new(false),
            dmc_interrupt: Cell::new(false),
            nmi_pending: Cell::new(false),
            apu_cycle: 0,
            frame_counter: 0,
            frame_counter_mode: false,
            length_counters: [0; 4],
            length_counter_halt: [false; 4],
            channel_enabled: [false; 4],
        };

        // Test different address regions
        mcu.write(0x0100, 0x11); // Lower RAM
        mcu.write(0x4000, 0x22); // APU

        assert_eq!(mcu.read(0x0100), 0x11);
        assert_eq!(mcu.read(0x4000), 0x22);
    }

    #[test]
    fn test_get_machine_mcu() {
        let mut mcu = NesMcu {
            lower_ram: LowerRam::new(),
            ppu: Ppu::default(),
            after_ppu: RamMcu::start_from(0x4000, [0; 0x20]),
            cartridge: Box::new(MockCartridge::new()),
            frame_counter_interrupt: Cell::new(false),
            dmc_interrupt: Cell::new(false),
            nmi_pending: Cell::new(false),
            apu_cycle: 0,
            frame_counter: 0,
            frame_counter_mode: false,
            length_counters: [0; 4],
            length_counter_halt: [false; 4],
            channel_enabled: [false; 4],
        };

        let machine_mcu = mcu.get_machine_mcu();
        // Should be able to call render
        let img = machine_mcu.render();
        assert_eq!(img.dimensions(), (256, 240));
    }

    #[test]
    fn test_render() {
        let mut mcu = NesMcu {
            lower_ram: LowerRam::new(),
            ppu: Ppu::default(),
            after_ppu: RamMcu::start_from(0x4000, [0; 0x20]),
            cartridge: Box::new(MockCartridge::new()),
            frame_counter_interrupt: Cell::new(false),
            dmc_interrupt: Cell::new(false),
            nmi_pending: Cell::new(false),
            apu_cycle: 0,
            frame_counter: 0,
            frame_counter_mode: false,
            length_counters: [0; 4],
            length_counter_halt: [false; 4],
            channel_enabled: [false; 4],
        };

        let img = mcu.render();
        assert_eq!(img.dimensions(), (256, 240));
    }

    #[test]
    fn test_ppu_dma() {
        let mut mcu = NesMcu {
            lower_ram: LowerRam::new(),
            ppu: Ppu::default(),
            after_ppu: RamMcu::start_from(0x4000, [0; 0x20]),
            cartridge: Box::new(MockCartridge::new()),
            frame_counter_interrupt: Cell::new(false),
            dmc_interrupt: Cell::new(false),
            nmi_pending: Cell::new(false),
            apu_cycle: 0,
            frame_counter: 0,
            frame_counter_mode: false,
            length_counters: [0; 4],
            length_counter_halt: [false; 4],
            channel_enabled: [false; 4],
        };

        // Set up some data in RAM for DMA
        mcu.write(0x0200, 0x12);
        mcu.write(0x0201, 0x34);
        mcu.write(0x0202, 0x56);

        // Trigger DMA from address 0x02 (which means 0x0200)
        mcu.write(0x4014, 0x02);

        // The DMA should have transferred the data to OAM
        // We can't directly check OAM contents, but we verify it doesn't panic
    }

    #[test]
    fn test_tick_ppu_with_nmi_pending() {
        let mut mcu = NesMcu {
            lower_ram: LowerRam::new(),
            ppu: Ppu::default(),
            after_ppu: RamMcu::start_from(0x4000, [0; 0x20]),
            cartridge: Box::new(MockCartridge::new()),
            frame_counter_interrupt: Cell::new(false),
            dmc_interrupt: Cell::new(false),
            nmi_pending: Cell::new(true),
            apu_cycle: 0,
            frame_counter: 0,
            frame_counter_mode: false,
            length_counters: [0; 4],
            length_counter_halt: [false; 4],
            channel_enabled: [false; 4],
        };

        // tick_ppu should return true when NMI is pending
        let result = mcu.tick_ppu();
        assert!(result);

        // Second call should return false as NMI was consumed
        let result = mcu.tick_ppu();
        assert!(!result);
    }

    #[test]
    fn test_nmi_pending_from_ppu_write() {
        let mut mcu = NesMcu {
            lower_ram: LowerRam::new(),
            ppu: Ppu::default(),
            after_ppu: RamMcu::start_from(0x4000, [0; 0x20]),
            cartridge: Box::new(MockCartridge::new()),
            frame_counter_interrupt: Cell::new(false),
            dmc_interrupt: Cell::new(false),
            nmi_pending: Cell::new(false),
            apu_cycle: 0,
            frame_counter: 0,
            frame_counter_mode: false,
            length_counters: [0; 4],
            length_counter_halt: [false; 4],
            channel_enabled: [false; 4],
        };

        // Write to PPU control register (0x2000) with NMI enabled during VBlank
        // This is a simplified test - actual NMI triggering depends on PPU state
        mcu.write(0x2000, 0x80); // Enable NMI

        // The nmi_pending flag would be set by PPU.write() if conditions are met
        // For now, just verify the write doesn't panic
    }

    #[test]
    fn test_cartridge_write() {
        let mut mcu = NesMcu {
            lower_ram: LowerRam::new(),
            ppu: Ppu::default(),
            after_ppu: RamMcu::start_from(0x4000, [0; 0x20]),
            cartridge: Box::new(MockCartridge::new()),
            frame_counter_interrupt: Cell::new(false),
            dmc_interrupt: Cell::new(false),
            nmi_pending: Cell::new(false),
            apu_cycle: 0,
            frame_counter: 0,
            frame_counter_mode: false,
            length_counters: [0; 4],
            length_counter_halt: [false; 4],
            channel_enabled: [false; 4],
        };

        // Write to cartridge space (0x4020+)
        mcu.write(0x4020, 0xAB);
        // MockCartridge ignores writes, but verify it doesn't panic
    }

    #[test]
    fn test_cartridge_read_boundary() {
        let mut cart = MockCartridge::new();
        cart.prg_rom[0] = 0x11;
        cart.prg_rom[0x7FFF] = 0x22; // Last byte of PRG ROM

        let mcu = NesMcu {
            lower_ram: LowerRam::new(),
            ppu: Ppu::default(),
            after_ppu: RamMcu::start_from(0x4000, [0; 0x20]),
            cartridge: Box::new(cart),
            frame_counter_interrupt: Cell::new(false),
            dmc_interrupt: Cell::new(false),
            nmi_pending: Cell::new(false),
            apu_cycle: 0,
            frame_counter: 0,
            frame_counter_mode: false,
            length_counters: [0; 4],
            length_counter_halt: [false; 4],
            channel_enabled: [false; 4],
        };

        assert_eq!(mcu.read(0x8000), 0x11);
        assert_eq!(mcu.read(0xFFFF), 0x22);
    }

    #[test]
    fn test_read_4015_returns_zero() {
        let mcu = NesMcu {
            lower_ram: LowerRam::new(),
            ppu: Ppu::default(),
            after_ppu: RamMcu::start_from(0x4000, [0; 0x20]),
            cartridge: Box::new(MockCartridge::new()),
            frame_counter_interrupt: Cell::new(false),
            dmc_interrupt: Cell::new(false),
            nmi_pending: Cell::new(false),
            apu_cycle: 0,
            frame_counter: 0,
            frame_counter_mode: false,
            length_counters: [0; 4],
            length_counter_halt: [false; 4],
            channel_enabled: [false; 4],
        };

        // Reading 0x4015 should return 0
        assert_eq!(mcu.read(0x4015), 0);
    }
}
