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
    }
}

impl NesMcu {
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
                self.frame_counter_interrupt.replace(false);
                0
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
                self.dmc_interrupt.replace(false);
            }
            0x4017 => {
                self.frame_counter_interrupt
                    .replace(value & 0b1100_0000 == 0);
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

        let mut mcu = NesMcu {
            lower_ram: LowerRam::new(),
            ppu: Ppu::default(),
            after_ppu: RamMcu::start_from(0x4000, [0; 0x20]),
            cartridge: Box::new(cart),
            frame_counter_interrupt: Cell::new(false),
            dmc_interrupt: Cell::new(false),
            nmi_pending: Cell::new(false),
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
        };

        // Write to frame counter at 0x4017
        mcu.write(0x4017, 0b0000_0000);
        assert!(mcu.frame_counter_interrupt.get());

        mcu.write(0x4017, 0b1100_0000);
        assert!(!mcu.frame_counter_interrupt.get());
    }

    #[test]
    fn test_status_register_read_clears_flags() {
        let mut mcu = NesMcu {
            lower_ram: LowerRam::new(),
            ppu: Ppu::default(),
            after_ppu: RamMcu::start_from(0x4000, [0; 0x20]),
            cartridge: Box::new(MockCartridge::new()),
            frame_counter_interrupt: Cell::new(true),
            dmc_interrupt: Cell::new(false),
            nmi_pending: Cell::new(false),
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
        };

        // Test different address regions
        mcu.write(0x0100, 0x11); // Lower RAM
        mcu.write(0x4000, 0x22); // APU

        assert_eq!(mcu.read(0x0100), 0x11);
        assert_eq!(mcu.read(0x4000), 0x22);
    }
}
