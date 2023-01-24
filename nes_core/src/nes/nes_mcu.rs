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
    should_irq: Cell<bool>,
}

pub fn build(file: &INesFile) -> impl Mcu {
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
        should_irq: Cell::new(false),
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
                self.should_irq.replace(false);
                0
            }
            0x4000..=0x401f => self.after_ppu.read(address),
            0x4020..=0xffff => self.cartridge.read(address),
        }
    }

    fn write(&mut self, address: u16, value: u8) {
        match address {
            0x0000..=0x1fff => self.lower_ram.write(address, value),
            0x2000..=0x3fff => self.ppu.write(address, value),
            0x4014 => self.ppu_dma(value),
            0x4017 => {
                self.should_irq.replace(value & 0b1100_0000 == 0);
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
        self.should_irq.get()
    }
}

impl MachineMcu for NesMcu {
    fn render(&mut self) -> &RgbaImage {
        self.ppu.render(self.cartridge.pattern_ref())
    }
}
