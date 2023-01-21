use crate::ines::INesFile;
use crate::mcu::{MappingMcu, Mcu, RamMcu};
use crate::nes::lower_ram::LowerRam;
use crate::nes::mapper;
use crate::nes::ppu::{Mirroring, Ppu, PpuTrait};
use log::info;

pub struct NesMcu<P: PpuTrait> {
    lower_ram: LowerRam,
    ppu: P,
    after_ppu: RamMcu<0x20>,
    inside_cartridge: MappingMcu,
}

pub fn build(file: &INesFile) -> impl Mcu {
    let inside_cartridge = MappingMcu::new(mapper::create_cartridge(file));
    let mut ppu = Ppu::new(mapper::create_ppu_pattern(file));
    ppu.set_mirroring(if file.header().ver_or_hor_arrangement {
        Mirroring::Vertical
    } else {
        Mirroring::Horizontal
    });

    NesMcu {
        lower_ram: LowerRam::new(),
        ppu,
        after_ppu: RamMcu::start_from(0x4000, [0; 0x20]),
        inside_cartridge,
    }
}

impl<P: PpuTrait> NesMcu<P> {
    fn ppu_dma(&mut self, address: u8) {
        info!("ppu dma");
        let addr = (address as u16) << 8;
        let mut buf = [0x00u8; 0x100];
        for i in 0..0x100 {
            buf[i] = self.read(addr + i as u16);
        }
        self.ppu.oam_dma(&buf);
    }
}

impl<P: PpuTrait> Mcu for NesMcu<P> {
    fn read(&self, address: u16) -> u8 {
        match address {
            0x0000..=0x1fff => self.lower_ram.read(address),
            0x2000..=0x3fff => self.ppu.read(address),
            0x4000..=0x401f => self.after_ppu.read(address),
            0x4020..=0xffff => self.inside_cartridge.read(address),
        }
    }

    fn write(&mut self, address: u16, value: u8) {
        match address {
            0x0000..=0x1fff => self.lower_ram.write(address, value),
            0x2000..=0x3fff => self.ppu.write(address, value),
            0x4014 => self.ppu_dma(value),
            0x4000..=0x401f => self.after_ppu.write(address, value),
            0x4020..=0xffff => self.inside_cartridge.write(address, value),
        }
    }

    fn get_ppu(&mut self) -> &mut dyn PpuTrait {
        self.ppu.get_ppu()
    }
}
