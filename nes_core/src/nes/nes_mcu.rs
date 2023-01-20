use crate::ines::INesFile;
use crate::mcu::{MappingMcu, Mcu, RamMcu};
use crate::nes::lower_ram::LowerRam;
use crate::nes::mapper;
use crate::nes::ppu::PpuTrait;

pub struct NesMcu<P: PpuTrait> {
    lower_ram: LowerRam,
    ppu: P,
    after_ppu: RamMcu<0x20>,
    inside_cartridge: MappingMcu,
}

impl<P: PpuTrait> Mcu for NesMcu<P> {
    fn read(&self, address: u16) -> u8 {
        match address {
            0x0000..=0x1fff => self.lower_ram.read(address),
            0x2000..=0x3fff => self.ppu.read(address),
            0x4000..=0x401f => self.after_ppu.read(address),
            0x4020..=0xffff => self.inside_cartridge.read(address),
            _ => unreachable!(),
        }
    }

    fn write(&mut self, address: u16, value: u8) {
        match address {
            0x0000..=0x1fff => self.lower_ram.write(address, value),
            0x2000..=0x3fff => self.ppu.write(address, value),
            0x4000..=0x401f => self.after_ppu.write(address, value),
            0x4020..=0xffff => self.inside_cartridge.write(address, value),
            _ => unreachable!(),
        }
    }
}

impl<P: PpuTrait> NesMcu<P> {
    pub fn build(file: &INesFile, ppu: P) -> NesMcu<P> {
        let inside_cartridge = MappingMcu::new(mapper::create_cartridge(file));
        NesMcu {
            lower_ram: LowerRam::new(),
            ppu,
            after_ppu: RamMcu::start_from(0x4000, [0; 0x20]),
            inside_cartridge,
        }
    }
}
