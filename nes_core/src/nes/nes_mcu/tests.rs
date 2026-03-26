use super::*;

struct MockCartridge {
    prg_rom: [u8; 0x8000],
    chr_rom: [u8; 0x2000],
}

impl MockCartridge {
    fn new() -> Self {
        Self {
            prg_rom: [0; 0x8000],
            chr_rom: [0; 0x2000],
        }
    }
}

impl Cartridge for MockCartridge {
    fn read(&mut self, address: u16) -> u8 {
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

fn test_mcu() -> NesMcu {
    NesMcu {
        lower_ram: LowerRam::new(),
        ppu: Ppu::default(),
        after_ppu: RamMcu::start_from(0x4000, [0; 0x20]),
        cartridge: Box::new(MockCartridge::new()),
        apu: ApuController::new(FakeApuControllerDriver::default()),
    }
}

#[test]
fn test_lower_ram_mirroring() {
    let mut mcu = test_mcu();

    mcu.write(0x0000, 0x42);
    assert_eq!(mcu.read(0x0000), 0x42);
    assert_eq!(mcu.read(0x0800), 0x42);
    assert_eq!(mcu.read(0x1000), 0x42);
    assert_eq!(mcu.read(0x1800), 0x42);
}

#[test]
fn test_apu_register_access() {
    let mut mcu = test_mcu();

    mcu.write(0x4000, 0x5A);
    assert_eq!(mcu.read(0x4000), 0x5A);

    mcu.write(0x4015, 0x0F);
    let status = mcu.read(0x4015);
    assert_eq!(status & 0x0F, 0);
}

#[test]
fn test_apu_frame_counter_irq_flow() {
    let mut mcu = test_mcu();

    mcu.write(0x4017, 0x00);
    for _ in 0..(14914 * 2) {
        mcu.tick_apu();
    }

    let status = mcu.read(0x4015);
    assert_ne!(status & 0x40, 0);
}

#[test]
fn test_frame_counter_inhibit_clears_irq() {
    let mut mcu = test_mcu();

    mcu.write(0x4017, 0x00);
    for _ in 0..(14914 * 2) {
        mcu.tick_apu();
    }

    mcu.write(0x4017, 0x40);
    assert_eq!(mcu.read(0x4015) & 0x40, 0);
}

#[test]
fn test_length_counter_status_comes_from_apu_controller() {
    let mut mcu = test_mcu();

    mcu.write(0x4000, 0x00);
    mcu.write(0x4002, 0x34);
    mcu.write(0x4003, 0xF8);
    mcu.write(0x4015, 0x01);

    assert_eq!(mcu.read(0x4015) & 0x01, 0x01);

    mcu.write(0x4015, 0x00);
    assert_eq!(mcu.read(0x4015) & 0x01, 0x00);
}

#[test]
fn test_ppu_dma() {
    let mut mcu = test_mcu();

    mcu.write(0x0200, 0x12);
    mcu.write(0x0201, 0x34);
    mcu.write(0x0202, 0x56);
    mcu.write(0x4014, 0x02);
}
