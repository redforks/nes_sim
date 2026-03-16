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

/// Helper: build a `NesBundle` backed by a `MockCartridge`.
fn make_bundle() -> NesBundle {
    let ppu = Ppu::default();
    let cartridge: Box<dyn Cartridge> = Box::new(MockCartridge::new());
    let ppu_cartridge = Rc::new(RefCell::new(PpuCartridgeState::new(ppu, cartridge)));
    let apu = Rc::new(RefCell::new(NesApu::default()));
    let lower_ram = Rc::new(RefCell::new(LowerRam::new()));

    let regions = vec![
        Region::with_defined(LowerRamRegionMcu::new(Rc::clone(&lower_ram))),
        Region::with_defined(PpuRegionMcu::new(Rc::clone(&ppu_cartridge))),
        Region::with_defined(OamDmaRegionMcu::new(
            Rc::clone(&ppu_cartridge),
            Rc::clone(&lower_ram),
        )),
        Region::with_defined(ApuRegionMcu::new(Rc::clone(&apu))),
        Region::with_defined(CartridgeRegionMcu::new(Rc::clone(&ppu_cartridge))),
    ];

    NesBundle {
        mcu: MappingMcu::new(regions),
        ppu_cartridge,
        apu,
    }
}

#[test]
fn test_lower_ram_mirroring() {
    let mut bundle = make_bundle();

    // Write to address 0x0000
    bundle.mcu.write(0x0000, 0x42);
    assert_eq!(bundle.mcu.read(0x0000), 0x42);

    // Should be mirrored to 0x0800, 0x1000, 0x1800
    assert_eq!(bundle.mcu.read(0x0800), 0x42);
    assert_eq!(bundle.mcu.read(0x1000), 0x42);
    assert_eq!(bundle.mcu.read(0x1800), 0x42);
}

#[test]
fn test_ppu_register_access() {
    let mut bundle = make_bundle();

    // PPU registers are at 0x2000-0x3FFF
    // Just verify we can access them without panicking
    let _val = bundle.mcu.read(0x2000);
    bundle.mcu.write(0x2001, 0xFF);
}

#[test]
fn test_apu_register_access() {
    let mut bundle = make_bundle();

    // APU registers are at 0x4000-0x401F (excluding 0x4014 which is OAM DMA)
    bundle.mcu.write(0x4000, 0x5A);
    assert_eq!(bundle.mcu.read(0x4000), 0x5A);

    // 0x401F is within the extended APU region; write should not panic
    bundle.mcu.write(0x401F, 0xAB);
}

#[test]
fn test_cartridge_prg_rom_access() {
    let ppu = Ppu::default();
    let mut cart = MockCartridge::new();
    cart.prg_rom[0] = 0xEA; // NOP opcode
    let cartridge: Box<dyn Cartridge> = Box::new(cart);
    let ppu_cartridge = Rc::new(RefCell::new(PpuCartridgeState::new(ppu, cartridge)));
    let apu = Rc::new(RefCell::new(NesApu::default()));
    let lower_ram = Rc::new(RefCell::new(LowerRam::new()));

    let regions = vec![
        Region::with_defined(LowerRamRegionMcu::new(Rc::clone(&lower_ram))),
        Region::with_defined(PpuRegionMcu::new(Rc::clone(&ppu_cartridge))),
        Region::with_defined(OamDmaRegionMcu::new(
            Rc::clone(&ppu_cartridge),
            Rc::clone(&lower_ram),
        )),
        Region::with_defined(ApuRegionMcu::new(Rc::clone(&apu))),
        Region::with_defined(CartridgeRegionMcu::new(Rc::clone(&ppu_cartridge))),
    ];

    let mcu = MappingMcu::new(regions);
    assert_eq!(mcu.read(0x8000), 0xEA);
}

#[test]
fn test_frame_counter_interrupt_flag() {
    let bundle = make_bundle();
    bundle.apu.borrow().frame_counter_interrupt.set(true);
    assert!(bundle.apu.borrow().request_irq());
}

#[test]
fn test_no_interrupt_when_both_false() {
    let bundle = make_bundle();
    assert!(!bundle.apu.borrow().request_irq());
}

#[test]
fn test_frame_counter_write() {
    let mut bundle = make_bundle();

    // Write to frame counter at 0x4017
    bundle.mcu.write(0x4017, 0b0000_0000);
    assert!(bundle.apu.borrow().frame_counter_interrupt.get());

    bundle.mcu.write(0x4017, 0b1100_0000);
    assert!(!bundle.apu.borrow().frame_counter_interrupt.get());
}

#[test]
fn test_status_register_read_clears_flags() {
    let bundle = make_bundle();
    bundle.apu.borrow().frame_counter_interrupt.set(true);
    assert!(bundle.apu.borrow().request_irq());

    // Reading 0x4015 should clear frame counter interrupt
    let _status = bundle.mcu.read(0x4015);
    assert!(!bundle.apu.borrow().frame_counter_interrupt.get());
}

#[test]
fn test_tick_ppu() {
    let bundle = make_bundle();

    // tick_ppu should return false when no NMI pending
    let result = bundle.ppu_cartridge.borrow_mut().tick_ppu();
    assert!(!result);
}

#[test]
fn test_memory_mapping() {
    let mut bundle = make_bundle();

    // Test different address regions
    bundle.mcu.write(0x0100, 0x11); // Lower RAM
    bundle.mcu.write(0x4000, 0x22); // APU

    assert_eq!(bundle.mcu.read(0x0100), 0x11);
    assert_eq!(bundle.mcu.read(0x4000), 0x22);
}

#[test]
fn test_ppu_dma() {
    let mut bundle = make_bundle();

    // Set up some data in RAM for DMA
    bundle.mcu.write(0x0200, 0x12);
    bundle.mcu.write(0x0201, 0x34);
    bundle.mcu.write(0x0202, 0x56);

    // Trigger DMA from address 0x02 (which means 0x0200)
    bundle.mcu.write(0x4014, 0x02);

    // The DMA should have transferred the data to OAM; verify no panic.
}

#[test]
fn test_cartridge_write() {
    let mut bundle = make_bundle();

    // Write to cartridge space (0x4020+)
    bundle.mcu.write(0x4020, 0xAB);
    // MockCartridge ignores writes; verify no panic.
}

#[test]
fn test_cartridge_read_boundary() {
    let ppu = Ppu::default();
    let mut cart = MockCartridge::new();
    cart.prg_rom[0] = 0x11;
    cart.prg_rom[0x7FFF] = 0x22; // Last byte of 32KB PRG ROM
    let cartridge: Box<dyn Cartridge> = Box::new(cart);
    let ppu_cartridge = Rc::new(RefCell::new(PpuCartridgeState::new(ppu, cartridge)));
    let apu = Rc::new(RefCell::new(NesApu::default()));
    let lower_ram = Rc::new(RefCell::new(LowerRam::new()));

    let regions = vec![
        Region::with_defined(LowerRamRegionMcu::new(Rc::clone(&lower_ram))),
        Region::with_defined(PpuRegionMcu::new(Rc::clone(&ppu_cartridge))),
        Region::with_defined(OamDmaRegionMcu::new(
            Rc::clone(&ppu_cartridge),
            Rc::clone(&lower_ram),
        )),
        Region::with_defined(ApuRegionMcu::new(Rc::clone(&apu))),
        Region::with_defined(CartridgeRegionMcu::new(Rc::clone(&ppu_cartridge))),
    ];

    let mcu = MappingMcu::new(regions);
    assert_eq!(mcu.read(0x8000), 0x11);
    assert_eq!(mcu.read(0xFFFF), 0x22);
}

#[test]
fn test_read_4015_returns_zero() {
    let bundle = make_bundle();

    // Reading 0x4015 with no channels active / no interrupt should return 0
    assert_eq!(bundle.mcu.read(0x4015), 0);
}

#[test]
fn test_build_function() {
    // Minimal iNES ROM
    use crate::ines::INesFile;
    let mut rom = Vec::new();
    rom.extend_from_slice(&[0x4e, 0x45, 0x53, 0x1a]);
    rom.push(1); // 1 PRG page
    rom.push(1); // 1 CHR page
    rom.push(0x00);
    rom.push(0x00);
    rom.extend_from_slice(&[0; 8]);
    rom.extend(std::iter::repeat_n(0u8, 16 * 1024));
    rom.extend(std::iter::repeat_n(0u8, 8 * 1024));
    let file = INesFile::new(rom).unwrap();

    let bundle = build(&file);
    // Verify the bundle components are present
    let _ = bundle.mcu;
    let _ = bundle.ppu_cartridge;
    let _ = bundle.apu;
}
