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
        vblank_started: Cell::new(false),
        apu_cycle: 0,
        apu_even_cycle: false,
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
        vblank_started: Cell::new(false),
        apu_cycle: 0,
        apu_even_cycle: false,
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
        vblank_started: Cell::new(false),
        apu_cycle: 0,
        apu_even_cycle: false,
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
        vblank_started: Cell::new(false),
        apu_cycle: 0,
        apu_even_cycle: false,
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
        vblank_started: Cell::new(false),
        apu_cycle: 0,
        apu_even_cycle: false,
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
        vblank_started: Cell::new(false),
        apu_cycle: 0,
        apu_even_cycle: false,
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
        vblank_started: Cell::new(false),
        apu_cycle: 0,
        apu_even_cycle: false,
        frame_counter: 0,
        frame_counter_mode: false,
        length_counters: [0; 4],
        length_counter_halt: [false; 4],
        channel_enabled: [false; 4],
    };

    assert!(!mcu.request_irq());
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
        vblank_started: Cell::new(false),
        apu_cycle: 0,
        apu_even_cycle: false,
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
        vblank_started: Cell::new(false),
        apu_cycle: 0,
        apu_even_cycle: false,
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
        vblank_started: Cell::new(false),
        apu_cycle: 0,
        apu_even_cycle: false,
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
        vblank_started: Cell::new(false),
        apu_cycle: 0,
        apu_even_cycle: false,
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
        vblank_started: Cell::new(false),
        apu_cycle: 0,
        apu_even_cycle: false,
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
fn test_ppu_dma() {
    let mut mcu = NesMcu {
        lower_ram: LowerRam::new(),
        ppu: Ppu::default(),
        after_ppu: RamMcu::start_from(0x4000, [0; 0x20]),
        cartridge: Box::new(MockCartridge::new()),
        frame_counter_interrupt: Cell::new(false),
        dmc_interrupt: Cell::new(false),
        nmi_pending: Cell::new(false),
        vblank_started: Cell::new(false),
        apu_cycle: 0,
        apu_even_cycle: false,
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
        vblank_started: Cell::new(false),
        apu_cycle: 0,
        apu_even_cycle: false,
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
        vblank_started: Cell::new(false),
        apu_cycle: 0,
        apu_even_cycle: false,
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
        vblank_started: Cell::new(false),
        apu_cycle: 0,
        apu_even_cycle: false,
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
        vblank_started: Cell::new(false),
        apu_cycle: 0,
        apu_even_cycle: false,
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
        vblank_started: Cell::new(false),
        apu_cycle: 0,
        apu_even_cycle: false,
        frame_counter: 0,
        frame_counter_mode: false,
        length_counters: [0; 4],
        length_counter_halt: [false; 4],
        channel_enabled: [false; 4],
    };

    // Reading 0x4015 should return 0
    assert_eq!(mcu.read(0x4015), 0);
}
