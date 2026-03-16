use crate::ines::INesFile;
use crate::mcu::{DefinedRegion, MappingMcu, Mcu, RamMcu, Region};
use crate::nes::lower_ram::LowerRam;
use crate::nes::mapper;
use crate::nes::mapper::Cartridge;
use crate::nes::nes_apu::NesApu;
use crate::nes::ppu::{Mirroring, Ppu};
use crate::render::Render;
use log::info;
use std::cell::RefCell;
use std::rc::Rc;

/// Shared state between Machine, PpuRegionMcu, and CartridgeRegionMcu.
///
/// The PPU requires CHR ROM (pattern data) from the cartridge for rendering.
/// The cartridge's `write()` method may update PPU mirroring (MMC1 mapper).
/// Both are therefore bundled together and shared via `Rc<RefCell<>>`.
pub struct PpuCartridgeState {
    pub ppu: Ppu,
    pub cartridge: Box<dyn Cartridge>,
    /// Set when VBlank starts; cleared by `take_vblank()`.
    vblank_started: bool,
}

impl PpuCartridgeState {
    pub fn new(ppu: Ppu, cartridge: Box<dyn Cartridge>) -> Self {
        PpuCartridgeState {
            ppu,
            cartridge,
            vblank_started: false,
        }
    }

    /// Tick the PPU by one dot.
    /// Returns true if NMI should be triggered.
    pub fn tick_ppu(&mut self) -> bool {
        let result = self.ppu.tick(self.cartridge.pattern_ref());
        if result.vblank_started {
            self.vblank_started = true;
        }
        result.nmi
    }

    /// Returns and clears the vblank_started flag.
    pub fn take_vblank(&mut self) -> bool {
        let v = self.vblank_started;
        self.vblank_started = false;
        v
    }

    /// Forward a cartridge write, passing `&mut self.ppu` to the cartridge.
    ///
    /// This exists because the borrow checker cannot prove that `self.ppu` and
    /// `self.cartridge` are independent fields when both are borrowed through a
    /// single `borrow_mut()` of the enclosing `RefCell`.  By moving the split
    /// into a method on `PpuCartridgeState` itself the compiler can see that the
    /// two fields are distinct.
    pub fn cartridge_write(&mut self, address: u16, value: u8) {
        self.cartridge.write(&mut self.ppu, address, value);
    }
}

/// MCU region for PPU registers ($2000–$3FFF).
///
/// Shares ownership of `PpuCartridgeState` so cartridge CHR ROM is available
/// for PPU reads, and PPU mirroring can be updated from cartridge writes.
pub struct PpuRegionMcu {
    state: Rc<RefCell<PpuCartridgeState>>,
}

impl PpuRegionMcu {
    pub fn new(state: Rc<RefCell<PpuCartridgeState>>) -> Self {
        PpuRegionMcu { state }
    }
}

impl Mcu for PpuRegionMcu {
    fn read(&self, address: u16) -> u8 {
        let state = self.state.borrow();
        state.ppu.read(state.cartridge.pattern_ref(), address)
    }

    fn write(&mut self, address: u16, value: u8) {
        self.state.borrow_mut().ppu.write(address, value);
    }
}

impl DefinedRegion for PpuRegionMcu {
    fn region(&self) -> (u16, u16) {
        (0x2000, 0x3FFF)
    }
}

/// MCU region for the PPU OAM DMA register ($4014).
///
/// A write to $4014 triggers a 256-byte DMA from CPU RAM into PPU OAM.
/// This region wraps the `LowerRam` reference so the DMA can read from it.
pub struct OamDmaRegionMcu {
    state: Rc<RefCell<PpuCartridgeState>>,
    lower_ram: Rc<RefCell<LowerRam>>,
}

impl OamDmaRegionMcu {
    pub fn new(state: Rc<RefCell<PpuCartridgeState>>, lower_ram: Rc<RefCell<LowerRam>>) -> Self {
        OamDmaRegionMcu { state, lower_ram }
    }
}

impl Mcu for OamDmaRegionMcu {
    fn read(&self, _address: u16) -> u8 {
        // $4014 is write-only; reads return open bus (0x55 or similar)
        0x55
    }

    fn write(&mut self, _address: u16, value: u8) {
        info!("ppu dma");
        let base = (value as u16) << 8;
        let mut buf = [0u8; 0x100];
        {
            let ram = self.lower_ram.borrow();
            for (i, item) in buf.iter_mut().enumerate() {
                *item = ram.read(base + i as u16);
            }
        }
        self.state.borrow_mut().ppu.oam_dma(&buf);
    }
}

impl DefinedRegion for OamDmaRegionMcu {
    fn region(&self) -> (u16, u16) {
        (0x4014, 0x4014)
    }
}

/// MCU region for cartridge space ($4020–$FFFF).
///
/// Writes to this range may update PPU mirroring (e.g., MMC1 mapper), so
/// the PPU is accessed through the shared `PpuCartridgeState`.
pub struct CartridgeRegionMcu {
    state: Rc<RefCell<PpuCartridgeState>>,
}

impl CartridgeRegionMcu {
    pub fn new(state: Rc<RefCell<PpuCartridgeState>>) -> Self {
        CartridgeRegionMcu { state }
    }
}

impl Mcu for CartridgeRegionMcu {
    fn read(&self, address: u16) -> u8 {
        self.state.borrow().cartridge.read(address)
    }

    fn write(&mut self, address: u16, value: u8) {
        self.state.borrow_mut().cartridge_write(address, value);
    }
}

impl DefinedRegion for CartridgeRegionMcu {
    fn region(&self) -> (u16, u16) {
        (0x4020, 0xFFFF)
    }
}

/// MCU region for the lower RAM ($0000–$1FFF).
///
/// Wraps `LowerRam` in a `Rc<RefCell<>>` so it can be shared with the
/// `OamDmaRegionMcu`.
pub struct LowerRamRegionMcu {
    lower_ram: Rc<RefCell<LowerRam>>,
}

impl LowerRamRegionMcu {
    pub fn new(lower_ram: Rc<RefCell<LowerRam>>) -> Self {
        LowerRamRegionMcu { lower_ram }
    }
}

impl Mcu for LowerRamRegionMcu {
    fn read(&self, address: u16) -> u8 {
        self.lower_ram.borrow().read(address)
    }

    fn write(&mut self, address: u16, value: u8) {
        self.lower_ram.borrow_mut().write(address, value);
    }
}

impl DefinedRegion for LowerRamRegionMcu {
    fn region(&self) -> (u16, u16) {
        (0x0000, 0x1FFF)
    }
}

/// APU MCU region wrapper that also routes length counter loads and halt flags.
///
/// Wraps `NesApu` in a `Rc<RefCell<>>` so Machine can access it for ticking.
pub struct ApuRegionMcu {
    pub apu: Rc<RefCell<NesApu>>,
    /// General-purpose APU register RAM ($4000–$401F excluding special addresses).
    after_ppu: RamMcu<0x20>,
}

impl ApuRegionMcu {
    pub fn new(apu: Rc<RefCell<NesApu>>) -> Self {
        ApuRegionMcu {
            apu,
            after_ppu: RamMcu::start_from(0x4000, [0; 0x20]),
        }
    }
}

impl Mcu for ApuRegionMcu {
    fn read(&self, address: u16) -> u8 {
        match address {
            0x4015 => self.apu.borrow().read(address),
            _ => self.after_ppu.read(address),
        }
    }

    fn write(&mut self, address: u16, value: u8) {
        match address {
            0x4015 | 0x4017 => {
                self.apu.borrow_mut().write(address, value);
                // Also write to after_ppu for any raw register reads
                self.after_ppu.write(address, value);
            }
            // Length counter load registers: update both NesApu and after_ppu
            0x4003 | 0x4007 | 0x400B | 0x400F => {
                self.apu.borrow_mut().write(address, value);
                self.after_ppu.write(address, value);
            }
            // Halt flag registers
            0x4000 | 0x4004 | 0x4008 | 0x400C => {
                self.apu.borrow_mut().write(address, value);
                self.after_ppu.write(address, value);
            }
            _ => self.after_ppu.write(address, value),
        }
    }
}

impl DefinedRegion for ApuRegionMcu {
    fn region(&self) -> (u16, u16) {
        // Covers APU registers ($4000–$401F), including the status register
        // ($4015) and frame counter ($4017).  The OAM DMA address ($4014) is
        // handled by a separate `OamDmaRegionMcu` that must appear earlier in
        // the `MappingMcu` region list so it takes precedence.
        (0x4000, 0x401F)
    }
}

/// Bundle produced by `build_nes()` containing the `MappingMcu` for the CPU
/// plus shared handles for the PPU/cartridge state and APU used by `Machine`.
pub struct NesBundle {
    pub mcu: MappingMcu,
    pub ppu_cartridge: Rc<RefCell<PpuCartridgeState>>,
    pub apu: Rc<RefCell<NesApu>>,
}

/// Build NES hardware into a `NesBundle`.
///
/// Registers all NES devices into a `MappingMcu` and returns shared handles
/// to the PPU/cartridge state and APU so `Machine` can tick them directly.
pub fn build(file: &INesFile) -> NesBundle {
    build_with_renderer(file, None)
}

/// Build NES hardware with a custom renderer into a `NesBundle`.
pub fn build_with_renderer(file: &INesFile, renderer: Option<Box<dyn Render>>) -> NesBundle {
    let cartridge = mapper::create_cartridge(file);
    let mut ppu = Ppu::default();
    ppu.set_mirroring(if file.header().ver_or_hor_arrangement {
        Mirroring::Vertical
    } else {
        Mirroring::Horizontal
    });
    if let Some(r) = renderer {
        ppu.set_renderer(r);
    }

    let ppu_cartridge = Rc::new(RefCell::new(PpuCartridgeState::new(ppu, cartridge)));
    let apu = Rc::new(RefCell::new(NesApu::default()));
    let lower_ram = Rc::new(RefCell::new(LowerRam::new()));

    let regions = vec![
        Region::with_defined(LowerRamRegionMcu::new(Rc::clone(&lower_ram))),
        Region::with_defined(PpuRegionMcu::new(Rc::clone(&ppu_cartridge))),
        // OamDma must come before ApuRegionMcu so $4014 is caught before the
        // broader APU range ($4000–$401F) matches it.
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

#[cfg(test)]
mod tests;
