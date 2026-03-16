use crate::ines::INesFile;
use crate::machine::Machine;
use crate::mcu::Mcu;
use crate::render::Render;
use crate::{EmptyPlugin, Plugin};

pub mod apu;
pub mod controller;
mod lower_ram;
mod mapper;
pub mod nes_apu;
pub mod nes_mcu;
pub mod ppu;

/// Create a `Machine` wired up with full NES hardware from an iNES file.
///
/// The `Machine` owns the `MappingMcu` as its CPU bus, and also holds direct
/// references to `PpuCartridgeState` and `NesApu` so it can tick them every
/// instruction without going through the `Mcu` trait.
pub fn create_machine(file: &INesFile) -> Machine<EmptyPlugin> {
    create_machine_with_renderer(file, None)
}

/// Create a `Machine` wired up with full NES hardware and a custom renderer.
pub fn create_machine_with_renderer(
    file: &INesFile,
    renderer: Option<Box<dyn Render>>,
) -> Machine<EmptyPlugin> {
    let bundle = nes_mcu::build_with_renderer(file, renderer);
    let mut machine = Machine::new(Box::new(bundle.mcu));
    machine.attach_ppu_cartridge(bundle.ppu_cartridge);
    machine.attach_apu(bundle.apu);
    machine
}

/// Create a `Machine` with a custom plugin and full NES hardware.
pub fn create_machine_with_plugin<P: Plugin>(file: &INesFile, plugin: P) -> Machine<P> {
    create_machine_with_plugin_and_renderer(file, plugin, None)
}

/// Create a `Machine` with a custom plugin, full NES hardware, and a custom renderer.
pub fn create_machine_with_plugin_and_renderer<P: Plugin>(
    file: &INesFile,
    plugin: P,
    renderer: Option<Box<dyn Render>>,
) -> Machine<P> {
    let bundle = nes_mcu::build_with_renderer(file, renderer);
    let mut machine = Machine::with_plugin(plugin, Box::new(bundle.mcu));
    machine.attach_ppu_cartridge(bundle.ppu_cartridge);
    machine.attach_apu(bundle.apu);
    machine
}

/// Create a raw `MappingMcu` from an iNES file (legacy interface).
///
/// Prefer `create_machine` unless you need to manage the `Machine` lifecycle
/// separately. The returned MCU does **not** tick PPU or APU — those are
/// ticked by the `Machine` that wraps it when using `create_machine`.
pub fn create_mcu(file: &INesFile) -> impl Mcu + use<> {
    nes_mcu::build(file).mcu
}

/// Create a raw `MappingMcu` with a custom renderer (legacy interface).
///
/// See `create_mcu` for caveats — prefer `create_machine_with_renderer` instead.
pub fn create_mcu_with_renderer(
    file: &INesFile,
    renderer: Option<Box<dyn Render>>,
) -> impl Mcu + use<> {
    nes_mcu::build_with_renderer(file, renderer).mcu
}

#[cfg(test)]
mod tests {
    use super::*;

    fn make_test_ines() -> INesFile {
        let mut rom = Vec::new();
        rom.extend_from_slice(&[0x4e, 0x45, 0x53, 0x1a]); // NES signature
        rom.push(1); // 1 PRG page (16KB)
        rom.push(1); // 1 CHR page (8KB)
        rom.push(0x00); // mapper 0, horizontal mirroring
        rom.push(0x00); // mapper 0
        rom.extend_from_slice(&[0; 8]); // padding
        rom.extend(std::iter::repeat_n(0, 16 * 1024)); // PRG ROM
        rom.extend(std::iter::repeat_n(0, 8 * 1024)); // CHR ROM
        INesFile::new(rom).unwrap()
    }

    #[test]
    fn test_create_mcu() {
        let file = make_test_ines();
        let _mcu = create_mcu(&file);
    }

    #[test]
    fn test_create_machine() {
        let file = make_test_ines();
        let _machine = create_machine(&file);
    }

    #[test]
    fn test_create_machine_with_renderer() {
        let file = make_test_ines();
        let _machine = create_machine_with_renderer(&file, None);
    }
}

#[macro_use]
mod macros {
    #[macro_export]
    macro_rules! to_from_u8 {
        ($t: ty) => {
            impl From<$t> for u8 {
                fn from(n: $t) -> Self {
                    n.into_bytes()[0]
                }
            }

            impl From<u8> for $t {
                fn from(v: u8) -> Self {
                    <$t>::from_bytes([v])
                }
            }
        };
    }
}
