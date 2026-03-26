use crate::ines::INesFile;
use crate::nes::apu::AudioDriver;
use crate::render::Render;

pub mod apu;
pub mod controller;
mod lower_ram;
mod mapper;
pub mod nes_mcu;
pub mod ppu;

pub use nes_mcu::NesMcu;

pub fn create_mcu(file: &INesFile) -> NesMcu {
    nes_mcu::build(file)
}

/// Create an MCU with a custom renderer
///
/// This allows injecting different rendering backends (image, markdown, composite, etc.)
///
/// # Parameters
/// - `file`: The iNES file to load
/// - `renderer`: Optional custom renderer. If None, uses default ImageRender.
pub fn create_mcu_with_renderer(file: &INesFile, renderer: Option<Box<dyn Render>>) -> NesMcu {
    nes_mcu::build_with_renderer(file, renderer)
}

pub fn create_mcu_with_renderer_and_audio(
    file: &INesFile,
    renderer: Option<Box<dyn Render>>,
    audio_driver: Box<dyn AudioDriver>,
) -> NesMcu {
    nes_mcu::build_with_renderer_and_audio(file, renderer, audio_driver)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_create_mcu() {
        // Create a minimal valid iNES file for testing
        let mut rom = Vec::new();

        // NES signature
        rom.extend_from_slice(&[0x4e, 0x45, 0x53, 0x1a]);

        // PRG ROM pages (1 page = 16KB)
        rom.push(1);

        // CHR ROM pages (1 page = 8KB)
        rom.push(1);

        // Control byte 1: mapper in bits 7-4, flags in bits 3-0
        rom.push(0x00); // Mapper 0, horizontal mirroring

        // Control byte 2: mapper upper bits in bits 7-4
        rom.push(0x00); // Mapper 0

        // 8 bytes of padding
        rom.extend_from_slice(&[0; 8]);

        // PRG ROM data (16KB)
        rom.extend(std::iter::repeat_n(0, 16 * 1024));

        // CHR ROM data (8KB)
        rom.extend(std::iter::repeat_n(0, 8 * 1024));

        let file = INesFile::new(rom).unwrap();
        let _mcu = create_mcu(&file);
        // If we got here without panicking, the test passed
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
