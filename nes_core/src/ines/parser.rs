use crate::ines::{FileVersion, INesFile};
use winnow::binary::u8;
use winnow::error::ContextError;
use winnow::prelude::*;
use winnow::token::{literal, take};

const NES_SIGNATURE: &[u8] = &[0x4e, 0x45, 0x53, 0x1a];

pub fn check_signature(image: &[u8]) -> bool {
    literal::<_, _, ContextError>(NES_SIGNATURE)
        .parse_peek(image)
        .is_ok()
}

fn is_nes20(flags7: u8) -> bool {
    flags7 & 0x0c == 0x08
}

fn decode_shift_count(shift: u8) -> Option<usize> {
    if shift == 0 {
        None
    } else {
        Some(64usize << shift)
    }
}

fn decode_rom_size(lsb: u8, msb_nibble: u8, unit_size: usize) -> usize {
    if msb_nibble != 0x0f {
        (((msb_nibble as usize) << 8) | lsb as usize) * unit_size
    } else {
        let exponent = (lsb >> 2) as usize;
        let multiplier = ((lsb & 0b11) as usize) * 2 + 1;
        (1usize << exponent) * multiplier
    }
}

fn parse_header(input: &mut &[u8]) -> Result<(FileVersion, super::Header), ContextError> {
    let n_prg = u8.parse_next(input)?;
    let n_chr = u8.parse_next(input)?;
    let flags6 = u8.parse_next(input)?;
    let flags7 = u8.parse_next(input)?;
    let flags8 = u8.parse_next(input)?;
    let flags9 = u8.parse_next(input)?;
    let flags10 = u8.parse_next(input)?;
    let flags11 = u8.parse_next(input)?;
    let flags12 = u8.parse_next(input)?;
    let flags13 = u8.parse_next(input)?;
    let flags14 = u8.parse_next(input)?;
    let flags15 = u8.parse_next(input)?;

    let mapper_low = flags6 >> 4;
    let mapper_high = flags7 >> 4;
    let nes20 = is_nes20(flags7);
    let mapper_no = if nes20 {
        mapper_low as u16 | ((mapper_high as u16) << 4) | (((flags8 & 0x0f) as u16) << 8)
    } else {
        mapper_low as u16 | ((mapper_high as u16) << 4)
    };
    let prg_rom_size = if nes20 {
        decode_rom_size(n_prg, flags9 & 0x0f, 16 * 1024)
    } else {
        n_prg as usize * 16 * 1024
    };
    let chr_rom_size = if nes20 {
        decode_rom_size(n_chr, flags9 >> 4, 8 * 1024)
    } else {
        n_chr as usize * 8 * 1024
    };
    let console_type = flags7 & 0b11;

    Ok((
        if nes20 {
            FileVersion::Nes20
        } else {
            FileVersion::INes
        },
        super::Header {
            n_prg_pages: n_prg,
            n_chr_pages: n_chr,
            prg_rom_size,
            chr_rom_size,
            mapper_no,
            submapper_no: nes20.then_some(flags8 >> 4),
            ignore_mirror_control: flags6 & 0b0000_1000 != 0,
            has_trainer: flags6 & 0b0000_0100 != 0,
            battery_backed_ram: flags6 & 0b0000_0010 != 0,
            nametable_arrangement: if flags6 & 0b0000_0001 == 0 {
                super::NametableArrangement::Vertical
            } else {
                super::NametableArrangement::Horizontal
            },
            console_type,
            prg_ram_size: nes20.then(|| decode_shift_count(flags10 & 0x0f)).flatten(),
            prg_nvram_size: nes20.then(|| decode_shift_count(flags10 >> 4)).flatten(),
            chr_ram_size: nes20.then(|| decode_shift_count(flags11 & 0x0f)).flatten(),
            chr_nvram_size: nes20.then(|| decode_shift_count(flags11 >> 4)).flatten(),
            timing_mode: nes20.then_some(flags12 & 0b11),
            vs_ppu_type: (nes20 && console_type == 1).then_some(flags13 & 0x0f),
            vs_hardware_type: (nes20 && console_type == 1).then_some(flags13 >> 4),
            extended_console_type: (nes20 && console_type == 3).then_some(flags13 & 0x0f),
            misc_rom_count: nes20.then_some(flags14 & 0b11),
            default_expansion_device: nes20.then_some(flags15 & 0x7f),
        },
    ))
}

fn _parse_file(input: &mut &[u8]) -> Result<INesFile, ContextError> {
    let mut signature = literal::<_, _, ContextError>(NES_SIGNATURE);
    signature.parse_next(input)?;
    let (file_version, header) = parse_header(input)?;
    if header.has_trainer {
        take(512usize).void().parse_next(input)?;
    }
    let prg = take(header.prg_rom_size).parse_next(input)?;
    let chr = take(header.chr_rom_size).parse_next(input)?;

    Ok(INesFile {
        file_version,
        header,
        prg_rom: prg.to_vec(),
        chr_rom: chr.to_vec(),
    })
}

pub fn parse_file(buf: &[u8]) -> Result<INesFile, ContextError> {
    let mut input = buf;
    _parse_file(&mut input)
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::vec;

    #[test]
    fn test_check_signature() {
        let image = vec![
            0x4e, 0x45, 0x53, 0x1a, 0x01, 0x01, 0x01, 0x01, 0x01, 0x01, 0x01, 0x01, 0x01, 0x01,
            0x01, 0x01,
        ];
        assert!(check_signature(&image));

        // not enough bytes
        let image = vec![0x4e, 0x45];
        assert!(!check_signature(&image));

        // invalid signature
        let image = vec![0x4e, 0x45, 0x53, 0x1b];
        assert!(!check_signature(&image));
    }

    #[test]
    fn test_parse_file() {
        let rom = include_bytes!("01-basics.nes");
        let f = parse_file(rom).unwrap();
        assert_eq!(2, f.header().n_prg_pages);
        assert_eq!(0, f.header().mapper_no);
        assert_eq!(32 * 1024, f.read_prg_rom().len());
        assert_eq!(FileVersion::INes, f.file_version());
    }
}
