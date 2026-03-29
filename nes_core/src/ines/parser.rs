use crate::ines::INesFile;
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

fn parse_header(input: &mut &[u8]) -> Result<super::Header, ContextError> {
    let n_prg = u8.parse_next(input)?;
    let n_chr = u8.parse_next(input)?;
    let flags6 = u8.parse_next(input)?;
    let flags7 = u8.parse_next(input)?;

    let mapper_low = flags6 >> 4;
    let mapper_high = flags7 >> 4;

    Ok(super::Header {
        n_prg_pages: n_prg,
        n_chr_pages: n_chr,
        mapper_no: mapper_low | (mapper_high << 4),
        ignore_mirror_control: flags6 & 0b0000_1000 != 0,
        has_trainer: flags6 & 0b0000_0100 != 0,
        battery_backed_ram: flags6 & 0b0000_0010 != 0,
        ver_or_hor_arrangement: flags6 & 0b0000_0001 != 0,
    })
}

fn _parse_file(input: &mut &[u8]) -> Result<INesFile, ContextError> {
    let mut signature = literal::<_, _, ContextError>(NES_SIGNATURE);
    signature.parse_next(input)?;
    let header = parse_header(input)?;
    // skip 8 bytes
    take(8usize).void().parse_next(input)?;
    // read 16 * header.n_prg_pages bytes
    let prg = take(16 * 1024 * header.n_prg_pages as usize).parse_next(input)?;
    // read 8 * header.n_chr_pages bytes
    let chr = take(8 * 1024 * header.n_chr_pages as usize).parse_next(input)?;

    Ok(INesFile {
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
    }
}
