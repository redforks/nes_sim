use crate::ines::INesFile;
use nom::bits::streaming::take as bit_take;
use nom::error::Error as NomError;
use nom::{IResult, Parser, bits::bits, bytes};

const NES_SIGNATURE: &[u8] = &[0x4e, 0x45, 0x53, 0x1a];

pub fn check_signature(image: &[u8]) -> bool {
    bytes::complete::tag::<&[u8], _, NomError<&[u8]>>(NES_SIGNATURE)
        .parse(image)
        .is_ok()
}

fn parse_header(input: &[u8]) -> IResult<&[u8], super::Header> {
    let (input, n_prg) = nom::number::streaming::u8.parse(input)?;
    let (input, n_chr) = nom::number::streaming::u8.parse(input)?;
    let (
        input,
        (
            mapper_low,
            ignore_mirror_control,
            has_trainer,
            battery_backed_ram,
            ver_or_hor_arrangement,
        ),
    ): (&[u8], (u8, u8, u8, u8, u8)) = bits::<_, _, NomError<(&[u8], usize)>, _, _>((
        bit_take(4usize),
        bit_take(1usize),
        bit_take(1usize),
        bit_take(1usize),
        bit_take(1usize),
    ))
    .parse(input)?;

    let (input, (mapper_high, _)): (&[u8], (u8, u8)) =
        bits::<_, _, NomError<(&[u8], usize)>, _, _>((bit_take(4usize), bit_take(4usize)))
            .parse(input)?;

    Ok((
        input,
        super::Header {
            n_prg_pages: n_prg,
            n_chr_pages: n_chr,
            mapper_no: mapper_low | (mapper_high << 4),
            ignore_mirror_control: ignore_mirror_control != 0,
            has_trainer: has_trainer != 0,
            battery_backed_ram: battery_backed_ram != 0,
            ver_or_hor_arrangement: ver_or_hor_arrangement != 0,
        },
    ))
}

fn _parse_file(buf: &[u8]) -> IResult<&[u8], INesFile> {
    let (buf, _) = bytes::complete::tag(NES_SIGNATURE).parse(buf)?;
    let (buf, header) = parse_header(buf)?;
    // skip 8 bytes
    let (buf, _) = bytes::complete::take(8usize).parse(buf)?;
    // read 16 * header.n_prg_pages bytes
    let (buf, prg) = bytes::complete::take(16 * 1024 * header.n_prg_pages as usize).parse(buf)?;
    // read 8 * header.n_chr_pages bytes
    let (buf, chr) = bytes::complete::take(8 * 1024 * header.n_chr_pages as usize).parse(buf)?;

    Ok((
        buf,
        INesFile {
            header,
            prg_rom: prg.to_vec(),
            chr_rom: chr.to_vec(),
        },
    ))
}

pub fn parse_file(buf: &[u8]) -> Result<INesFile, nom::error::Error<&[u8]>> {
    let result = nom::combinator::complete(_parse_file).parse(buf);
    match result {
        Ok((_, file)) => Ok(file),
        Err(err) => match err {
            nom::Err::Incomplete(_) => unreachable!(),
            nom::Err::Error(err) => Err(err),
            nom::Err::Failure(err) => Err(err),
        },
    }
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
