pub struct DirectChr {
    pub data: [u8; 0x2000],
}

impl DirectChr {
    pub fn empty() -> Self {
        Self { data: [0; 0x2000] }
    }

    pub fn from_chr_rom(chr_rom: &[u8]) -> Self {
        let mut data = [0; 0x2000];
        let len = chr_rom.len().min(0x2000);
        data[..len].copy_from_slice(&chr_rom[..len]);
        Self { data }
    }

    pub fn read_chr(&self, address: u16) -> u8 {
        self.data[address as usize % self.data.len()]
    }

    pub fn write_chr(&mut self, address: u16, value: u8) {
        self.data[address as usize % self.data.len()] = value;
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn direct_chr_reads_stored_value() {
        let mut chr = DirectChr::empty();
        chr.write_chr(0, 0x42);
        assert_eq!(chr.read_chr(0), 0x42);
    }

    #[test]
    fn direct_chr_wraps_at_0x2000_boundary() {
        let mut chr = DirectChr::empty();
        chr.write_chr(0, 0xaa);
        assert_eq!(chr.read_chr(0x2000), 0xaa);
        assert_eq!(chr.read_chr(0x4000), 0xaa);
    }

    #[test]
    fn direct_chr_constructs_from_8kb_slice() {
        let mut src = [0u8; 0x2000];
        src[0] = 0x11;
        src[0x1fff] = 0x22;
        let mut chr = DirectChr::empty();
        for (i, &b) in src.iter().enumerate() {
            chr.write_chr(i as u16, b);
        }
        assert_eq!(chr.read_chr(0), 0x11);
        assert_eq!(chr.read_chr(0x1fff), 0x22);
    }
}
