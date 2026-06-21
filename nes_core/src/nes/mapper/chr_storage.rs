use super::ChrStorage;

pub struct DirectChr {
    data: [u8; 0x2000],
}

impl DirectChr {
    pub fn empty() -> Self {
        Self { data: [0; 0x2000] }
    }
}

impl ChrStorage for DirectChr {
    fn read_chr(&self, address: u16) -> u8 {
        self.data[address as usize % self.data.len()]
    }

    fn write_chr(&mut self, address: u16, value: u8) {
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

pub struct WindowedChr {
    source: Vec<u8>,
    cache: [u8; 0x2000],
}

impl WindowedChr {
    pub fn new(chr_mem: Vec<u8>) -> Self {
        Self {
            source: chr_mem,
            cache: [0; 0x2000],
        }
    }

    pub fn source_len(&self) -> usize {
        self.source.len()
    }

    pub fn refresh(&mut self, offset_into_source: usize) {
        let end = (offset_into_source + 0x2000).min(self.source.len());
        let len = end - offset_into_source;
        self.cache[..len].copy_from_slice(&self.source[offset_into_source..end]);
    }

    pub fn copy_from_source(&mut self, dest_offset: usize, source_offset: usize, len: usize) {
        self.cache[dest_offset..dest_offset + len]
            .copy_from_slice(&self.source[source_offset..source_offset + len]);
    }

    pub fn write_chr_with_source(&mut self, address: u16, value: u8, source_offset: usize) {
        self.cache[address as usize % self.cache.len()] = value;
        self.source[source_offset] = value;
    }
}

impl ChrStorage for WindowedChr {
    fn read_chr(&self, address: u16) -> u8 {
        self.cache[address as usize % self.cache.len()]
    }

    fn write_chr(&mut self, address: u16, value: u8) {
        self.cache[address as usize % self.cache.len()] = value;
    }
}

#[cfg(test)]
mod windowed_tests {
    use super::*;

    #[test]
    fn windowed_chr_writes_and_reads_cache() {
        let mut chr = WindowedChr::new(vec![0; 0x4000]);
        chr.write_chr(0x100, 0xab);
        assert_eq!(chr.read_chr(0x100), 0xab);
    }

    #[test]
    fn windowed_chr_write_through_survives_refresh() {
        let mut source = vec![0; 0x4000];
        source[0x200] = 0xff;
        let mut chr = WindowedChr::new(source);
        chr.write_chr_with_source(0x100, 0xab, 0x200);
        assert_eq!(chr.read_chr(0x100), 0xab);
        chr.refresh(0x0000);
        assert_eq!(chr.read_chr(0x200), 0xab);
    }

    #[test]
    fn windowed_chr_refresh_copies_source_to_cache() {
        let mut source = vec![0; 0x4000];
        source[0x2100] = 0xcd;
        let mut chr = WindowedChr::new(source);
        chr.refresh(0x2000);
        assert_eq!(chr.read_chr(0x100), 0xcd);
    }

    #[test]
    fn windowed_chr_copy_from_source_copies_partial() {
        let mut source = vec![0; 0x4000];
        source[0x0a00] = 0x11;
        source[0x0b00] = 0x22;
        let mut chr = WindowedChr::new(source);
        chr.copy_from_source(0x0000, 0x0a00, 0x400);
        chr.copy_from_source(0x0400, 0x0b00, 0x400);
        assert_eq!(chr.read_chr(0x000), 0x11);
        assert_eq!(chr.read_chr(0x400), 0x22);
    }

    #[test]
    fn windowed_chr_source_len() {
        let chr = WindowedChr::new(vec![0; 0x8000]);
        assert_eq!(chr.source_len(), 0x8000);
    }
}
