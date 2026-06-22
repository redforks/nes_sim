use super::ChrStorage;

pub struct DirectChr {
    data: [u8; 0x2000],
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

pub struct CnromChrStorage {
    inner: WindowedChr,
    bank_count: usize,
}

impl CnromChrStorage {
    pub fn new(chr_rom: &[u8]) -> Self {
        let bank_count = chr_rom.len() / 0x2000;
        let mut inner = WindowedChr::new(chr_rom.to_vec());
        inner.refresh(0);
        Self { inner, bank_count }
    }
}

impl ChrStorage for CnromChrStorage {
    fn read_chr(&self, address: u16) -> u8 {
        self.inner.read_chr(address)
    }

    fn write_chr(&mut self, address: u16, value: u8) {
        self.inner.write_chr(address, value);
    }

    fn write_register(&mut self, _addr: u16, value: u8) {
        let bank = (value as usize) % self.bank_count;
        self.inner.refresh(bank * 0x2000);
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

#[cfg(test)]
mod cnrom_tests {
    use super::*;

    fn make_chr() -> Vec<u8> {
        let mut data = vec![0u8; 0x4000];
        data[0] = 0x10;
        data[0x2000] = 0x20;
        data
    }

    #[test]
    fn initial_state_reads_bank_0() {
        let chr = CnromChrStorage::new(&make_chr());
        assert_eq!(chr.read_chr(0), 0x10);
    }

    #[test]
    fn write_register_switches_bank() {
        let mut chr = CnromChrStorage::new(&make_chr());
        chr.write_register(0x8000, 1);
        assert_eq!(chr.read_chr(0), 0x20);
    }

    #[test]
    fn bank_wraps_at_bank_count() {
        let mut chr = CnromChrStorage::new(&make_chr());
        chr.write_register(0x8000, 2);
        assert_eq!(chr.read_chr(0), 0x10);
    }
}

pub struct Mmc3ChrStorage {
    source: Vec<u8>,
    has_chr_ram: bool,
    bank_select: u8,
    bank_registers: [u8; 8],
    bank_offsets: [usize; 8],
}

impl Mmc3ChrStorage {
    const BANK_SIZE: usize = 0x0400;
    const WINDOW_SIZE: usize = 0x2000;

    pub fn new(chr_rom: &[u8]) -> Self {
        let has_chr_ram = chr_rom.is_empty();
        let size = if has_chr_ram { Self::WINDOW_SIZE } else { chr_rom.len() };
        let mut source = vec![0; size];
        source[..chr_rom.len()].copy_from_slice(chr_rom);
        let mut storage = Self {
            source,
            has_chr_ram,
            bank_select: 0,
            bank_registers: [0; 8],
            bank_offsets: [0; 8],
        };
        storage.sync_banks();
        storage
    }

    fn bank_count(&self) -> usize {
        self.source.len() / Self::BANK_SIZE
    }

    fn normalize_bank(&self, bank: u8) -> usize {
        if self.bank_count() == 0 { 0 } else { (bank as usize) % self.bank_count() }
    }

    fn sync_banks(&mut self) {
        let chr_mode = (self.bank_select & 0x80) != 0;
        let r0_even = self.normalize_bank(self.bank_registers[0] & !1);
        let r0_odd = self.normalize_bank(self.bank_registers[0] | 1);
        let r1_even = self.normalize_bank(self.bank_registers[1] & !1);
        let r1_odd = self.normalize_bank(self.bank_registers[1] | 1);
        let r2 = self.normalize_bank(self.bank_registers[2]);
        let r3 = self.normalize_bank(self.bank_registers[3]);
        let r4 = self.normalize_bank(self.bank_registers[4]);
        let r5 = self.normalize_bank(self.bank_registers[5]);

        let banks = if chr_mode {
            [r2, r3, r4, r5, r0_even, r0_odd, r1_even, r1_odd]
        } else {
            [r0_even, r0_odd, r1_even, r1_odd, r2, r3, r4, r5]
        };

        for (slot, bank) in banks.into_iter().enumerate() {
            self.bank_offsets[slot] = bank * Self::BANK_SIZE;
        }
    }
}

impl ChrStorage for Mmc3ChrStorage {
    fn read_chr(&self, address: u16) -> u8 {
        let addr = address as usize % Self::WINDOW_SIZE;
        let slot = addr / Self::BANK_SIZE;
        let offset = addr % Self::BANK_SIZE;
        let source_addr = self.bank_offsets[slot] + offset;
        let len = self.source.len();
        self.source[source_addr % len]
    }

    fn write_chr(&mut self, address: u16, value: u8) {
        if !self.has_chr_ram {
            return;
        }
        let addr = address as usize % Self::WINDOW_SIZE;
        let slot = addr / Self::BANK_SIZE;
        let offset = addr % Self::BANK_SIZE;
        let source_addr = self.bank_offsets[slot] + offset;
        let len = self.source.len();
        self.source[source_addr % len] = value;
    }

    fn write_register(&mut self, addr: u16, value: u8) {
        match addr {
            0x8000..=0x9fff if addr & 0x01 == 0 => {
                self.bank_select = value;
            }
            0x8000..=0x9fff => {
                let target = (self.bank_select & 0x07) as usize;
                if target < 6 {
                    self.bank_registers[target] = value;
                    self.sync_banks();
                }
            }
            _ => {}
        }
    }
}

#[cfg(test)]
mod mmc3_chr_tests {
    use super::*;

    fn create_chr() -> Vec<u8> {
        let mut chr = vec![0u8; Mmc3ChrStorage::BANK_SIZE * 16];
        for bank in 0..16 {
            chr[bank * Mmc3ChrStorage::BANK_SIZE] = bank as u8;
        }
        chr
    }

    #[test]
    fn switches_chr_layout_in_chr_mode_0() {
        let chr_data = create_chr();
        let mut chr = Mmc3ChrStorage::new(&chr_data);

        chr.write_register(0x8000, 0x00);
        chr.write_register(0x8001, 0x06);
        chr.write_register(0x8000, 0x01);
        chr.write_register(0x8001, 0x08);
        chr.write_register(0x8000, 0x02);
        chr.write_register(0x8001, 0x0a);
        chr.write_register(0x8000, 0x03);
        chr.write_register(0x8001, 0x0b);
        chr.write_register(0x8000, 0x04);
        chr.write_register(0x8001, 0x0c);
        chr.write_register(0x8000, 0x05);
        chr.write_register(0x8001, 0x0d);

        assert_eq!(chr.read_chr(0x0000), 0x06);
        assert_eq!(chr.read_chr(0x0400), 0x07);
        assert_eq!(chr.read_chr(0x0800), 0x08);
        assert_eq!(chr.read_chr(0x0c00), 0x09);
        assert_eq!(chr.read_chr(0x1000), 0x0a);
        assert_eq!(chr.read_chr(0x1400), 0x0b);
        assert_eq!(chr.read_chr(0x1800), 0x0c);
        assert_eq!(chr.read_chr(0x1c00), 0x0d);
    }

    #[test]
    fn switches_chr_layout_in_chr_mode_1() {
        let chr_data = create_chr();
        let mut chr = Mmc3ChrStorage::new(&chr_data);

        chr.write_register(0x8000, 0x80);
        chr.write_register(0x8001, 0x06);
        chr.write_register(0x8000, 0x81);
        chr.write_register(0x8001, 0x08);
        chr.write_register(0x8000, 0x82);
        chr.write_register(0x8001, 0x0a);
        chr.write_register(0x8000, 0x83);
        chr.write_register(0x8001, 0x0b);
        chr.write_register(0x8000, 0x84);
        chr.write_register(0x8001, 0x0c);
        chr.write_register(0x8000, 0x85);
        chr.write_register(0x8001, 0x0d);

        assert_eq!(chr.read_chr(0x0000), 0x0a);
        assert_eq!(chr.read_chr(0x0400), 0x0b);
        assert_eq!(chr.read_chr(0x0800), 0x0c);
        assert_eq!(chr.read_chr(0x0c00), 0x0d);
        assert_eq!(chr.read_chr(0x1000), 0x06);
        assert_eq!(chr.read_chr(0x1400), 0x07);
        assert_eq!(chr.read_chr(0x1800), 0x08);
        assert_eq!(chr.read_chr(0x1c00), 0x09);
    }

    #[test]
    fn writes_to_chr_ram_through_current_mapping() {
        let mut chr = Mmc3ChrStorage::new(&[]);

        chr.write_register(0x8000, 0x02);
        chr.write_register(0x8001, 0x03);
        chr.write_chr(0x1000, 0xaa);

        assert_eq!(chr.read_chr(0x1000), 0xaa);
    }
}
