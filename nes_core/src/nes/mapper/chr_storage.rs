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

pub struct Mmc1ChrStorage {
    source: Vec<u8>,
    has_chr_ram: bool,
    chr_bank0: u8,
    chr_bank1: u8,
    chr_in_4k: bool,
    shift_register: u8,
}

impl Mmc1ChrStorage {
    const INITIAL_SHIFT_REGISTER: u8 = 0x10;

    pub fn new(chr_rom: &[u8]) -> Self {
        let has_chr_ram = chr_rom.is_empty();
        let size = if has_chr_ram { 0x2000 } else { chr_rom.len() };
        let mut source = vec![0; size];
        source[..chr_rom.len()].copy_from_slice(chr_rom);
        Self {
            source,
            has_chr_ram,
            chr_bank0: 0,
            chr_bank1: 0,
            chr_in_4k: false,
            shift_register: Self::INITIAL_SHIFT_REGISTER,
        }
    }

    fn bank_count_4k(&self) -> usize {
        if self.source.is_empty() { 1 } else { self.source.len() / 0x1000 }
    }

    fn chr_offset(&self, address: u16) -> usize {
        let offset = address as usize % 0x2000;
        let upper = offset >= 0x1000;
        if self.chr_in_4k {
            let bank = if upper { self.chr_bank1 } else { self.chr_bank0 };
            (bank as usize % self.bank_count_4k()) * 0x1000 + (offset % 0x1000)
        } else {
            let bank = usize::from(self.chr_bank0 & 0x1e) % self.bank_count_4k();
            let bank = if upper { (bank + 1) % self.bank_count_4k() } else { bank };
            bank * 0x1000 + (offset % 0x1000)
        }
    }
}

impl ChrStorage for Mmc1ChrStorage {
    fn read_chr(&self, address: u16) -> u8 {
        let src_offset = self.chr_offset(address);
        self.source[src_offset % self.source.len()]
    }

    fn write_chr(&mut self, address: u16, value: u8) {
        if !self.has_chr_ram {
            return;
        }
        let src_offset = self.chr_offset(address);
        let len = self.source.len();
        self.source[src_offset % len] = value;
    }

    fn write_register(&mut self, addr: u16, value: u8) {
        if !(0x8000..=0xffff).contains(&addr) {
            return;
        }

        if value & 0x80 != 0 {
            self.shift_register = Self::INITIAL_SHIFT_REGISTER;
            return;
        }

        let ready = self.shift_register & 0x01 != 0;
        self.shift_register >>= 1;
        self.shift_register |= (value & 0x01) << 4;

        if ready {
            let reg_value = self.shift_register & 0x1f;
            match addr {
                0x8000..=0x9fff => {
                    self.chr_in_4k = (reg_value & 0x10) != 0;
                }
                0xa000..=0xbfff => {
                    self.chr_bank0 = reg_value;
                }
                0xc000..=0xdfff => {
                    self.chr_bank1 = reg_value;
                }
                _ => {}
            }
            self.shift_register = Self::INITIAL_SHIFT_REGISTER;
        }
    }
}

#[cfg(test)]
mod mmc1_chr_tests {
    use super::*;

    fn make_chr() -> Vec<u8> {
        let mut data = vec![0u8; 0x8000];
        data[0x0000] = 0x10;
        data[0x1000] = 0x20;
        data[0x2000] = 0x30;
        data[0x3000] = 0x40;
        data
    }

    fn write_serial(chr: &mut Mmc1ChrStorage, address: u16, value: u8) {
        for bit in 0..5 {
            chr.write_register(address, (value >> bit) & 0x01);
        }
    }

    #[test]
    fn initial_state_reads_bank_0() {
        let chr = Mmc1ChrStorage::new(&make_chr());
        assert_eq!(chr.read_chr(0x0000), 0x10);
    }

    #[test]
    fn select_chr_bank0_in_4k_mode() {
        let mut chr = Mmc1ChrStorage::new(&make_chr());
        write_serial(&mut chr, 0x8000, 0x10);
        write_serial(&mut chr, 0xa000, 0x03);
        assert_eq!(chr.read_chr(0x0000), 0x40);
    }

    #[test]
    fn select_chr_bank1_in_4k_mode() {
        let mut chr = Mmc1ChrStorage::new(&make_chr());
        write_serial(&mut chr, 0x8000, 0x10);
        write_serial(&mut chr, 0xc000, 0x01);
        assert_eq!(chr.read_chr(0x1000), 0x20);
    }

    #[test]
    fn writes_to_chr_ram_persists() {
        let mut chr = Mmc1ChrStorage::new(&[]);
        chr.write_chr(0x0000, 0xab);
        assert_eq!(chr.read_chr(0x0000), 0xab);
    }
}

pub struct Vrc24ChrStorage {
    source: Vec<u8>,
    has_chr_ram: bool,
    s0: u8,
    s1: u8,
    is_vrc4: bool,
    chr_shift_low_bit: bool,
    chrs: [u8; 16],
    bank_offsets: [usize; 8],
}

impl Vrc24ChrStorage {
    const BANK_SIZE: usize = 0x0400;

    pub fn new(chr_rom: &[u8], s0: u8, s1: u8, is_vrc4: bool, chr_shift_low_bit: bool) -> Self {
        let has_chr_ram = chr_rom.is_empty();
        let size = if has_chr_ram { 0x2000 } else { chr_rom.len() };
        let mut source = vec![0; size];
        source[..chr_rom.len()].copy_from_slice(chr_rom);
        let mut storage = Self {
            source,
            has_chr_ram,
            s0,
            s1,
            is_vrc4,
            chr_shift_low_bit,
            chrs: [0; 16],
            bank_offsets: [0; 8],
        };
        storage.refresh_banks();
        storage
    }

    fn compute_register_index(&self, address: u16) -> usize {
        let bit0 = ((address >> self.s0) & 1) as usize;
        let bit1 = ((address >> self.s1) & 1) as usize;
        bit1 << 1 | bit0
    }

    fn bank_count(&self) -> usize {
        if self.source.is_empty() { 1 } else { self.source.len() / Self::BANK_SIZE }
    }

    fn refresh_banks(&mut self) {
        for slot in 0..8 {
            let lo = self.chrs[slot * 2] as u16;
            let hi = self.chrs[slot * 2 + 1] as u16;
            let mut bank = lo | (hi << 4);
            if self.is_vrc4 {
                bank &= 0x1ff;
            } else {
                bank &= 0x0ff;
            }
            if self.chr_shift_low_bit {
                bank >>= 1;
            }
            self.bank_offsets[slot] = (bank as usize % self.bank_count()) * Self::BANK_SIZE;
        }
    }
}

impl ChrStorage for Vrc24ChrStorage {
    fn read_chr(&self, address: u16) -> u8 {
        let addr = address as usize % 0x2000;
        let slot = addr / Self::BANK_SIZE;
        let offset = addr % Self::BANK_SIZE;
        let src = self.bank_offsets[slot] + offset;
        let len = self.source.len();
        self.source[src % len]
    }

    fn write_chr(&mut self, address: u16, value: u8) {
        if !self.has_chr_ram {
            return;
        }
        let addr = address as usize % 0x2000;
        let slot = addr / Self::BANK_SIZE;
        let offset = addr % Self::BANK_SIZE;
        let src = self.bank_offsets[slot] + offset;
        let len = self.source.len();
        self.source[src % len] = value;
    }

    fn write_register(&mut self, addr: u16, value: u8) {
        match addr {
            0xb000..=0xefff => {
                let group = ((addr >> 12) & 0x0f) as usize;
                let idx = self.compute_register_index(addr);
                let slot = (group - 0xb) * 2 + (idx >> 1);

                if idx & 1 == 0 {
                    self.chrs[slot * 2] = value & 0x0f;
                } else {
                    self.chrs[slot * 2 + 1] = value & 0x1f;
                }
                self.refresh_banks();
            }
            _ => {}
        }
    }
}

#[cfg(test)]
mod vrc24_chr_tests {
    use super::*;

    fn make_chr() -> Vec<u8> {
        let bank_count = 32;
        let mut data = vec![0u8; bank_count * 0x0400];
        for bank in 0..bank_count {
            data[bank * 0x0400] = bank as u8;
        }
        data
    }

    #[test]
    fn vrc4f_banking() {
        let mut chr = Vrc24ChrStorage::new(&make_chr(), 0, 1, true, false);
        chr.write_register(0xb000, 0x02);
        assert_eq!(chr.read_chr(0x0000), 0x02);
    }

    #[test]
    fn vrc4f_high_nibble() {
        let mut chr = Vrc24ChrStorage::new(&make_chr(), 0, 1, true, false);
        chr.write_register(0xb001, 0x01);
        assert_eq!(chr.read_chr(0x0000), 0x10);
    }

    #[test]
    fn writes_to_chr_ram() {
        let mut chr = Vrc24ChrStorage::new(&[], 0, 1, true, false);
        chr.write_chr(0x0000, 0xab);
        assert_eq!(chr.read_chr(0x0000), 0xab);
    }
}
