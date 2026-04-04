use super::CARTRIDGE_START_ADDR;
use crate::nes::ppu::{BackgroundTileOverride, PatternAccess};

const PRG_BANK_SIZE_8K: usize = 0x2000;
const CHR_WINDOW_SIZE: usize = 0x2000;
const CHR_BANK_SIZE_1K: usize = 0x0400;
const EXRAM_SIZE: usize = 0x0400;
const CIRAM_SIZE: usize = 0x0800;

pub struct MMC5 {
    prg_rom: Vec<u8>,
    prg_ram: Vec<u8>,
    chr_mem: Vec<u8>,
    cpu_chr_view: [u8; CHR_WINDOW_SIZE],
    has_chr_ram: bool,

    ciram: [u8; CIRAM_SIZE],
    exram: [u8; EXRAM_SIZE],

    prg_mode: u8,
    chr_mode: u8,
    prg_ram_protect1: u8,
    prg_ram_protect2: u8,
    exram_mode: u8,
    nt_mapping: [u8; 4],
    fill_tile: u8,
    fill_color: u8,

    prg_regs: [u8; 5],
    sprite_chr_regs: [u8; 8],
    bg_chr_regs: [u8; 4],
    upper_chr_bits: u8,
    last_cpu_chr_bg: bool,

    ppu_sprite_8x16: bool,
    substitutions_enabled: bool,

    split_control: u8,
    split_scroll: u8,
    split_bank: u8,

    irq_scanline_compare: u8,
    irq_enabled: bool,
    irq_pending: bool,
    in_frame: bool,
    current_scanline: u8,

    multiplier_a: u8,
    multiplier_b: u8,
}

impl MMC5 {
    pub fn new(
        prg_rom: &[u8],
        chr_rom: &[u8],
        prg_ram_size: Option<usize>,
        _ignore_mirror_control: bool,
    ) -> Self {
        let has_chr_ram = chr_rom.is_empty();
        let chr_size = if has_chr_ram {
            CHR_WINDOW_SIZE
        } else {
            chr_rom.len()
        };
        let mut chr_mem = vec![0; chr_size.max(CHR_WINDOW_SIZE)];
        chr_mem[..chr_rom.len()].copy_from_slice(chr_rom);

        let ram_size = prg_ram_size.unwrap_or(0x10000).max(PRG_BANK_SIZE_8K);
        let mut mmc5 = Self {
            prg_rom: prg_rom.to_vec(),
            prg_ram: vec![0; ram_size],
            chr_mem,
            cpu_chr_view: [0; CHR_WINDOW_SIZE],
            has_chr_ram,
            ciram: [0; CIRAM_SIZE],
            exram: [0; EXRAM_SIZE],
            prg_mode: 3,
            chr_mode: 0,
            prg_ram_protect1: 0x03,
            prg_ram_protect2: 0x03,
            exram_mode: 0x03,
            nt_mapping: [0; 4],
            fill_tile: 0,
            fill_color: 0,
            prg_regs: [0, 0, 0, 0, 0xff],
            sprite_chr_regs: [0; 8],
            bg_chr_regs: [0; 4],
            upper_chr_bits: 0,
            last_cpu_chr_bg: false,
            ppu_sprite_8x16: false,
            substitutions_enabled: false,
            split_control: 0,
            split_scroll: 0,
            split_bank: 0,
            irq_scanline_compare: 0,
            irq_enabled: false,
            irq_pending: false,
            in_frame: false,
            current_scanline: 0,
            multiplier_a: 0,
            multiplier_b: 0,
        };
        mmc5.sync_cpu_chr_view();
        mmc5
    }

    #[cfg(test)]
    pub fn pattern_ref(&self) -> &[u8] {
        &self.cpu_chr_view
    }

    pub fn write_pattern(&mut self, address: u16, value: u8) {
        if !self.has_chr_ram {
            return;
        }

        let chr_addr = self.map_chr_addr(address, PatternAccess::Cpu);
        self.chr_mem[chr_addr] = value;
        self.sync_cpu_chr_view();
    }

    pub fn read(&mut self, address: u16) -> u8 {
        match address {
            CARTRIDGE_START_ADDR..=0x4fff => 0,
            0x5000..=0x5015 => 0,
            0x5100 => self.prg_mode,
            0x5101 => self.chr_mode,
            0x5102 => self.prg_ram_protect1,
            0x5103 => self.prg_ram_protect2,
            0x5104 => self.exram_mode,
            0x5105 => {
                self.nt_mapping[0]
                    | (self.nt_mapping[1] << 2)
                    | (self.nt_mapping[2] << 4)
                    | (self.nt_mapping[3] << 6)
            }
            0x5106 => self.fill_tile,
            0x5107 => self.fill_color,
            0x5113..=0x5117 => self.prg_regs[(address - 0x5113) as usize],
            0x5120..=0x5127 => self.sprite_chr_regs[(address - 0x5120) as usize],
            0x5128..=0x512b => self.bg_chr_regs[(address - 0x5128) as usize],
            0x5130 => self.upper_chr_bits,
            0x5204 => {
                let status = (u8::from(self.irq_pending) << 7) | (u8::from(self.in_frame) << 6);
                self.irq_pending = false;
                status
            }
            0x5205 => self.product() as u8,
            0x5206 => (self.product() >> 8) as u8,
            0x5c00..=0x5fff => self.read_exram_cpu(address),
            0x6000..=0xffff => self.read_prg(address),
            _ => 0,
        }
    }

    pub fn write(&mut self, address: u16, value: u8) {
        match address {
            CARTRIDGE_START_ADDR..=0x4fff => {}
            0x5000..=0x5015 => {}
            0x5100 => self.prg_mode = value & 0x03,
            0x5101 => {
                self.chr_mode = value & 0x03;
                self.sync_cpu_chr_view();
            }
            0x5102 => self.prg_ram_protect1 = value & 0x03,
            0x5103 => self.prg_ram_protect2 = value & 0x03,
            0x5104 => self.exram_mode = value & 0x03,
            0x5105 => {
                self.nt_mapping[0] = value & 0x03;
                self.nt_mapping[1] = (value >> 2) & 0x03;
                self.nt_mapping[2] = (value >> 4) & 0x03;
                self.nt_mapping[3] = (value >> 6) & 0x03;
            }
            0x5106 => self.fill_tile = value,
            0x5107 => self.fill_color = value & 0x03,
            0x5113..=0x5117 => self.prg_regs[(address - 0x5113) as usize] = value,
            0x5120..=0x5127 => {
                self.sprite_chr_regs[(address - 0x5120) as usize] = value;
                self.last_cpu_chr_bg = false;
                self.sync_cpu_chr_view();
            }
            0x5128..=0x512b => {
                self.bg_chr_regs[(address - 0x5128) as usize] = value;
                self.last_cpu_chr_bg = true;
                self.sync_cpu_chr_view();
            }
            0x5130 => {
                self.upper_chr_bits = value & 0x03;
                self.sync_cpu_chr_view();
            }
            0x5200 => self.split_control = value,
            0x5201 => self.split_scroll = value,
            0x5202 => self.split_bank = value,
            0x5203 => self.irq_scanline_compare = value,
            0x5204 => self.irq_enabled = (value & 0x80) != 0,
            0x5205 => self.multiplier_a = value,
            0x5206 => self.multiplier_b = value,
            0x5c00..=0x5fff => self.write_exram_cpu(address, value),
            0x6000..=0xffff => self.write_prg(address, value),
            _ => {}
        }
    }

    pub fn on_ppu_tick(&mut self, scanline: u16, dot: u16, rendering_enabled: bool) {
        if dot != 0 {
            return;
        }

        if rendering_enabled && scanline < 240 {
            self.in_frame = true;
            self.current_scanline = scanline as u8;
            if self.irq_scanline_compare != 0 && self.current_scanline == self.irq_scanline_compare
            {
                self.irq_pending = true;
            }
        } else {
            self.in_frame = false;
            self.current_scanline = 0;
            if scanline >= 240 {
                self.irq_pending = false;
            }
        }
    }

    pub fn irq_pending(&self) -> bool {
        self.irq_enabled && self.irq_pending
    }

    pub fn read_chr(&self, address: u16, access: PatternAccess) -> u8 {
        let chr_addr = self.map_chr_addr(address, access);
        self.chr_mem[chr_addr]
    }

    pub fn write_nametable(&mut self, address: u16, value: u8) {
        let (region, offset) = self.nametable_region(address);
        match self.nt_mapping[region] {
            0 | 1 => {
                let page = self.nt_mapping[region] as usize;
                self.ciram[page * 0x400 + offset] = value;
            }
            2 => {
                if self.exram_mode != 0x03 {
                    self.exram[offset] = value;
                }
            }
            3 => {}
            _ => unreachable!(),
        }
    }

    pub fn read_nametable(&self, address: u16) -> u8 {
        self.read_nametable_byte(address)
    }

    #[allow(clippy::too_many_arguments)]
    pub fn background_override(
        &self,
        screen_x: u8,
        screen_y: u8,
        nametable_addr: u16,
        tile_idx: u8,
        _palette_idx: u8,
        tile_fine_x: usize,
        tile_fine_y: usize,
    ) -> Option<BackgroundTileOverride> {
        if self.split_enabled() && self.in_split_region(screen_x) {
            return self.split_region_pixel(screen_x, screen_y, tile_fine_x);
        }

        if !self.extended_attribute_enabled() {
            return None;
        }

        let ex = self.exram[(nametable_addr as usize) & 0x03ff];
        let palette_idx = ex >> 6;
        let bank = (((self.upper_chr_bits as usize) & 0x03) << 6) | ((ex as usize) & 0x3f);
        let base = bank * 0x1000;
        let color_idx = self.read_pattern_pixel(base, tile_idx, tile_fine_x, tile_fine_y);
        Some(BackgroundTileOverride {
            palette_idx,
            color_idx,
        })
    }

    pub fn on_ppu_ctrl_write(&mut self, value: u8) {
        self.ppu_sprite_8x16 = (value & 0x20) != 0;
        self.sync_cpu_chr_view();
    }

    pub fn on_ppu_mask_write(&mut self, value: u8) {
        self.substitutions_enabled = (value & 0x18) != 0;
        self.sync_cpu_chr_view();
        if !self.substitutions_enabled {
            self.in_frame = false;
            self.current_scanline = 0;
            self.irq_pending = false;
        }
    }

    pub fn on_ppu_scroll_write(&mut self, _value: u8) {}

    pub fn on_oam_dma(&mut self) {
        self.current_scanline = 0;
        self.irq_pending = false;
    }

    fn product(&self) -> u16 {
        self.multiplier_a as u16 * self.multiplier_b as u16
    }

    fn prg_rom_bank_count(&self) -> usize {
        self.prg_rom.len() / PRG_BANK_SIZE_8K
    }

    fn prg_ram_bank_count(&self) -> usize {
        self.prg_ram.len() / PRG_BANK_SIZE_8K
    }

    fn chr_bank_count_1k(&self) -> usize {
        self.chr_mem.len() / CHR_BANK_SIZE_1K
    }

    fn prg_ram_write_enabled(&self) -> bool {
        self.prg_ram_protect1 == 0x02 && self.prg_ram_protect2 == 0x01
    }

    fn read_prg(&self, address: u16) -> u8 {
        match address {
            0x6000..=0x7fff => {
                self.read_prg_ram_bank(self.prg_regs[0], (address - 0x6000) as usize)
            }
            0x8000..=0xffff => self.read_mapped_prg(address),
            _ => 0,
        }
    }

    fn write_prg(&mut self, address: u16, value: u8) {
        if !self.prg_ram_write_enabled() {
            return;
        }

        match self.prg_mapping(address) {
            Some((false, reg, offset, size)) => self.write_prg_ram_bank(reg, offset, size, value),
            None if (0x6000..=0x7fff).contains(&address) => self.write_prg_ram_bank(
                self.prg_regs[0],
                (address - 0x6000) as usize,
                0x2000,
                value,
            ),
            _ => {}
        }
    }

    fn read_mapped_prg(&self, address: u16) -> u8 {
        let Some((is_rom, reg, offset, size)) = self.prg_mapping(address) else {
            return 0;
        };

        if is_rom {
            self.read_prg_rom_bank(reg, offset, size)
        } else {
            self.read_prg_ram_bank_sized(reg, offset, size)
        }
    }

    fn prg_mapping(&self, address: u16) -> Option<(bool, u8, usize, usize)> {
        match self.prg_mode {
            0 => Some((true, self.prg_regs[4], (address - 0x8000) as usize, 0x8000)),
            1 => match address {
                0x8000..=0xbfff => Some((
                    (self.prg_regs[2] & 0x80) != 0,
                    self.prg_regs[2],
                    (address - 0x8000) as usize,
                    0x4000,
                )),
                0xc000..=0xffff => {
                    Some((true, self.prg_regs[4], (address - 0xc000) as usize, 0x4000))
                }
                _ => None,
            },
            2 => match address {
                0x8000..=0xbfff => Some((
                    (self.prg_regs[2] & 0x80) != 0,
                    self.prg_regs[2],
                    (address - 0x8000) as usize,
                    0x4000,
                )),
                0xc000..=0xdfff => Some((
                    (self.prg_regs[3] & 0x80) != 0,
                    self.prg_regs[3],
                    (address - 0xc000) as usize,
                    0x2000,
                )),
                0xe000..=0xffff => {
                    Some((true, self.prg_regs[4], (address - 0xe000) as usize, 0x2000))
                }
                _ => None,
            },
            _ => match address {
                0x8000..=0x9fff => Some((
                    (self.prg_regs[1] & 0x80) != 0,
                    self.prg_regs[1],
                    (address - 0x8000) as usize,
                    0x2000,
                )),
                0xa000..=0xbfff => Some((
                    (self.prg_regs[2] & 0x80) != 0,
                    self.prg_regs[2],
                    (address - 0xa000) as usize,
                    0x2000,
                )),
                0xc000..=0xdfff => Some((
                    (self.prg_regs[3] & 0x80) != 0,
                    self.prg_regs[3],
                    (address - 0xc000) as usize,
                    0x2000,
                )),
                0xe000..=0xffff => {
                    Some((true, self.prg_regs[4], (address - 0xe000) as usize, 0x2000))
                }
                _ => None,
            },
        }
    }

    fn read_prg_rom_bank(&self, reg: u8, offset: usize, size: usize) -> u8 {
        let bank = match size {
            0x8000 => (reg as usize & !0x03) % self.prg_rom_bank_count(),
            0x4000 => (reg as usize & !0x01) % self.prg_rom_bank_count(),
            _ => (reg as usize & 0x7f) % self.prg_rom_bank_count(),
        };
        self.prg_rom[(bank * PRG_BANK_SIZE_8K + offset) % self.prg_rom.len()]
    }

    fn read_prg_ram_bank(&self, reg: u8, offset: usize) -> u8 {
        self.read_prg_ram_bank_sized(reg, offset, 0x2000)
    }

    fn read_prg_ram_bank_sized(&self, reg: u8, offset: usize, size: usize) -> u8 {
        let bank = match size {
            0x4000 => (reg as usize & !0x01) % self.prg_ram_bank_count(),
            _ => (reg as usize & 0x07) % self.prg_ram_bank_count(),
        };
        self.prg_ram[(bank * PRG_BANK_SIZE_8K + offset) % self.prg_ram.len()]
    }

    fn write_prg_ram_bank(&mut self, reg: u8, offset: usize, size: usize, value: u8) {
        let bank = match size {
            0x4000 => (reg as usize & !0x01) % self.prg_ram_bank_count(),
            _ => (reg as usize & 0x07) % self.prg_ram_bank_count(),
        };
        let index = (bank * PRG_BANK_SIZE_8K + offset) % self.prg_ram.len();
        self.prg_ram[index] = value;
    }

    fn read_exram_cpu(&self, address: u16) -> u8 {
        match self.exram_mode {
            0x02 => self.exram[(address - 0x5c00) as usize],
            0x03 => self.exram[(address - 0x5c00) as usize],
            _ => 0,
        }
    }

    fn write_exram_cpu(&mut self, address: u16, value: u8) {
        if self.exram_mode != 0x03 {
            self.exram[(address - 0x5c00) as usize] = value;
        }
    }

    fn nametable_region(&self, address: u16) -> (usize, usize) {
        let addr = (address - 0x2000) as usize;
        let region = (addr / 0x400) & 0x03;
        let offset = addr % 0x400;
        (region, offset)
    }

    fn fill_attribute_byte(&self) -> u8 {
        let bits = self.fill_color & 0x03;
        bits | (bits << 2) | (bits << 4) | (bits << 6)
    }

    fn read_nametable_byte(&self, address: u16) -> u8 {
        let (region, offset) = self.nametable_region(address);
        match self.nt_mapping[region] {
            0 | 1 => {
                let page = self.nt_mapping[region] as usize;
                self.ciram[page * 0x400 + offset]
            }
            2 => {
                if self.exram_mode >= 0x02 {
                    0
                } else {
                    self.exram[offset]
                }
            }
            3 => {
                if offset < 0x03c0 {
                    self.fill_tile
                } else {
                    self.fill_attribute_byte()
                }
            }
            _ => 0,
        }
    }

    fn split_enabled(&self) -> bool {
        (self.split_control & 0x80) != 0 && self.substitutions_enabled && self.exram_mode <= 0x01
    }

    fn in_split_region(&self, screen_x: u8) -> bool {
        let tile_x = screen_x / 8;
        let threshold = self.split_control & 0x1f;
        if (self.split_control & 0x40) != 0 {
            tile_x >= threshold
        } else {
            tile_x < threshold
        }
    }

    fn split_region_pixel(
        &self,
        screen_x: u8,
        screen_y: u8,
        tile_fine_x: usize,
    ) -> Option<BackgroundTileOverride> {
        let tile_x = (screen_x / 8) as usize;
        let split_y = screen_y.wrapping_add(self.split_scroll);
        let tile_y = ((split_y as usize) / 8) % 30;
        let fine_y = (split_y as usize) % 8;
        let nt_index = tile_y * 32 + tile_x;
        let tile_idx = self.exram[nt_index];
        let attr_index = 0x03c0 + (tile_y / 4) * 8 + (tile_x / 4);
        let attr = self.exram[attr_index];
        let shift = (((tile_y as u8 >> 1) & 0x01) << 2) | (((tile_x as u8 >> 1) & 0x01) << 1);
        let palette_idx = (attr >> shift) & 0x03;
        let color_idx = self.read_pattern_pixel(
            self.split_bank as usize * 0x1000,
            tile_idx,
            tile_fine_x,
            fine_y,
        );
        Some(BackgroundTileOverride {
            palette_idx,
            color_idx,
        })
    }

    fn extended_attribute_enabled(&self) -> bool {
        self.exram_mode == 0x01 && self.substitutions_enabled
    }

    fn read_pattern_pixel(&self, base_addr: usize, tile_idx: u8, x: usize, y: usize) -> u8 {
        let tile_addr = (base_addr + tile_idx as usize * 16) % self.chr_mem.len();
        let low = self.chr_mem[tile_addr + y];
        let high = self.chr_mem[tile_addr + y + 8];
        let bit = 7 - x;
        ((low >> bit) & 1) | (((high >> bit) & 1) << 1)
    }

    fn current_cpu_chr_uses_bg_regs(&self) -> bool {
        self.ppu_sprite_8x16 && self.substitutions_enabled && self.last_cpu_chr_bg
    }

    fn sync_cpu_chr_view(&mut self) {
        for addr in 0..CHR_WINDOW_SIZE as u16 {
            let mapped = self.map_chr_addr(addr, PatternAccess::Cpu);
            self.cpu_chr_view[addr as usize] = self.chr_mem[mapped];
        }
    }

    fn map_chr_addr(&self, address: u16, access: PatternAccess) -> usize {
        let use_bg_regs = match access {
            PatternAccess::Background => self.ppu_sprite_8x16 && self.substitutions_enabled,
            PatternAccess::Cpu => self.current_cpu_chr_uses_bg_regs(),
            PatternAccess::Sprite => false,
        };

        if access == PatternAccess::Background && self.extended_attribute_enabled() {
            return address as usize % self.chr_mem.len();
        }

        let addr = address as usize & 0x1fff;
        let bank_index = self.chr_bank_index(addr, use_bg_regs);
        let bank_base = (bank_index % self.chr_bank_count_1k()) * CHR_BANK_SIZE_1K;
        bank_base + (addr % CHR_BANK_SIZE_1K)
    }

    fn chr_bank_index(&self, addr: usize, use_bg_regs: bool) -> usize {
        let upper = (self.upper_chr_bits as usize) << 8;
        let reg = if use_bg_regs {
            match self.chr_mode {
                0 => self.bg_chr_regs[3],
                1 => self.bg_chr_regs[3],
                2 => {
                    if addr & 0x0800 == 0 {
                        self.bg_chr_regs[1]
                    } else {
                        self.bg_chr_regs[3]
                    }
                }
                _ => self.bg_chr_regs[(addr / 0x0400) & 0x03],
            }
        } else {
            match self.chr_mode {
                0 => self.sprite_chr_regs[7],
                1 => {
                    if addr < 0x1000 {
                        self.sprite_chr_regs[3]
                    } else {
                        self.sprite_chr_regs[7]
                    }
                }
                2 => match addr / 0x0800 {
                    0 => self.sprite_chr_regs[1],
                    1 => self.sprite_chr_regs[3],
                    2 => self.sprite_chr_regs[5],
                    _ => self.sprite_chr_regs[7],
                },
                _ => self.sprite_chr_regs[addr / 0x0400],
            }
        } as usize;

        let scale = match self.chr_mode {
            0 => 8,
            1 => 4,
            2 => 2,
            _ => 1,
        };
        ((upper | reg) * scale) + ((addr / 0x0400) % scale)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn create_prg() -> [u8; PRG_BANK_SIZE_8K * 8] {
        let mut prg = [0u8; PRG_BANK_SIZE_8K * 8];
        for bank in 0..8 {
            prg[bank * PRG_BANK_SIZE_8K] = bank as u8;
        }
        prg
    }

    fn create_chr() -> [u8; CHR_BANK_SIZE_1K * 16] {
        let mut chr = [0u8; CHR_BANK_SIZE_1K * 16];
        for bank in 0..16 {
            chr[bank * CHR_BANK_SIZE_1K] = bank as u8;
        }
        chr
    }

    #[test]
    fn switches_prg_banks_in_mode_3() {
        let mut mmc5 = MMC5::new(&create_prg(), &create_chr(), Some(0x10000), false);
        mmc5.write(0x5114, 0x81);
        mmc5.write(0x5115, 0x82);
        mmc5.write(0x5116, 0x83);
        mmc5.write(0x5117, 0x84);

        assert_eq!(mmc5.read(0x8000), 1);
        assert_eq!(mmc5.read(0xa000), 2);
        assert_eq!(mmc5.read(0xc000), 3);
        assert_eq!(mmc5.read(0xe000), 4);
    }

    #[test]
    fn maps_fill_mode_nametable_reads() {
        let mut mmc5 = MMC5::new(&create_prg(), &create_chr(), Some(0x10000), false);
        mmc5.write(0x5105, 0xff);
        mmc5.write(0x5106, 0x2a);
        mmc5.write(0x5107, 0x03);

        assert_eq!(mmc5.read_nametable(0x2000), 0x2a);
        assert_eq!(mmc5.read_nametable(0x23c0), 0xff);
    }

    #[test]
    fn exposes_multiplier_result() {
        let mut mmc5 = MMC5::new(&create_prg(), &create_chr(), Some(0x10000), false);
        mmc5.write(0x5205, 7);
        mmc5.write(0x5206, 9);

        assert_eq!(mmc5.read(0x5205), 63);
        assert_eq!(mmc5.read(0x5206), 0);
    }
}
