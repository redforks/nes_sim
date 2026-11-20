//! One MCU test double for the CPU wait/freeze test modules.
//!
//! The wait tests observe the `Mcu` seam, not a real NES memory map. This
//! double keeps the one implementation of what those tests all need: flat
//! memory with read/write logs, `$4015..=$4017` device-register
//! classification, and configurable status-register behavior. Each module
//! picks the configuration its tests need instead of declaring its own MCU.

use crate::{SystemClock, mcu::Mcu};

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum StatusRegister {
    /// Plain memory: no destructive reads and no VBL flag.
    None,
    /// `$2002` (and its PPU mirrors) with real status-read side effects.
    Vbl,
    /// A test-driven flag at an arbitrary polled address. Reads stay plain;
    /// only the classification hooks report a status register.
    Polled(u16),
}

/// Shared MCU double for wait/freeze tests at the CPU/MCU seam.
#[derive(Debug)]
pub struct WaitMcu {
    pub mem: [u8; 0x10000],
    /// Every generic bus read, in order (including opcode fetches).
    pub reads: Vec<u16>,
    pub writes: Vec<(u16, u8)>,
    /// Real reads of the `$2002` status register and its mirrors.
    pub ppu_reads: Vec<u16>,
    /// Non-status reads in the `$2002` configuration; proves a frozen poll
    /// no longer drives the bus.
    pub other_reads: Vec<u16>,
    /// Replayed read side effects, counted apart from real reads. Includes
    /// status clears and `$4015`-`$4017` device actions.
    pub replays: usize,
    /// Device-side effect replays by address (`$4015`-`$4017`).
    pub device_replays: [usize; 3],
    /// Whether the modeled APU frame IRQ is pending. Device-effect replay
    /// clears it just as a real `$4015` read does.
    pub apu_irq_pending: bool,
    /// Number of shifts a real or replayed controller-port read performed.
    pub controller_shifts: [usize; 2],
    pub status: u8,
    pub write_toggle: bool,
    pub nmi_enabled: bool,
    /// A status read on VBL dot 1 suppresses VBL, as the real PPU does.
    pub suppressed: bool,
    pub scanline: u16,
    pub dot: u16,
    /// Test-driven VBL flag used by the [`StatusRegister::Polled`] setup.
    pub status_vbl: bool,
    status_register: StatusRegister,
}

impl Default for WaitMcu {
    fn default() -> Self {
        Self::new()
    }
}

impl WaitMcu {
    pub fn new() -> Self {
        Self {
            mem: [0; 0x10000],
            reads: Vec::new(),
            writes: Vec::new(),
            ppu_reads: Vec::new(),
            other_reads: Vec::new(),
            replays: 0,
            device_replays: [0; 3],
            apu_irq_pending: false,
            controller_shifts: [0; 2],
            status: 0,
            write_toggle: false,
            nmi_enabled: false,
            suppressed: false,
            scanline: 100,
            dot: 100,
            status_vbl: false,
            status_register: StatusRegister::None,
        }
    }

    /// Give the double `$2002` status semantics, including its mirrors.
    pub fn with_vbl_status(mut self) -> Self {
        self.status_register = StatusRegister::Vbl;
        self
    }

    /// Classify `addr` as a destructive status register whose VBL flag is
    /// driven by the `status_vbl` field. Bus reads stay plain memory reads.
    pub fn with_polled_status(mut self, addr: u16) -> Self {
        self.status_register = StatusRegister::Polled(addr);
        self
    }

    fn is_status_addr(&self, addr: u16) -> bool {
        match self.status_register {
            StatusRegister::None => false,
            StatusRegister::Vbl => (0x2000..=0x3FFF).contains(&addr) && (addr & 0x2007) == 0x2002,
            StatusRegister::Polled(status_addr) => addr == status_addr,
        }
    }

    fn vbl_visible(&self) -> bool {
        match self.status_register {
            StatusRegister::None => false,
            StatusRegister::Vbl => self.status & 0x80 != 0,
            StatusRegister::Polled(_) => self.status_vbl,
        }
    }

    fn read_status(&mut self, addr: u16) -> u8 {
        self.ppu_reads.push(addr);
        let status = self.status;
        self.status_side_effects();
        status
    }

    fn status_side_effects(&mut self) {
        if self.status_register != StatusRegister::Vbl {
            return;
        }
        if self.scanline == 241 && self.dot == 1 {
            self.suppressed = true;
        }
        self.status &= !0x80;
        self.write_toggle = false;
    }

    /// Advance the modeled PPU beam by one dot (VBL sets at 241:2).
    pub fn tick_ppu(&mut self) {
        if self.status_register == StatusRegister::Vbl
            && self.scanline == 241
            && self.dot == 2
            && !self.suppressed
        {
            self.status |= 0x80;
        }
    }

    pub fn nmi_line(&self) -> bool {
        self.vbl_visible() && self.nmi_enabled
    }

    /// Forget recorded bus traffic without changing memory or device state.
    #[allow(dead_code)]
    pub fn clear_traffic(&mut self) {
        self.reads.clear();
        self.writes.clear();
        self.ppu_reads.clear();
        self.other_reads.clear();
    }

    /// Index into `device_replays` / `controller_shifts` for a device port.
    fn device_index(address: u16) -> Option<usize> {
        match address {
            0x4015 => Some(0),
            0x4016 => Some(1),
            0x4017 => Some(2),
            _ => None,
        }
    }

    /// Model the device action of a real read or a frozen-loop replay.
    /// No bus value is produced here; the real read still returns memory.
    fn apply_device_effects(&mut self, address: u16) {
        match address {
            0x4015 => self.apu_irq_pending = false,
            0x4016 => self.controller_shifts[0] += 1,
            0x4017 => self.controller_shifts[1] += 1,
            _ => {}
        }
    }
}

impl Mcu for WaitMcu {
    fn read(&mut self, address: u16) -> u8 {
        self.reads.push(address);
        if Self::device_index(address).is_some() {
            self.apply_device_effects(address);
        }
        if self.status_register == StatusRegister::Vbl && self.is_status_addr(address) {
            return self.read_status(address);
        }
        let value = self.mem[address as usize];
        if self.status_register == StatusRegister::Vbl {
            self.other_reads.push(address);
        }
        value
    }

    fn peek(&self, address: u16) -> u8 {
        if self.status_register == StatusRegister::Vbl && self.is_status_addr(address) {
            self.status
        } else {
            self.mem[address as usize]
        }
    }

    fn write(&mut self, address: u16, value: u8, _clock: SystemClock) {
        self.writes.push((address, value));
        if self.status_register == StatusRegister::Vbl && (0x2000..=0x3FFF).contains(&address) {
            match address & 0x2007 {
                0x2000 => self.nmi_enabled = value & 0x80 != 0,
                0x2005 | 0x2006 => self.write_toggle = !self.write_toggle,
                _ => {}
            }
        }
        self.mem[address as usize] = value;
    }

    fn status_register_has_vbl(&self) -> bool {
        self.vbl_visible()
    }

    fn has_destructive_status_read(&self, address: u16) -> bool {
        match self.status_register {
            StatusRegister::None => false,
            StatusRegister::Vbl => self.is_status_addr(address),
            StatusRegister::Polled(status_addr) => address == status_addr,
        }
    }

    fn is_device_register(&self, address: u16) -> bool {
        (0x4015..=0x4017).contains(&address)
    }

    fn replay_status_read_side_effects(&mut self, address: u16) {
        // Side effects only: no bus read is recorded, matching the MCU
        // hook's contract. Replays are counted separately from real reads.
        self.replays += 1;
        self.status_side_effects();
        if let Some(index) = Self::device_index(address) {
            self.device_replays[index] += 1;
            self.apply_device_effects(address);
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn plain_memory_has_no_status_or_device_read_effects() {
        let mcu = WaitMcu::new();
        assert!(mcu.is_device_register(0x4015));
        assert!(mcu.is_device_register(0x4017));
        assert!(!mcu.is_device_register(0x4014));
        assert!(!mcu.has_destructive_status_read(0x0010));
        assert!(!mcu.status_register_has_vbl());
    }

    #[test]
    fn vbl_status_reads_clear_the_flag_and_toggle() {
        let mut mcu = WaitMcu::new().with_vbl_status();
        mcu.status = 0x80;
        mcu.write_toggle = true;
        assert!(mcu.status_register_has_vbl());
        assert_eq!(mcu.read(0x2FFA), 0x80);
        assert_eq!(mcu.ppu_reads, vec![0x2FFA]);
        assert_eq!(mcu.other_reads, Vec::<u16>::new());
        assert!(!mcu.write_toggle);
        assert_eq!(mcu.status, 0);
        mcu.replay_status_read_side_effects(0x2002);
        assert_eq!(mcu.replays, 1);
    }

    #[test]
    fn generic_memory_reads_are_logged_for_poll_traces() {
        let mut mcu = WaitMcu::new();
        mcu.mem[0x10] = 1;
        assert_eq!(mcu.read(0x10), 1);
        assert_eq!(mcu.reads, vec![0x10]);
        mcu.write(0x10, 2, SystemClock::default());
        assert_eq!(mcu.writes, vec![(0x10, 2)]);
        assert_eq!(mcu.mem[0x10], 2);
    }

    #[test]
    fn vbl_tick_sets_vbl_and_a_race_read_suppresses_it() {
        let mut mcu = WaitMcu::new().with_vbl_status();
        mcu.nmi_enabled = true;
        mcu.scanline = 241;
        mcu.dot = 1;
        mcu.read(0x2002);
        assert!(mcu.suppressed);
        mcu.dot = 2;
        mcu.tick_ppu();
        assert_eq!(mcu.status & 0x80, 0);
        assert!(!mcu.nmi_line());
    }

    #[test]
    fn polled_status_keeps_its_test_driven_flag_and_plain_reads() {
        let mut mcu = WaitMcu::new().with_polled_status(0x10);
        mcu.mem[0x10] = 0x7F;
        mcu.status_vbl = true;
        assert!(mcu.status_register_has_vbl());
        assert!(mcu.has_destructive_status_read(0x10));
        assert!(!mcu.has_destructive_status_read(0x11));
        assert_eq!(mcu.read(0x10), 0x7F);
        assert!(mcu.status_vbl, "a plain read must not clear the test flag");
        assert_eq!(mcu.reads, vec![0x10]);
        assert_eq!(mcu.ppu_reads, Vec::<u16>::new());
    }
}
