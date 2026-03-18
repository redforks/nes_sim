use super::*;
use crate::test_utils::MockMcu;
use std::cell::RefCell;

/// An MCU backed by a real PPU that fires VBlank at scanline 241 dot 1.
/// Used to test that `process_frame` correctly waits for VBlank.
struct VBlankMcu {
    memory: RefCell<[u8; 0x10000]>,
    ppu: crate::nes::ppu::Ppu,
    pattern: [u8; 0x2000],
    vblank_seen: RefCell<bool>,
}

impl VBlankMcu {
    fn new() -> Self {
        VBlankMcu {
            memory: RefCell::new([0; 0x10000]),
            ppu: crate::nes::ppu::Ppu::default(),
            pattern: [0; 0x2000],
            vblank_seen: RefCell::new(false),
        }
    }
}

impl Mcu for VBlankMcu {
    fn read(&self, addr: u16) -> u8 {
        self.memory.borrow()[addr as usize]
    }

    fn write(&self, addr: u16, value: u8) {
        self.memory.borrow_mut()[addr as usize] = value;
    }

    fn request_irq(&self) -> bool {
        false
    }

    fn tick_ppu(&self) -> bool {
        let result = self.ppu.tick(&self.pattern);
        if result.vblank_started {
            *self.vblank_seen.borrow_mut() = true;
        }
        result.nmi
    }

    fn take_vblank(&self) -> bool {
        let v = *self.vblank_seen.borrow();
        *self.vblank_seen.borrow_mut() = false;
        v
    }
}

#[test]
fn test_machine_creation() {
    let mcu = Box::new(MockMcu::new());
    let machine = Machine::new(mcu);
    assert_eq!(machine.cpu.pc, 0);
}

#[test]
fn test_machine_with_plugin() {
    let mcu = Box::new(MockMcu::new());
    let machine = Machine::with_plugin(EmptyPlugin(), mcu);
    assert_eq!(machine.cpu.pc, 0);
}

#[test]
fn test_set_pc() {
    let mcu = Box::new(MockMcu::new());
    let mut machine = Machine::new(mcu);

    machine.set_pc(0x1234);
    assert_eq!(machine.cpu.pc, 0x1234);
}

#[test]
fn test_reset() {
    let mcu = Box::new(MockMcu::new());
    let mut machine = Machine::new(mcu);

    machine.set_pc(0x5678);
    machine.reset();
    // Reset reads PC from 0xFFFC
    assert_eq!(machine.cpu.pc, 0);
}

#[test]
fn test_run_ticks_zero() {
    let mcu = Box::new(MockMcu::new());
    let mut machine = Machine::new(mcu);

    let result = machine.run_ticks(0);
    assert_eq!(result, ExecuteResult::Continue);
}

#[test]
fn test_process_frame() {
    let mcu = Box::new(MockMcu::new());
    let mut machine = Machine::new(mcu);

    // Run for 1 frame
    let result = machine.process_frame();

    // Should return continue result
    assert_eq!(result, ExecuteResult::Continue);
}

#[test]
fn test_process_frame_with_zero_ms() {
    let mcu = Box::new(MockMcu::new());
    let mut machine = Machine::new(mcu);

    // Run for 1 frame
    let result = machine.process_frame();

    // Should return continue result
    assert_eq!(result, ExecuteResult::Continue);
}

#[test]
fn test_run_ticks_positive() {
    let mcu = MockMcu::new();
    // Write a JMP instruction to create an infinite loop
    // JMP $8000 = 0x4C 0x00 0x80 at address 0x8000
    mcu.write(0x8000, 0x4C); // JMP absolute
    mcu.write(0x8001, 0x00); // low byte of address
    mcu.write(0x8002, 0x80); // high byte of address

    let mcu = Box::new(mcu);
    let mut machine = Machine::new(mcu);

    machine.set_pc(0x8000);

    // Run some ticks
    let result = machine.run_ticks(10);
    assert_eq!(result, ExecuteResult::Continue);
}

#[test]
fn test_process_frame_waits_for_vblank() {
    // Use a VBlankMcu whose PPU fires VBlank at the correct time.
    // Program: infinite JMP loop at $8000 so the CPU keeps executing.
    let mcu = VBlankMcu::new();
    mcu.write(0x8000, 0x4C); // JMP absolute
    mcu.write(0x8001, 0x00);
    mcu.write(0x8002, 0x80); // JMP $8000

    let mcu = Box::new(mcu);
    let mut machine = Machine::new(mcu);
    machine.set_pc(0x8000);

    // process_frame should return as soon as the PPU reaches VBlank
    // (scanline 241, dot 1 = after 241*341 + 1 = 82,262 PPU dots = ~27,421 CPU cycles).
    // The call must complete well within MAX_TICKS_PER_FRAME.
    let result = machine.process_frame();
    assert_eq!(result, ExecuteResult::Continue);

    // After one frame the PPU should be at or past scanline 241.
    // We verify by running a second frame — it should also complete without hitting
    // the safety limit, proving VBlank fires consistently every frame.
    let result = machine.process_frame();
    assert_eq!(result, ExecuteResult::Continue);
}
