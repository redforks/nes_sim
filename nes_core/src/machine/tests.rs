use super::*;
use image::RgbaImage;

struct MockMcu {
    memory: [u8; 0x10000],
    image: RgbaImage,
    ppu: crate::nes::ppu::Ppu,
}

impl MockMcu {
    fn new() -> Self {
        MockMcu {
            memory: [0; 0x10000],
            image: RgbaImage::new(256, 240),
            ppu: crate::nes::ppu::Ppu::default(),
        }
    }
}

impl Mcu for MockMcu {
    fn read(&self, addr: u16) -> u8 {
        self.memory[addr as usize]
    }

    fn write(&mut self, addr: u16, value: u8) {
        self.memory[addr as usize] = value;
    }

    fn get_ppu(&mut self) -> &mut dyn crate::nes::ppu::PpuTrait {
        &mut self.ppu
    }

    fn request_irq(&self) -> bool {
        false
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
    let mut mcu = MockMcu::new();
    // Write a JMP instruction to create an infinite loop
    // JMP $8000 = 0x4C 0x00 0x80 at address 0x8000
    mcu.memory[0x8000] = 0x4C; // JMP absolute
    mcu.memory[0x8001] = 0x00; // low byte of address
    mcu.memory[0x8002] = 0x80; // high byte of address

    let mcu = Box::new(mcu);
    let mut machine = Machine::new(mcu);

    machine.set_pc(0x8000);

    // Run some ticks
    let result = machine.run_ticks(10);
    assert_eq!(result, ExecuteResult::Continue);
}
