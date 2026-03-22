use super::*;

struct TestMcu {
    mem: [u8; 0x10000],
    ticks: usize,
}

impl Default for TestMcu {
    fn default() -> Self {
        Self {
            mem: [0; 0x10000],
            ticks: 0,
        }
    }
}

impl Mcu for TestMcu {
    fn read(&mut self, address: u16) -> u8 {
        self.mem[address as usize]
    }

    fn write(&mut self, address: u16, value: u8) {
        self.mem[address as usize] = value;
    }

    fn tick(&mut self) {
        self.ticks += 1;
    }
}

#[test]
fn new_initializes_registers_and_fetch_queue() {
    let cpu = Cpu::new(TestMcu::default());

    assert_eq!(cpu.a, 0);
    assert_eq!(cpu.x, 0);
    assert_eq!(cpu.y, 0);
    assert_eq!(cpu.pc, 0);
    assert_eq!(cpu.sp, 0);
    assert_eq!(cpu.status, Flag::InterruptDisabled as u8);
    assert_eq!(cpu.opcode, 0);
    assert_eq!(cpu.ab, 0);
    assert_eq!(cpu.alu, 0);
    assert_eq!(cpu.microcode_queue.len(), 0);
}

#[test]
fn inc_read_byte_advances_pc_and_ticks() {
    let mut mcu = TestMcu::default();
    mcu.mem[0x0200] = 0xAB;

    let mut cpu = Cpu::new(mcu);
    cpu.pc = 0x0200;

    assert_eq!(cpu.inc_read_byte(), 0xAB);
    assert_eq!(cpu.pc, 0x0201);
    assert_eq!(cpu.mcu().ticks, 1);
}
