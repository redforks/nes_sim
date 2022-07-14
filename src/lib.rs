/// Address generation unit
enum Agu {
    ZeroPage(u8),
    Absolute(u16),
    ZeroPageX(u8),
    ZeroPageY(u8),
    AbsoluteX(u16),
    AbsoluteY(u16),
    Indirect(u16),
    IndirectX(u8),
    IndirectY(u8),
}

impl Agu {
    /// Return  address and tick count for the given addressing mode
    fn address(&self, cpu: &Cpu) -> (u16, u8) {
        match self {
            &Agu::ZeroPage(addr) => (addr as u16, 1),
            &Agu::Absolute(addr) => (addr, 2),
            &Agu::ZeroPageX(addr) => (addr.wrapping_add(cpu.x) as u16, 2),
            &Agu::ZeroPageY(addr) => (addr.wrapping_add(cpu.y) as u16, 2),
            &Agu::AbsoluteX(addr) => {
                let high = (addr >> 8) as u8;
                let r = addr.wrapping_add(cpu.x as u16) as u16;
                (r, if high == (r >> 8) as u8 { 2 } else { 3 })
            },
            &Agu::AbsoluteY(addr) => {
                let high = (addr >> 8) as u8;
                let r = addr.wrapping_add(cpu.y as u16) as u16;
                (r, if high == (r >> 8) as u8 { 2 } else { 3 })
            },
            &Agu::Indirect(addr) => (cpu.read_word(addr), 2),
            &Agu::IndirectX(addr) => (cpu.read_zero_page_word(addr.wrapping_add(cpu.x)), 4),
            &Agu::IndirectY(addr) => {
                let addr = cpu.read_zero_page_word(addr);
                let high = (addr >> 8) as u8;
                let r = addr.wrapping_add(cpu.y as u16);
                (r, if high == (r >> 8) as u8 { 3 } else { 4 })
            }
        }
    }
}

#[cfg(test)]
mod addr_test {
    use super::*;

    #[test]
    fn zero_page() {
        let cpu = Cpu::new(0);
        let addr = Agu::ZeroPage(0x10);
        assert_eq!(addr.address(&cpu), (0x10, 1));
    }

    #[test]
    fn absolute() {
        let cpu = Cpu::new(0);
        let addr = Agu::Absolute(0x10);
        assert_eq!(addr.address(&cpu), (0x10, 2));
    }

    #[test]
    fn zero_page_x() {
        let mut cpu = Cpu::new(0);
        cpu.x = 10;
        assert_eq!((11,2), Agu::ZeroPageX(1).address(&cpu));

        // wrap if overflow
        assert_eq!((9, 2), Agu::ZeroPageX(0xff).address(&cpu));
    }

    #[test]
    fn zero_page_y() {
        let mut cpu = Cpu::new(0);
        cpu.y = 10;
        assert_eq!((11, 2), Agu::ZeroPageY(1).address(&cpu));

        // wrap if overflow
        assert_eq!((9, 2), Agu::ZeroPageY(0xff).address(&cpu));
    }

    #[test]
    fn absolute_x() {
        let mut cpu = Cpu::new(0);
        cpu.x = 0x10;
        assert_eq!((0x1010, 2), Agu::AbsoluteX(0x1000).address(&cpu));

        // wrap if overflow, add extra tick because of cross page
        cpu.x = 1;
        assert_eq!((0, 3), Agu::AbsoluteX(0xffff).address(&cpu));
    }

    #[test]
    fn absolute_y() {
        let mut cpu = Cpu::new(0);
        cpu.y = 0x10;
        assert_eq!((0x1010, 2), Agu::AbsoluteY(0x1000).address(&cpu));

        // wrap if overflow, add extra tick because of cross page
        cpu.y = 1;
        assert_eq!((0, 3), Agu::AbsoluteY(0xffff).address(&cpu));
    }

    #[test]
    fn indirect() {
        let mut cpu = Cpu::new(0);
        cpu.write_word(0x1000, 0x10);
        assert_eq!((0x10, 2), Agu::Indirect(0x1000).address(&cpu));
    }

    #[test]
    fn indirect_x() {
        let mut cpu = Cpu::new(0);
        cpu.write_word(0x12, 0x1000);
        cpu.x = 0x10;
        assert_eq!((0x1000, 4), Agu::IndirectX(0x2).address(&cpu));

        // read address from first and last bytes of zero page.
        cpu.write_byte(0xff, 0x20); // low byte
        cpu.write_byte(0x00, 0x10); // high byte
        cpu.x = 0x1;
        assert_eq!((0x1020, 4), Agu::IndirectX(0xfe).address(&cpu));
    }

    #[test]
    fn indirect_y() {
        let mut cpu = Cpu::new(0);
        cpu.write_word(0x2, 0x1000);
        cpu.y = 0x10;
        assert_eq!((0x1010, 3), Agu::IndirectY(0x2).address(&cpu));

        // wrap if overflow, add extra tick because of cross page
        cpu.write_word(0x2, 0xffff);
        cpu.y = 0x1;
        assert_eq!((0x0, 4), Agu::IndirectY(0x2).address(&cpu));
    }
}

enum Instruction {
    AddLiteral(AddLiteral),
    // AddFromZeroPage(u8),
    // AddFromZeroPageX(u8),
    // AddFromAbsolute(u16),
    // AddFromAbsoluteX(u16),
    // AddFromAbsoluteY(u16),
    // AddFromIndirectX(u8),
    // AddFromIndirectY(u8),
}

trait InstructionType {
    /// Instruction length, byte counts of the opcode and operands
    const LEN: u8;
    const TICK_COUNT: u8;

    fn execute(&self, cpu: &mut Cpu);
}

#[allow(dead_code)]
struct Cpu {
    a: u8,
    x: u8,
    y: u8,
    pc: u16,
    sp: u8,
    status: u8,
    memory: [u8; 0x10000],
}

struct AddLiteral(u8);

impl InstructionType for AddLiteral {
    const LEN: u8 = 2;
    const TICK_COUNT: u8 = 2;

    fn execute(&self, cpu: &mut Cpu) {
        let (v, overflow) = cpu.a.overflowing_add(self.0);
        cpu.a = v;
        cpu.set_flag(CarryFlag, overflow);
    }
}

trait FlagBit {
    const BIT: u8;
}

struct CarryFlag;

impl FlagBit for CarryFlag {
    const BIT: u8 = 0x1;
}

impl Cpu {
    fn new(pc: u16) -> Cpu {
        Cpu {
            a: 0,
            x: 0,
            y: 0,
            pc,
            sp: 0,
            status: 0,
            memory: [0; 0x10000],
        }
    }

    fn flag<T: FlagBit>(&self, _: T) -> bool {
        (self.status & T::BIT) != 0
    }

    fn set_flag<T: FlagBit>(&mut self, _: T, value: bool) {
        if value {
            self.status |= T::BIT;
        } else {
            self.status &= !T::BIT;
        }
    }

    fn execute(&mut self, inst: Instruction) {
        match inst {
            Instruction::AddLiteral(inst) => {
                inst.execute(self);
            }
        }
    }

    fn inc_pc(&mut self, inst: Instruction) {
        match inst {
            Instruction::AddLiteral(_) => {
                self.pc = self.pc.wrapping_add(AddLiteral::LEN as u16);
            }
        }
    }

    // fn wait_cycles(&mut self, inst: Instruction) {
    // TODO: thread sleep for remaining times of inst cycles
    // }

    fn read_byte(&self, addr: u16) -> u8 {
        self.memory[addr as usize]
    }

    fn write_byte(&mut self, addr: u16, value: u8) {
        self.memory[addr as usize] = value;
    }

    fn read_word(&self, addr: u16) -> u16 {
        (self.read_byte(addr) as u16) | ((self.read_byte(addr.wrapping_add(1)) as u16) << 8)
    }

    fn read_zero_page_word(&self, addr: u8) -> u16 {
        (self.read_byte(addr as u16) as u16) | ((self.read_byte(addr.wrapping_add(1) as u16) as u16) << 8)
    }

    fn write_word(&mut self, addr: u16, value: u16) {
        self.write_byte(addr, (value & 0xff) as u8);
        self.write_byte(addr.wrapping_add(1), ((value >> 8) & 0xff) as u8);
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn read_write_byte() {
        let mut cpu = Cpu::new(0);
        cpu.write_byte(0x1000, 0x10);
        assert_eq!(0x10, cpu.read_byte(0x1000));
        cpu.write_byte(0x1000, 0x20);
        assert_eq!(0x20, cpu.read_byte(0x1000));
    }

    #[test]
    fn read_write_word() {
        let mut cpu = Cpu::new(0);
        cpu.write_word(0x1000, 0x1020);
        assert_eq!(0x1020, cpu.read_word(0x1000));
        assert_eq!(0x20, cpu.read_byte(0x1000));
        assert_eq!(0x10, cpu.read_byte(0x1001));
    }

    #[test]
    fn read_write_zero_page() {
        let mut cpu = Cpu::new(0);
        cpu.write_word(0x10, 0x1020);
        assert_eq!(0x1020, cpu.read_zero_page_word(0x10));

        cpu.write_byte(0xff, 0x20);
        cpu.write_byte(0x00, 0x10);
        assert_eq!(0x1020, cpu.read_zero_page_word(0xff));
    }

    #[test]
    fn inc_pc() {
        let mut cpu = Cpu::new(0);
        cpu.inc_pc(Instruction::AddLiteral(AddLiteral(0)));
        assert_eq!(cpu.pc, 2);

        // wrap
        cpu.pc = 0xFFFF;
        cpu.inc_pc(Instruction::AddLiteral(AddLiteral(0)));
        assert_eq!(cpu.pc, 1);
    }

    #[test]
    fn add_literal() {
        let mut cpu = Cpu::new(0);

        // carry
        cpu.a = 0xFF;
        cpu.execute(Instruction::AddLiteral(AddLiteral(1)));
        assert_eq!(cpu.a, 0);
        assert!(cpu.flag(CarryFlag));

        // reset carry if no overflow
        cpu.a = 0x1;
        cpu.execute(Instruction::AddLiteral(AddLiteral(0x2)));
        assert_eq!(cpu.a, 0x3);
        assert!(!cpu.flag(CarryFlag));
    }
}
