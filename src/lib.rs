mod test;

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
    /// Return  (address, ticks, operands) ticks: count for the given addressing mode
    /// operands: number of operands in bytes for the given addressing mode
    fn address(&self, cpu: &Cpu) -> (u16, u8, u8) {
        match self {
            &Agu::ZeroPage(addr) => (addr as u16, 1, 1),
            &Agu::Absolute(addr) => (addr, 2, 2),
            &Agu::ZeroPageX(addr) => (addr.wrapping_add(cpu.x) as u16, 2, 1),
            &Agu::ZeroPageY(addr) => (addr.wrapping_add(cpu.y) as u16, 2, 1),
            &Agu::AbsoluteX(addr) => {
                let high = (addr >> 8) as u8;
                let r = addr.wrapping_add(cpu.x as u16) as u16;
                (r, if high == (r >> 8) as u8 { 2 } else { 3 }, 2)
            }
            &Agu::AbsoluteY(addr) => {
                let high = (addr >> 8) as u8;
                let r = addr.wrapping_add(cpu.y as u16) as u16;
                (r, if high == (r >> 8) as u8 { 2 } else { 3 }, 2)
            }
            &Agu::Indirect(addr) => (cpu.read_word(addr), 2, 2),
            &Agu::IndirectX(addr) => (cpu.read_zero_page_word(addr.wrapping_add(cpu.x)), 4, 1),
            &Agu::IndirectY(addr) => {
                let addr = cpu.read_zero_page_word(addr);
                let high = (addr >> 8) as u8;
                let r = addr.wrapping_add(cpu.y as u16);
                (r, if high == (r >> 8) as u8 { 3 } else { 4 }, 1)
            }
        }
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

    // (pcDelta, tickCount)
    fn execute(&self, cpu: &mut Cpu) -> (u8, u8);
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

    fn execute(&self, cpu: &mut Cpu) -> (u8, u8) {
        let (v, overflow) = cpu.a.overflowing_add(self.0);
        cpu.a = v;
        cpu.set_flag(CarryFlag, overflow);
        (2, 2)
    }
}

trait FlagBit {
    const BIT: u8;
}

struct CarryFlag;

impl FlagBit for CarryFlag {
    const BIT: u8 = 0x1;
}

// Trait to sync instruction execution  times.
trait SyncInstructionCycle {
    fn start(&mut self);

    fn end(&mut self, cycles: u8);
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

    fn execute<T: SyncInstructionCycle>(&mut self, inst: Instruction, cycle_sync: &mut T) {
        cycle_sync.start();
        let (pc, cycles) = match inst {
            Instruction::AddLiteral(inst) => {
                inst.execute(self)
            }
        };
        cycle_sync.end(cycles);

        self.inc_pc(pc);
    }

    fn inc_pc(&mut self, delta: u8) {
        self.pc = self.pc.wrapping_add(delta as u16);
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

    fn get(&self, agu: Agu) -> (u8, u8, u8) {
        let addr = agu.address(self);
        (self.read_byte(addr.0), addr.1, addr.2)
    }
}
