use crate::{cpu::Cpu2, mcu::Mcu};

/// Each Microcode instruction executed by the CPU in a single cycle
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Microcode {
    /// Fetch and decode next op code
    FetchAndDecode,
    /// Load accumulator with value from memory
    LoadA,
    /// Load X register with value from memory
    LoadX,
    /// Load Y register with value from memory
    LoadY,
    /// Store accumulator value to memory
    StoreA,
    /// Store X register value to memory
    StoreX,
    /// Store Y register value to memory
    StoreY,
    /// Read immediate value into accumulator
    LoadImmediateA,

    /// Store cpu alu register into memory at cpu.ab
    StoreAlu,

    // addressing microcodes
    /// Take a byte from instruction data stream, set cpu address_bus field by read from memory use zero page addressing
    ZeroPage {
        load_into_alu: bool,
    },
    /// Impl ZeroPageIndexedX addressing, work after ZeroPage, set abl = (abl + x) % 256, set abh = 00
    ZeroPageIndexedX {
        load_into_alu: bool,
    },
    /// Take a byte from instruction data stream, set cpu abl field
    AbsoluteL,
    /// Take a byte from instruction data stream, set cpu abh field
    AbsoluteH {
        load_into_alu: bool,
    },
    /// Take a byte from instruction data stream, set cpu abh field, add ab with
    /// x register value, set cpu ab field, retain_cycle if the 8bit plus
    /// operation overflows
    ///
    /// Must after AbsoluteL
    AbsoluteIndexedX {
        oops: bool,
        load_into_alu: bool,
    },
    /// Take a byte from instruction data stream, set cpu abh field, add ab with
    /// y register value, set cpu ab field, retain_cycle if the 8bit plus
    /// operation overflows
    ///
    /// Must after AbsoluteL
    AbsoluteIndexedY {
        oops: bool,
        load_into_alu: bool,
    },
    /// AbsoluteIndexedY but do not take high byte from instruction data stream, used to impl IndirectIndexed
    AbsoluteIndexedYWithoutHigh {
        oops: bool,
        load_into_alu: bool,
    },
    /// Do nothing, used in "oops" cycles of AbsoluteIndexed and Indirect Indexed addressing
    Nop,
    /// Load abl from memory at [ab], load abh from memory at [ab+1]
    /// append with Nop, because it is actually need two cycles
    Indexed {
        load_into_alu: bool,
    },

    /// Take immediate value from instruction data stream, add to accumulator with carry
    AdcImmediate,
    /// Fetch a byte from memory use address saved in `data_latch`, then add to accumulator with carry
    Adc,

    /// Take immediate value from instruction data stream, and it with accumulator
    AndImmediate,
    /// Fetch a byte from memory use address saved in `data_latch`, then and with accumulator
    And,

    AslAccumulator,
    Asl,
}

/// Represents the opcode of a CPU instruction, the first byte of an instruction
pub enum Opcode {}

impl Opcode {
    pub const AND_IMMEDIATE: u8 = 0x29;
    pub const AND_ZERO_PAGE: u8 = 0x25;
    pub const AND_ZERO_PAGE_X: u8 = 0x35;
    pub const AND_ABSOLUTE: u8 = 0x2D;
    pub const AND_ABSOLUTE_INDEXED_X: u8 = 0x3D;
    pub const AND_ABSOLUTE_INDEXED_Y: u8 = 0x39;
    pub const AND_INDEXED_INDIRECT: u8 = 0x21;
    pub const AND_INDIRECT_INDEXED: u8 = 0x31;

    pub const ADC_IMMEDIATE: u8 = 0x69;
    pub const ADC_ZERO_PAGE: u8 = 0x65;
    pub const ADC_ZERO_PAGE_X: u8 = 0x75;
    pub const ADC_ABSOLUTE: u8 = 0x6D;
    pub const ADC_ABSOLUTE_INDEXED_X: u8 = 0x7D;
    pub const ADC_ABSOLUTE_INDEXED_Y: u8 = 0x79;
    pub const ADC_INDEXED_INDIRECT: u8 = 0x61;
    pub const ADC_INDIRECT_INDEXED: u8 = 0x71;

    pub const ASL_ACCUMULATOR: u8 = 0x0A;
    pub const ASL_ZERO_PAGE: u8 = 0x06;
    pub const ASL_ZERO_PAGE_X: u8 = 0x16;
    pub const ASL_ABSOLUTE: u8 = 0x0E;
    pub const ASL_ABSOLUTE_INDEXED_X: u8 = 0x1E;
}

impl Microcode {
    /// Execute the micro code
    pub fn exec<M: Mcu>(self, cpu: &mut Cpu2<M>) {
        match self {
            Microcode::FetchAndDecode => Self::fetch_and_decode(cpu),
            Microcode::LoadA => Self::load_a(cpu),
            Microcode::LoadX => Self::load_x(cpu),
            Microcode::LoadY => Self::load_y(cpu),
            Microcode::StoreA => Self::store_a(cpu),
            Microcode::StoreX => Self::store_x(cpu),
            Microcode::StoreY => Self::store_y(cpu),
            Microcode::LoadImmediateA => Self::load_immediate_a(cpu),
            Microcode::AndImmediate => Self::and_immediate(cpu),
            Microcode::ZeroPage { load_into_alu } => Self::zero_page(cpu, load_into_alu),
            Microcode::ZeroPageIndexedX { load_into_alu } => {
                Self::zero_page_indexed_x(cpu, load_into_alu)
            }
            Microcode::AbsoluteL => Self::absolute_l(cpu),
            Microcode::AbsoluteH { load_into_alu } => Self::absolute_h(cpu, load_into_alu),
            Microcode::AbsoluteIndexedX {
                oops,
                load_into_alu,
            } => Self::absolute_indexed_x(cpu, oops, load_into_alu),
            Microcode::AbsoluteIndexedY {
                oops,
                load_into_alu,
            } => Self::absolute_indexed_y(cpu, oops, load_into_alu),
            Microcode::AbsoluteIndexedYWithoutHigh {
                oops,
                load_into_alu,
            } => Self::absolute_indexed_y_without_high(cpu, oops, load_into_alu),
            Microcode::AdcImmediate => Self::adc_immediate(cpu),
            Microcode::Adc => Self::adc(cpu),
            Microcode::And => Self::and(cpu),
            Microcode::StoreAlu => Self::store_alu(cpu),
            Microcode::Nop => {}
            Microcode::Indexed { load_into_alu } => Self::indexed(cpu, load_into_alu),
            Microcode::AslAccumulator => Self::asl_accumulator(cpu),
            Microcode::Asl => Self::asl(cpu),
        }
    }

    fn fetch_and_decode<M: Mcu>(cpu: &mut Cpu2<M>) {
        let opcode = cpu.inc_read_byte();
        cpu.opcode = opcode;
        match opcode {
            Opcode::AND_IMMEDIATE => {
                cpu.push_microcode(Microcode::AndImmediate);
            }
            Opcode::AND_ZERO_PAGE => {
                cpu.push_microcode(Microcode::ZeroPage {
                    load_into_alu: false,
                });
                cpu.push_microcode(Microcode::And);
            }
            Opcode::AND_ZERO_PAGE_X => {
                zero_page_indexed_x_addressing(cpu, false);
                cpu.push_microcode(Microcode::And);
            }
            Opcode::AND_ABSOLUTE => {
                absolute_addressing(cpu, false);
                cpu.push_microcode(Microcode::And);
            }
            Opcode::AND_ABSOLUTE_INDEXED_X => {
                absolute_indexed_x_addressing(cpu, true, false);
                cpu.push_microcode(Microcode::And);
            }
            Opcode::AND_ABSOLUTE_INDEXED_Y => {
                absolute_indexed_y_addressing(cpu, true, false);
                cpu.push_microcode(Microcode::And);
            }
            Opcode::AND_INDEXED_INDIRECT => {
                indexed_indirect_addressing(cpu, false);
                cpu.push_microcode(Microcode::And);
            }
            Opcode::AND_INDIRECT_INDEXED => {
                indirect_indexed_addressing(cpu, true, false);
                cpu.push_microcode(Microcode::And);
            }
            Opcode::ADC_IMMEDIATE => {
                cpu.push_microcode(Microcode::AdcImmediate);
            }
            Opcode::ADC_ZERO_PAGE => {
                cpu.push_microcode(Microcode::ZeroPage {
                    load_into_alu: false,
                });
                cpu.push_microcode(Microcode::Adc);
            }
            Opcode::ADC_ZERO_PAGE_X => {
                zero_page_indexed_x_addressing(cpu, false);
                cpu.push_microcode(Microcode::Adc);
            }
            Opcode::ADC_ABSOLUTE => {
                absolute_addressing(cpu, false);
                cpu.push_microcode(Microcode::Adc);
            }
            Opcode::ADC_ABSOLUTE_INDEXED_X => {
                absolute_indexed_x_addressing(cpu, true, false);
                cpu.push_microcode(Microcode::Adc);
            }
            Opcode::ADC_ABSOLUTE_INDEXED_Y => {
                absolute_indexed_y_addressing(cpu, true, false);
                cpu.push_microcode(Microcode::Adc);
            }
            Opcode::ADC_INDEXED_INDIRECT => {
                indexed_indirect_addressing(cpu, false);
                cpu.push_microcode(Microcode::Adc);
            }
            Opcode::ADC_INDIRECT_INDEXED => {
                indirect_indexed_addressing(cpu, true, false);
                cpu.push_microcode(Microcode::Adc);
            }
            Opcode::ASL_ACCUMULATOR => {
                cpu.push_microcode(Microcode::AslAccumulator);
            }
            Opcode::ASL_ZERO_PAGE => {
                cpu.push_microcode(Microcode::ZeroPage {
                    load_into_alu: true,
                });
                cpu.push_microcode(Microcode::StoreAlu);
                cpu.push_microcode(Microcode::Asl);
                cpu.push_microcode(Microcode::StoreAlu);
            }
            Opcode::ASL_ZERO_PAGE_X => {
                zero_page_indexed_x_addressing(cpu, true);
                cpu.push_microcode(Microcode::StoreAlu);
                cpu.push_microcode(Microcode::Asl);
                cpu.push_microcode(Microcode::StoreAlu);
            }
            Opcode::ASL_ABSOLUTE => {
                absolute_addressing(cpu, true);
                cpu.push_microcode(Microcode::StoreAlu);
                cpu.push_microcode(Microcode::Asl);
                cpu.push_microcode(Microcode::StoreAlu);
            }
            Opcode::ASL_ABSOLUTE_INDEXED_X => {
                absolute_indexed_x_addressing(cpu, false, true);
                cpu.push_microcode(Microcode::StoreAlu);
                cpu.push_microcode(Microcode::Asl);
                cpu.push_microcode(Microcode::StoreAlu);
            }
            _ => panic!("Unknown opcode: {}", opcode),
        }
    }

    fn load_a<M: Mcu>(cpu: &mut Cpu2<M>) {
        cpu.a = cpu.read_byte(cpu.ab);
        cpu.update_negative_flag(cpu.a);
        cpu.update_zero_flag(cpu.a);
    }

    fn load_x<M: Mcu>(cpu: &mut Cpu2<M>) {
        cpu.x = cpu.read_byte(cpu.ab);
        cpu.update_negative_flag(cpu.x);
        cpu.update_zero_flag(cpu.x);
    }

    fn load_y<M: Mcu>(cpu: &mut Cpu2<M>) {
        cpu.y = cpu.read_byte(cpu.ab);
        cpu.update_negative_flag(cpu.y);
        cpu.update_zero_flag(cpu.y);
    }

    fn store_a<M: Mcu>(cpu: &mut Cpu2<M>) {
        cpu.write_byte(cpu.ab, cpu.a);
    }

    fn store_x<M: Mcu>(cpu: &mut Cpu2<M>) {
        cpu.write_byte(cpu.ab, cpu.x);
    }

    fn store_y<M: Mcu>(cpu: &mut Cpu2<M>) {
        cpu.write_byte(cpu.ab, cpu.y);
    }

    fn load_immediate_a<M: Mcu>(cpu: &mut Cpu2<M>) {
        cpu.a = cpu.inc_read_byte();
        cpu.update_negative_flag(cpu.a);
        cpu.update_zero_flag(cpu.a);
    }

    fn and_immediate<M: Mcu>(cpu: &mut Cpu2<M>) {
        let value = cpu.inc_read_byte();
        cpu.and(value);
    }

    fn zero_page<M: Mcu>(cpu: &mut Cpu2<M>, load_into_alu: bool) {
        let addr = cpu.inc_read_byte();
        cpu.ab = addr as u16;
        if load_into_alu {
            cpu.load_alu();
        }
    }

    fn zero_page_indexed_x<M: Mcu>(cpu: &mut Cpu2<M>, load_into_alu: bool) {
        cpu.ab = cpu.abl().wrapping_add(cpu.x) as u16;
        if load_into_alu {
            cpu.load_alu();
        }
    }

    fn absolute_l<M: Mcu>(cpu: &mut Cpu2<M>) {
        let low = cpu.inc_read_byte();
        cpu.set_abl(low);
    }

    fn absolute_h<M: Mcu>(cpu: &mut Cpu2<M>, load_into_alu: bool) {
        let high = cpu.inc_read_byte();
        cpu.set_abh(high);
        if load_into_alu {
            cpu.load_alu();
        }
    }

    fn adc_immediate<M: Mcu>(cpu: &mut Cpu2<M>) {
        let value = cpu.inc_read_byte();
        cpu.adc(value);
    }

    fn absolute_indexed_x<M: Mcu>(cpu: &mut Cpu2<M>, oops: bool, load_into_alu: bool) {
        let abh = cpu.inc_read_byte();
        cpu.set_abh(abh);
        cpu.ab = cpu.ab.wrapping_add(cpu.x as u16);
        if load_into_alu {
            cpu.load_alu();
        }
        if oops && abh != cpu.abh() {
            cpu.retain_cycle();
        }
    }

    fn absolute_indexed_y<M: Mcu>(cpu: &mut Cpu2<M>, oops: bool, load_into_alu: bool) {
        let abh = cpu.inc_read_byte();
        cpu.set_abh(abh);
        Self::absolute_indexed_y_without_high(cpu, oops, load_into_alu);
    }

    fn absolute_indexed_y_without_high<M: Mcu>(cpu: &mut Cpu2<M>, oops: bool, load_into_alu: bool) {
        let abh = cpu.abh();
        cpu.ab = cpu.ab.wrapping_add(cpu.y as u16);
        if oops && abh != cpu.abh() {
            cpu.retain_cycle();
        }
        if load_into_alu {
            cpu.load_alu();
        }
    }

    fn adc<M: Mcu>(cpu: &mut Cpu2<M>) {
        let value = cpu.read_byte(cpu.ab);
        cpu.adc(value);
    }

    fn and<M: Mcu>(cpu: &mut Cpu2<M>) {
        let value = cpu.read_byte(cpu.ab);
        cpu.and(value);
    }

    fn store_alu<M: Mcu>(cpu: &mut Cpu2<M>) {
        cpu.write_byte(cpu.ab, cpu.alu);
    }

    fn indexed<M: Mcu>(cpu: &mut Cpu2<M>, load_into_alu: bool) {
        let addr_low = cpu.read_byte(cpu.ab);
        let addr_high = cpu.read_byte(cpu.ab.wrapping_add(1));
        cpu.ab = (addr_high as u16) << 8 | addr_low as u16;
        if load_into_alu {
            cpu.alu = cpu.read_byte(cpu.ab);
        }
    }

    fn asl_accumulator<M: Mcu>(cpu: &mut Cpu2<M>) {
        cpu.a = cpu.asl(cpu.a);
    }

    fn asl<M: Mcu>(cpu: &mut Cpu2<M>) {
        cpu.alu = cpu.asl(cpu.alu);
    }
}

/// Push Microcodes for zero page indexed x addressing
fn zero_page_indexed_x_addressing<M: Mcu>(cpu: &mut Cpu2<M>, load_into_alu: bool) {
    cpu.push_microcode(Microcode::ZeroPage {
        load_into_alu: false,
    });
    cpu.push_microcode(Microcode::ZeroPageIndexedX { load_into_alu });
}

/// Push Microcodes for absolute addressing
fn absolute_addressing<M: Mcu>(cpu: &mut Cpu2<M>, load_into_alu: bool) {
    cpu.push_microcode(Microcode::AbsoluteL);
    cpu.push_microcode(Microcode::AbsoluteH { load_into_alu });
}

/// Push Microcodes for absolute indexed x addressing
fn absolute_indexed_x_addressing<M: Mcu>(cpu: &mut Cpu2<M>, oops: bool, load_into_alu: bool) {
    cpu.push_microcode(Microcode::AbsoluteL);
    cpu.push_microcode(Microcode::AbsoluteIndexedX {
        oops,
        load_into_alu,
    }); // may add nop for extra cycle
}

/// Push Microcodes for absolute indexed y addressing
fn absolute_indexed_y_addressing<M: Mcu>(cpu: &mut Cpu2<M>, oops: bool, load_into_alu: bool) {
    cpu.push_microcode(Microcode::AbsoluteL);
    cpu.push_microcode(Microcode::AbsoluteIndexedY {
        oops,
        load_into_alu,
    }); // may add nop for extra cycle
}

/// Push Microcodes for zero page indexed indirect addressing
fn indexed_indirect_addressing<M: Mcu>(cpu: &mut Cpu2<M>, load_into_alu: bool) {
    cpu.push_microcode(Microcode::ZeroPage {
        load_into_alu: false,
    });
    cpu.push_microcode(Microcode::ZeroPageIndexedX {
        load_into_alu: false,
    });
    cpu.push_microcode(Microcode::Indexed { load_into_alu });
    cpu.push_microcode(Microcode::Nop);
}

/// Push Microcodes for indirect indexed addressing
fn indirect_indexed_addressing<M: Mcu>(cpu: &mut Cpu2<M>, oops: bool, load_into_alu: bool) {
    cpu.push_microcode(Microcode::ZeroPage {
        load_into_alu: false,
    });
    cpu.push_microcode(Microcode::Indexed {
        load_into_alu: false,
    });
    cpu.push_microcode(Microcode::AbsoluteIndexedYWithoutHigh {
        oops,
        load_into_alu,
    }); // may add nop for extra cycle
}

#[cfg(test)]
mod tests;
