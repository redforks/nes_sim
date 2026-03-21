use crate::{
    cpu::{Cpu2, Flag},
    mcu::Mcu,
};

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

    // addressing microcodes
    /// Take a byte from instruction data stream, set cpu address_bus field by read from memory use zero page addressing
    ZeroPage,
    /// Impl ZeroPageIndexedX addressing, work after ZeroPage, set abl = (abl + x) % 256, set abh = 00
    ZeroPageIndexedX,
    /// Take a byte from instruction data stream, set cpu abl field
    AbsoluteL,
    /// Take a byte from instruction data stream, set cpu abh field
    AbsoluteH,
    /// Take a byte from instruction data stream, set cpu abh field, add ab with x register value, set cpu ab field, retain_cycle if the 8bit plus operation overflows
    AbsoluteIndexedX,
    /// Take a byte from instruction data stream, set cpu abh field, add ab with y register value, set cpu ab field, retain_cycle if the 8bit plus operation overflows
    AbsoluteIndexedY,
    /// Do nothing, used in "oops" cycles of AbsoluteIndexed and Indirect Indexed addressing
    Nop,

    /// Take immediate value from instruction data stream, add to accumulator with carry
    AdcImmediate,
    /// Fetch a byte from memory use address saved in `data_latch`, then add to accumulator with carry
    Adc,
}

/// Represents the opcode of a CPU instruction, the first byte of an instruction
pub enum Opcode {}

impl Opcode {
    pub const ADC_IMMEDIATE: u8 = 0x69;
    pub const ADC_ZERO_PAGE: u8 = 0x65;
    pub const ADC_ZERO_PAGE_X: u8 = 0x75;
    pub const ADC_ABSOLUTE: u8 = 0x6D;
    pub const ADC_ABSOLUTE_INDEXED_X: u8 = 0x7D;
    pub const ADC_ABSOLUTE_INDEXED_Y: u8 = 0x79;
    pub const ADC_INDEXED_INDIRECT: u8 = 0x61;
    pub const ADC_INDIRECT_INDEXED: u8 = 0x71;
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
            Microcode::ZeroPage => Self::zero_page(cpu),
            Microcode::ZeroPageIndexedX => Self::zero_page_indexed_x(cpu),
            Microcode::AbsoluteL => Self::absolute_l(cpu),
            Microcode::AbsoluteH => Self::absolute_h(cpu),
            Microcode::AbsoluteIndexedX => Self::absolute_indexed_x(cpu),
            Microcode::AbsoluteIndexedY => Self::absolute_indexed_y(cpu),
            Microcode::AdcImmediate => Self::adc_immediate(cpu),
            Microcode::Adc => Self::adc(cpu),
            _ => unimplemented!("Unimplementd microcode: {:?}", self),
        }
    }

    fn fetch_and_decode<M: Mcu>(cpu: &mut Cpu2<M>) {
        let opcode = cpu.inc_read_byte();
        cpu.opcode = opcode;
        match opcode {
            Opcode::ADC_IMMEDIATE => {
                cpu.push_microcode(Microcode::AdcImmediate);
            }
            Opcode::ADC_ZERO_PAGE => {
                cpu.push_microcode(Microcode::ZeroPage);
                cpu.push_microcode(Microcode::Adc);
            }
            Opcode::ADC_ZERO_PAGE_X => {
                cpu.push_microcode(Microcode::ZeroPage);
                cpu.push_microcode(Microcode::ZeroPageIndexedX);
                cpu.push_microcode(Microcode::Adc);
            }
            Opcode::ADC_ABSOLUTE => {
                cpu.push_microcode(Microcode::AbsoluteL);
                cpu.push_microcode(Microcode::AbsoluteH);
                cpu.push_microcode(Microcode::Adc);
            }
            Opcode::ADC_ABSOLUTE_INDEXED_X => {
                cpu.push_microcode(Microcode::AbsoluteL);
                cpu.push_microcode(Microcode::AbsoluteIndexedX);
                cpu.push_microcode(Microcode::Adc);
            }
            Opcode::ADC_ABSOLUTE_INDEXED_Y => {
                cpu.push_microcode(Microcode::AbsoluteL);
                cpu.push_microcode(Microcode::AbsoluteIndexedY);
                cpu.push_microcode(Microcode::Adc);
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

    fn zero_page<M: Mcu>(cpu: &mut Cpu2<M>) {
        let addr = cpu.inc_read_byte();
        cpu.ab = addr as u16;
    }

    fn zero_page_indexed_x<M: Mcu>(cpu: &mut Cpu2<M>) {
        cpu.ab = cpu.abl().wrapping_add(cpu.x) as u16;
    }

    fn absolute_l<M: Mcu>(cpu: &mut Cpu2<M>) {
        let low = cpu.inc_read_byte();
        cpu.set_abl(low);
    }

    fn absolute_h<M: Mcu>(cpu: &mut Cpu2<M>) {
        let high = cpu.inc_read_byte();
        cpu.set_abh(high);
    }

    fn adc_immediate<M: Mcu>(cpu: &mut Cpu2<M>) {
        let value = cpu.inc_read_byte();
        cpu.adc(value);
    }

    fn absolute_indexed_x<M: Mcu>(cpu: &mut Cpu2<M>) {
        let abh = cpu.inc_read_byte();
        cpu.set_abh(abh);
        cpu.ab = cpu.ab.wrapping_add(cpu.x as u16);
        if abh != cpu.abh() {
            cpu.retain_cycle();
        }
    }

    fn absolute_indexed_y<M: Mcu>(cpu: &mut Cpu2<M>) {
        let abh = cpu.inc_read_byte();
        cpu.set_abh(abh);
        cpu.ab = cpu.ab.wrapping_add(cpu.y as u16);
        if abh != cpu.abh() {
            cpu.retain_cycle();
        }
    }

    fn adc<M: Mcu>(cpu: &mut Cpu2<M>) {
        let value = cpu.read_byte(cpu.ab);
        cpu.adc(value);
    }
}

#[cfg(test)]
mod tests;
