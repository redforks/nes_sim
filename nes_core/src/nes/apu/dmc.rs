use super::*;

const DMC_RATE_TABLE: [u16; 16] = [
    428, 380, 340, 320, 286, 254, 226, 214, 190, 160, 142, 128, 106, 84, 72, 54,
];

#[derive(Debug)]
pub struct Dmc {
    irq_loop_freq: DmcIRQLoopFreq,
    load_counter: u8,
    sample_address: u8,
    sample_length: u8,
    current_address: u16,
    bytes_remaining: u16,
    sample_buffer: Option<u8>,
    shift_register: u8,
    bits_remaining: u8,
    output_level: u8,
    silence_flag: bool,
    timer: Divider<u16>,
    dma_pending: bool,
    interrupt_flag: bool,
    dma_request: Option<(u16, bool)>,
}

impl Default for Dmc {
    fn default() -> Self {
        Self {
            irq_loop_freq: DmcIRQLoopFreq::default(),
            load_counter: 0,
            sample_address: 0,
            sample_length: 0,
            current_address: 0,
            bytes_remaining: 0,
            sample_buffer: None,
            shift_register: 0,
            bits_remaining: 0,
            output_level: 0,
            silence_flag: true,
            timer: Divider::new(DMC_RATE_TABLE[0]),
            dma_pending: false,
            interrupt_flag: false,
            dma_request: None,
        }
    }
}

impl Dmc {
    pub fn set_enabled(&mut self, enabled: bool) {
        if !enabled {
            self.bytes_remaining = 0;
        } else if self.bytes_remaining == 0 {
            self.restart_sample();
        }
    }

    pub fn status_bit(&self) -> bool {
        self.bytes_remaining > 0
    }

    fn restart_sample(&mut self) {
        self.current_address = 0xC000 | ((self.sample_address as u16) << 6);
        self.bytes_remaining = (self.sample_length as u16) * 16 + 1;
    }

    pub fn tick(&mut self) {
        if self.timer.tick() {
            self.timer.set_period_and_reset(DMC_RATE_TABLE[self.irq_loop_freq.freq() as usize] - 1);
            self.clock_output_unit();
            if self.sample_buffer.is_none() && self.bytes_remaining > 0 && !self.dma_pending {
                self.dma_pending = true;
                self.dma_request = Some((self.current_address, true));
            }
        }
    }

    pub fn check_and_request_dma(&mut self) {
        // Load DMA: triggered by $4015 write with empty buffer
        if self.dma_request.is_none()
            && self.sample_buffer.is_none()
            && self.bytes_remaining > 0
            && !self.dma_pending
        {
            self.dma_pending = true;
            self.dma_request = Some((self.current_address, false));
        }
    }

    fn clock_output_unit(&mut self) {
        if self.bits_remaining == 0 {
            self.bits_remaining = 8;
            if let Some(byte) = self.sample_buffer.take() {
                self.silence_flag = false;
                self.shift_register = byte;
            } else {
                self.silence_flag = true;
            }
        }

        if !self.silence_flag {
            if self.shift_register & 1 != 0 {
                if self.output_level <= 125 {
                    self.output_level += 2;
                }
            } else if self.output_level >= 2 {
                self.output_level -= 2;
            }
        }
        self.shift_register >>= 1;
        self.bits_remaining -= 1;
    }

    pub fn supply_dma_byte(&mut self, byte: u8) {
        self.sample_buffer = Some(byte);
        self.dma_pending = false;

        self.current_address = if self.current_address == 0xFFFF {
            0x8000
        } else {
            self.current_address + 1
        };

        self.bytes_remaining -= 1;

        if self.bytes_remaining == 0 {
            if self.irq_loop_freq.loop_flag() {
                self.restart_sample();
            } else if self.irq_loop_freq.irq_enabled() {
                self.interrupt_flag = true;
            }
        }
    }

    pub fn output(&self) -> u8 {
        self.output_level
    }

    pub fn write_load_counter(&mut self, value: DmcLoadCounter) {
        self.load_counter = value.load_counter();
        self.output_level = self.load_counter;
    }

    pub fn write_sample_address(&mut self, value: u8) {
        self.sample_address = value;
    }

    pub fn write_sample_length(&mut self, value: u8) {
        self.sample_length = value;
    }

    pub fn take_dma_request(&mut self) -> Option<(u16, bool)> {
        self.dma_request.take()
    }

    pub fn interrupt_flag(&self) -> bool {
        self.interrupt_flag
    }

    pub fn clear_interrupt_flag(&mut self) {
        self.interrupt_flag = false;
    }

    pub fn write_dmc_irq_loop_freq(&mut self, value: DmcIRQLoopFreq) {
        self.irq_loop_freq = value;
        if !self.irq_loop_freq.irq_enabled() {
            self.interrupt_flag = false;
        }
    }
}
