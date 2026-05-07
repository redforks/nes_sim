use super::*;

const DMC_RATE_TABLE: [u16; 16] = [
    428, 380, 340, 320, 286, 254, 226, 214, 190, 160, 142, 128, 106, 84, 72, 54,
];

#[derive(Debug, Default)]
struct DmaReader {
    sample_address: u8,
    sample_length: u8,
    enable_loop: bool,
    irq_enabled: bool,

    current_address: u16,
    bytes_remaining: u16,
    request_interrupt: bool,

    dma_pending: bool,
    dma_request: Option<(u16, bool)>,
}

impl DmaReader {
    fn restart_sample(&mut self) {
        self.current_address = 0xC000 + ((self.sample_address as u16) * 0x40);
        self.bytes_remaining = (self.sample_length as u16) * 0x10 + 1;
    }

    fn inc_addr(&mut self) {
        self.dma_pending = false;

        self.current_address = match self.current_address {
            0xFFFF => 0x8000,
            _ => self.current_address + 1,
        };

        self.bytes_remaining -= 1;
        if self.bytes_remaining == 0 {
            if self.enable_loop {
                self.restart_sample();
            } else if self.irq_enabled {
                self.request_interrupt = true;
            }
        }
    }

    fn set_enabled(&mut self, enabled: bool) {
        if !enabled {
            self.bytes_remaining = 0;
        } else if self.bytes_remaining == 0 {
            self.restart_sample();
        }
    }

    fn enabled(&self) -> bool {
        self.bytes_remaining != 0
    }

    fn tick(&mut self) {
        if self.bytes_remaining > 0 && !self.dma_pending {
            self.dma_pending = true;
            self.dma_request = Some((self.current_address, true));
        }
    }

    fn check_and_request_dma(&mut self) {
        // Load DMA: triggered by $4015 write with empty buffer
        if self.dma_request.is_none() && self.enabled() && !self.dma_pending {
            self.dma_pending = true;
            self.dma_request = Some((self.current_address, false));
        }
    }

    fn take_dma_request(&mut self) -> Option<(u16, bool)> {
        self.dma_request.take()
    }

    fn clear_irq(&mut self) {
        self.request_interrupt = false;
    }

    fn config(&mut self, value: DmcIRQLoopFreq) {
        self.enable_loop = value.loop_flag();
        self.irq_enabled = value.irq_enabled();
        if !self.irq_enabled {
            self.clear_irq();
        }
    }
}

#[derive(Debug)]
pub struct Dmc {
    dma_reader: DmaReader,
    irq_loop_freq: DmcIRQLoopFreq,
    load_counter: u8,
    sample_buffer: Option<u8>,
    shift_register: u8,
    bits_remaining: u8,
    output_level: u8,
    silence_flag: bool,
    timer: Divider<u16>,
}

impl Default for Dmc {
    fn default() -> Self {
        Self {
            dma_reader: DmaReader::default(),
            irq_loop_freq: DmcIRQLoopFreq::default(),
            load_counter: 0,
            sample_buffer: None,
            shift_register: 0,
            bits_remaining: 0,
            output_level: 0,
            silence_flag: true,
            timer: Divider::new(DMC_RATE_TABLE[0]),
        }
    }
}

impl Dmc {
    pub fn set_enabled(&mut self, enabled: bool) {
        self.dma_reader.set_enabled(enabled);
    }

    pub fn status_bit(&self) -> bool {
        self.dma_reader.enabled()
    }

    pub fn tick(&mut self) {
        if self.timer.tick() {
            self.timer
                .set_period_and_reset(DMC_RATE_TABLE[self.irq_loop_freq.freq() as usize] - 1);
            self.clock_output_unit();
            if self.sample_buffer.is_none() {
                self.dma_reader.tick();
            }
        }
    }

    pub fn check_and_request_dma(&mut self) {
        // Load DMA: triggered by $4015 write with empty buffer
        if self.sample_buffer.is_none() {
            self.dma_reader.check_and_request_dma();
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
        self.dma_reader.inc_addr();
    }

    pub fn output(&self) -> u8 {
        self.output_level
    }

    pub fn write_load_counter(&mut self, value: DmcLoadCounter) {
        self.load_counter = value.load_counter();
        self.output_level = self.load_counter;
    }

    pub fn write_sample_address(&mut self, value: u8) {
        self.dma_reader.sample_address = value;
    }

    pub fn write_sample_length(&mut self, value: u8) {
        self.dma_reader.sample_length = value;
    }

    pub fn take_dma_request(&mut self) -> Option<(u16, bool)> {
        self.dma_reader.take_dma_request()
    }

    pub fn interrupt_flag(&self) -> bool {
        self.dma_reader.request_interrupt
    }

    pub fn clear_interrupt_flag(&mut self) {
        self.dma_reader.clear_irq();
    }

    pub fn write_dmc_irq_loop_freq(&mut self, value: DmcIRQLoopFreq) {
        self.irq_loop_freq = value;
        self.dma_reader.config(value);
        self.timer
            .set_period(DMC_RATE_TABLE[self.irq_loop_freq.freq() as usize] - 1);
    }
}
