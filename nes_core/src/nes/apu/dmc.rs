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
        if self.enabled() && !self.dma_pending {
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

#[derive(Debug, Default)]
struct OutputUnit {
    shift_register: u8,
    remain_bits: u8,
    silence_flag: bool,
    output: u8,
    sample_buffer: Option<u8>,
}

impl OutputUnit {
    fn start_cycle(&mut self) {
        self.remain_bits = 8;
        if let Some(byte) = self.sample_buffer.take() {
            self.silence_flag = false;
            self.shift_register = byte;
        } else {
            self.silence_flag = true;
        }
    }

    fn receive_sample(&mut self, sample: u8) {
        self.sample_buffer = Some(sample);
    }

    fn set_output(&mut self, v: u8) {
        self.output = v;
    }

    fn tick(&mut self) {
        if self.remain_bits == 0 {
            self.start_cycle();
        }

        if !self.silence_flag {
            self.output = match self.shift_register & 0x1 {
                0 => self.output.saturating_sub(2),
                1 => {
                    if self.output < 126 {
                        self.output + 2
                    } else {
                        self.output
                    }
                }
                _ => self.output,
            };
            self.shift_register >>= 1;
        }
        self.remain_bits -= 1;
    }

    fn is_buffer_empty(&self) -> bool {
        self.sample_buffer.is_none()
    }

    fn clear(&mut self) {
        // If the DMC bit is clear, the DMC bytes remaining will be set to 0 and the DMC will silence when it empties.
        self.sample_buffer = None;
        self.output = 0;
        self.remain_bits = 0;
    }
}

#[derive(Debug)]
pub struct Dmc {
    dma_reader: DmaReader,
    output: OutputUnit,
    timer: Divider<u16>,
}

impl Default for Dmc {
    fn default() -> Self {
        Self {
            dma_reader: DmaReader::default(),
            output: OutputUnit::default(),
            timer: Divider::new(DMC_RATE_TABLE[0]),
        }
    }
}

impl Dmc {
    pub fn set_enabled(&mut self, enabled: bool) {
        // Always clear DMC interrupt on $4015 write
        self.clear_interrupt_flag();

        self.dma_reader.set_enabled(enabled);
        if !enabled {
            self.output.clear();
        } else if self.output.is_buffer_empty() {
            self.dma_reader.check_and_request_dma();
        };
    }

    pub fn status_bit(&self) -> bool {
        self.dma_reader.enabled()
    }

    pub fn tick_timer(&mut self) {
        if self.timer.tick() {
            self.output.tick();
            if self.output.is_buffer_empty() {
                self.dma_reader.tick();
            }
        }
    }

    pub fn supply_dma_byte(&mut self, byte: u8) {
        self.output.receive_sample(byte);
        self.dma_reader.inc_addr();
    }

    pub fn output(&self) -> u8 {
        self.output.output
    }

    pub fn write_dac(&mut self, value: DmcDacBits) {
        self.output.set_output(value.dac_value());
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
        self.dma_reader.config(value);
        self.timer
            .set_period(DMC_RATE_TABLE[value.freq() as usize] - 1);
    }
}
