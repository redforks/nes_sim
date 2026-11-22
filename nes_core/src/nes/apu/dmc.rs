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
    dma_request: Option<(DmcDmaType, u16)>,
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

    fn request_dma(&mut self, kind: DmcDmaType) {
        if self.dma_request.is_none() && self.enabled() && !self.dma_pending {
            self.dma_pending = true;
            self.dma_request = Some((kind, self.current_address));
        }
    }

    fn take_dma_request(&mut self) -> Option<(DmcDmaType, u16)> {
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
        if let Some(byte) = self.sample_buffer {
            self.sample_buffer = None;
            self.silence_flag = false;
            self.shift_register = byte;
        } else {
            self.silence_flag = true;
        }
    }

    fn tick(&mut self) {
        if self.remain_bits == 0 {
            self.start_cycle();
        }

        if !self.silence_flag {
            self.output = if self.shift_register & 1 == 0 {
                self.output.saturating_sub(2)
            } else if self.output < 126 {
                self.output + 2
            } else {
                self.output
            };
            self.shift_register >>= 1;
        }
        self.remain_bits -= 1;
    }

    fn is_buffer_empty(&self) -> bool {
        self.sample_buffer.is_none()
    }
}

#[derive(Debug)]
pub struct Dmc {
    dma_reader: DmaReader,
    output: OutputUnit,
    timer: Divider<u16>,
    /// Total $4011 writes. The Zapper test ROMs signal per-frame progress
    /// with DAC-register "clicks"; the harness counts them.
    dac_writes: u64,
}

/// Full [`Dmc`] channel state for save-states (ticket #184).
#[derive(Debug, Clone, Copy)]
pub(crate) struct DmcDmaRequestState {
    /// `true` = `Load`, `false` = `Reload`.
    pub is_load: bool,
    pub addr: u16,
}

/// Full [`Dmc`] channel state for save-states (ticket #184).
#[derive(Debug, Clone)]
pub(crate) struct DmcState {
    pub sample_address: u8,
    pub sample_length: u8,
    pub enable_loop: bool,
    pub irq_enabled: bool,
    pub current_address: u16,
    pub bytes_remaining: u16,
    pub request_interrupt: bool,
    pub dma_pending: bool,
    pub dma_request: Option<DmcDmaRequestState>,
    pub shift_register: u8,
    pub remain_bits: u8,
    pub silence_flag: bool,
    pub output: u8,
    pub sample_buffer: Option<u8>,
    pub timer_period: u16,
    pub timer_counter: u16,
    pub dac_writes: u64,
}

impl Dmc {
    /// Capture the full channel state.
    pub(crate) fn capture_dmc_state(&self) -> DmcState {
        DmcState {
            sample_address: self.dma_reader.sample_address,
            sample_length: self.dma_reader.sample_length,
            enable_loop: self.dma_reader.enable_loop,
            irq_enabled: self.dma_reader.irq_enabled,
            current_address: self.dma_reader.current_address,
            bytes_remaining: self.dma_reader.bytes_remaining,
            request_interrupt: self.dma_reader.request_interrupt,
            dma_pending: self.dma_reader.dma_pending,
            dma_request: self
                .dma_reader
                .dma_request
                .map(|(kind, addr)| DmcDmaRequestState {
                    is_load: matches!(kind, DmcDmaType::Load),
                    addr,
                }),
            shift_register: self.output.shift_register,
            remain_bits: self.output.remain_bits,
            silence_flag: self.output.silence_flag,
            output: self.output.output,
            sample_buffer: self.output.sample_buffer,
            timer_period: self.timer.period(),
            timer_counter: self.timer.counter(),
            dac_writes: self.dac_writes,
        }
    }

    /// Restore a state previously produced by [`Self::capture_dmc_state`].
    pub(crate) fn restore_dmc_state(&mut self, s: &DmcState) {
        self.dma_reader.sample_address = s.sample_address;
        self.dma_reader.sample_length = s.sample_length;
        self.dma_reader.enable_loop = s.enable_loop;
        self.dma_reader.irq_enabled = s.irq_enabled;
        self.dma_reader.current_address = s.current_address;
        self.dma_reader.bytes_remaining = s.bytes_remaining;
        self.dma_reader.request_interrupt = s.request_interrupt;
        self.dma_reader.dma_pending = s.dma_pending;
        self.dma_reader.dma_request = s.dma_request.map(|r| {
            let kind = if r.is_load {
                DmcDmaType::Load
            } else {
                DmcDmaType::Reload
            };
            (kind, r.addr)
        });
        self.output.shift_register = s.shift_register;
        self.output.remain_bits = s.remain_bits;
        self.output.silence_flag = s.silence_flag;
        self.output.output = s.output;
        self.output.sample_buffer = s.sample_buffer;
        self.timer.restore(s.timer_period, s.timer_counter);
        self.dac_writes = s.dac_writes;
    }
}

impl Default for Dmc {
    fn default() -> Self {
        Self {
            dma_reader: DmaReader::default(),
            output: OutputUnit::default(),
            // $4010 powers up with rate 0; mirror the write path's
            // minus-one convention so pre-write playback runs at the
            // documented 428-CPU-cycle rate.
            timer: Divider::new(DMC_RATE_TABLE[0] - 1),
            dac_writes: 0,
        }
    }
}

impl Dmc {
    pub fn set_enabled(&mut self, enabled: bool) {
        // Always clear DMC interrupt on $4015 write
        self.clear_interrupt_flag();

        self.dma_reader.set_enabled(enabled);
        if enabled && self.output.is_buffer_empty() {
            // Load DMA: triggered by a $4015 write with the buffer empty.
            self.dma_reader.request_dma(DmcDmaType::Load);
        }
    }

    pub fn status_bit(&self) -> bool {
        self.dma_reader.enabled()
    }

    #[inline(always)]
    pub fn tick_timer(&mut self) {
        if self.timer.tick() {
            self.output.tick();
            if self.output.is_buffer_empty() {
                self.dma_reader.request_dma(DmcDmaType::Reload);
            }
        }
    }

    pub fn supply_dma_byte(&mut self, byte: u8) {
        self.output.sample_buffer = Some(byte);
        self.dma_reader.inc_addr();
    }

    pub fn output(&self) -> u8 {
        self.output.output
    }

    pub fn write_dac(&mut self, value: DmcDacBits) {
        self.dac_writes += 1;
        self.output.output = value.dac_value();
    }

    pub fn dac_writes(&self) -> u64 {
        self.dac_writes
    }

    pub fn write_sample_address(&mut self, value: u8) {
        self.dma_reader.sample_address = value;
    }

    pub fn write_sample_length(&mut self, value: u8) {
        self.dma_reader.sample_length = value;
    }

    pub fn take_dma_request(&mut self) -> Option<(DmcDmaType, u16)> {
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
