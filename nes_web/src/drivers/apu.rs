use nes_core::nes::apu::{
    self, APUStatus, ControlFlags, DmcIRQLoopFreq, DutyCycle, FrameCounter, LengthCounterLoad,
    LinearCounterControl, NoiseEnvelop, NoiseLength, NoisePeriod, Sweep,
};

// empty PulseDriver impl
pub struct PulseDriver();

impl apu::PulseDriver for PulseDriver {
    fn set_duty_cycle(&mut self, _: DutyCycle) {
        todo!()
    }

    fn set_sweep(&mut self, _: Sweep) {
        todo!()
    }

    fn set_length_counter_load(&mut self, _: LengthCounterLoad) {
        todo!()
    }
}

// empty TriangleDriver impl
pub struct TriangleDriver();

impl apu::TriangleDriver for TriangleDriver {
    fn set_linear_counter_control(&mut self, _: LinearCounterControl) {
        todo!()
    }

    fn set_length_counter_load(&mut self, _: LengthCounterLoad) {
        todo!()
    }
}

// empty NoiseDriver impl
pub struct NoiseDriver();

impl apu::NoiseDriver for NoiseDriver {
    fn set_envelop(&mut self, _: NoiseEnvelop) {
        todo!()
    }

    fn set_period(&mut self, _: NoisePeriod) {
        todo!()
    }

    fn set_length(&mut self, _: NoiseLength) {
        todo!()
    }
}

// empty DmcDriver impl
pub struct DmcDriver();

impl apu::DmcDriver for DmcDriver {
    fn set_irq_loop_freq(&mut self, _: DmcIRQLoopFreq) {
        todo!()
    }

    fn set_load_counter(&mut self, _: u8) {
        todo!()
    }

    fn set_sample_address(&mut self, _: u8) {
        todo!()
    }

    fn set_sample_length(&mut self, _: u8) {
        todo!()
    }
}

// empty ApuControlDriver impl
pub struct ApuControlDriver();

impl apu::APUControllerDriver for ApuControlDriver {
    fn set_control_flags(&mut self, _: ControlFlags) {
        todo!()
    }

    fn set_frame_counter(&mut self, _: FrameCounter) {
        todo!()
    }

    fn read_status(&self) -> APUStatus {
        todo!()
    }
}
