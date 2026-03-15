use crate::mcu::Mcu;
use crate::{Cpu, EmptyPlugin, ExecuteResult, Plugin};
use image::RgbaImage;

pub struct Machine<P> {
    cpu: Cpu,
    p: P,
}

impl Machine<EmptyPlugin> {
    pub fn new(mcu: Box<dyn Mcu>) -> Self {
        Self::with_plugin(EmptyPlugin(), mcu)
    }
}

pub const CYCLES_PER_FRAME: f64 = 29780.5;
pub const CYCLES_PER_MS: f64 = CYCLES_PER_FRAME / 100.0 * 60.0;
pub const V_BLANK_CYCLES: f64 = 2273.3333333;

impl<P: Plugin> Machine<P> {
    pub fn with_plugin(p: P, mcu: Box<dyn Mcu>) -> Self {
        Machine {
            // cpu: Cpu::new(Box::new(create_mcu(&ines))),
            cpu: Cpu::new(mcu),
            p,
        }
    }

    /// Run the machine for a single frame.
    /// `ms`: milliseconds elapsed since last frame.
    pub fn process_frame(&mut self, ms: f64) -> (&RgbaImage, ExecuteResult) {
        let cycles = (ms * CYCLES_PER_MS) as u32;

        let er = self.run_ticks(cycles);

        (self.cpu.mcu().get_machine_mcu().render(), er)
    }

    pub fn run_ticks(&mut self, ticks: u32) -> ExecuteResult {
        for _ in 0..ticks {
            match self.cpu.clock_tick(&mut self.p) {
                ExecuteResult::Continue => continue,
                other => return other,
            }
        }
        ExecuteResult::Continue
    }

    pub fn reset(&mut self) {
        self.cpu.reset()
    }

    pub fn set_pc(&mut self, pc: u16) {
        self.cpu.pc = pc;
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    struct MockMcu {
        memory: [u8; 0x10000],
    }

    impl MockMcu {
        fn new() -> Self {
            MockMcu {
                memory: [0; 0x10000],
            }
        }
    }

    impl Mcu for MockMcu {
        fn read(&self, addr: u16) -> u8 {
            self.memory[addr as usize]
        }

        fn write(&mut self, addr: u16, value: u8) {
            self.memory[addr as usize] = value;
        }

        fn get_machine_mcu(&mut self) -> &mut dyn crate::mcu::MachineMcu {
            self
        }

        fn request_irq(&self) -> bool {
            false
        }
    }

    impl crate::mcu::MachineMcu for MockMcu {
        fn render(&mut self) -> &RgbaImage {
            use std::sync::OnceLock;
            static IMAGE: OnceLock<RgbaImage> = OnceLock::new();
            IMAGE.get_or_init(|| RgbaImage::new(256, 240))
        }
    }

    #[test]
    fn test_machine_creation() {
        let mcu = Box::new(MockMcu::new());
        let machine = Machine::new(mcu);
        assert_eq!(machine.cpu.pc, 0);
    }

    #[test]
    fn test_machine_with_plugin() {
        let mcu = Box::new(MockMcu::new());
        let machine = Machine::with_plugin(EmptyPlugin(), mcu);
        assert_eq!(machine.cpu.pc, 0);
    }

    #[test]
    fn test_set_pc() {
        let mcu = Box::new(MockMcu::new());
        let mut machine = Machine::new(mcu);

        machine.set_pc(0x1234);
        assert_eq!(machine.cpu.pc, 0x1234);
    }

    #[test]
    fn test_reset() {
        let mcu = Box::new(MockMcu::new());
        let mut machine = Machine::new(mcu);

        machine.set_pc(0x5678);
        machine.reset();
        // Reset reads PC from 0xFFFC
        assert_eq!(machine.cpu.pc, 0);
    }

    #[test]
    fn test_run_ticks_zero() {
        let mcu = Box::new(MockMcu::new());
        let mut machine = Machine::new(mcu);

        let result = machine.run_ticks(0);
        assert_eq!(result, ExecuteResult::Continue);
    }

    #[test]
    fn test_constants_values() {
        // Verify the constants have reasonable values
        assert!(CYCLES_PER_FRAME > 0.0);
        assert!(CYCLES_PER_MS > 0.0);
        assert!(V_BLANK_CYCLES > 0.0);

        // CYCLES_PER_MS should be derived from CYCLES_PER_FRAME
        let calculated = CYCLES_PER_FRAME / 100.0 * 60.0;
        assert!((CYCLES_PER_MS - calculated).abs() < 0.1);
    }

    #[test]
    fn test_cycles_per_ms_calculation() {
        // Just verify that the constants produce reasonable cycle counts
        let ms = 1.0;
        let cycles = (ms * CYCLES_PER_MS) as u32;

        // Should be positive and reasonable (not 0)
        assert!(cycles > 0);
        assert!(cycles < 1_000_000);
    }

    #[test]
    fn test_process_frame() {
        let mcu = Box::new(MockMcu::new());
        let mut machine = Machine::new(mcu);

        // Run for 1 ms (about 17868 cycles)
        let (img, result) = machine.process_frame(1.0);

        // Should return an image and continue result
        assert_eq!(img.dimensions(), (256, 240));
        assert_eq!(result, ExecuteResult::Continue);
    }

    #[test]
    fn test_process_frame_with_zero_ms() {
        let mcu = Box::new(MockMcu::new());
        let mut machine = Machine::new(mcu);

        // Run for 0 ms
        let (img, result) = machine.process_frame(0.0);

        // Should still return an image
        assert_eq!(img.dimensions(), (256, 240));
        assert_eq!(result, ExecuteResult::Continue);
    }

    #[test]
    fn test_run_ticks_positive() {
        let mut mcu = MockMcu::new();
        // Write a JMP instruction to create an infinite loop
        // JMP $8000 = 0x4C 0x00 0x80 at address 0x8000
        mcu.memory[0x8000] = 0x4C; // JMP absolute
        mcu.memory[0x8001] = 0x00; // low byte of address
        mcu.memory[0x8002] = 0x80; // high byte of address

        let mcu = Box::new(mcu);
        let mut machine = Machine::new(mcu);

        machine.set_pc(0x8000);

        // Run some ticks
        let result = machine.run_ticks(10);
        assert_eq!(result, ExecuteResult::Continue);
    }
}
