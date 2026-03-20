use nes_core::mcu::Mcu;
use nes_core::{Cpu, ExecuteResult, Plugin};
use std::collections::VecDeque;

pub struct DetectDeadLoop<const DEPTH: usize, const REPEATS: u32 = 200000> {
    recent_pc: VecDeque<u16>,
    should_exit: bool,
    exit_code: u8,
    count: u32,
}

impl<const DEPTH: usize, const REPEATS: u32> DetectDeadLoop<DEPTH, REPEATS> {
    pub fn new() -> Self {
        DetectDeadLoop {
            recent_pc: VecDeque::with_capacity(2 * DEPTH),
            should_exit: false,
            exit_code: 1,
            count: 0,
        }
    }
}

impl<const DEPTH: usize, const REPEATS: u32, M: Mcu> Plugin<M> for DetectDeadLoop<DEPTH, REPEATS> {
    fn start(&mut self, _: &mut Cpu<M>) {}

    fn end(&mut self, cpu: &mut Cpu<M>, _cycles: u8) {
        if self.recent_pc.len() == DEPTH * 2 {
            self.recent_pc.pop_front();
        }
        self.recent_pc.push_back(cpu.pc);
        if self.recent_pc.len() < DEPTH * 2 {
            return;
        }

        for i in 0..DEPTH {
            if self.recent_pc[i] != self.recent_pc[i + DEPTH] {
                self.count = 0;
                return;
            }
        }

        self.count += 1;
        self.should_exit = self.count > REPEATS;
        if self.should_exit {
            let pc = cpu.pc;
            let op = cpu.peek_byte(pc);
            let lo = cpu.peek_byte(pc.wrapping_add(1));
            let hi = cpu.peek_byte(pc.wrapping_add(2));
            let jmp_target = ((hi as u16) << 8) | lo as u16;
            let status = cpu.peek_byte(0x6000);

            if op == 0x4c && jmp_target == pc && status == 0 {
                self.exit_code = 0;
            } else {
                self.exit_code = 1;
                eprintln!("test failed: pc repeated ({})", DEPTH);
            }
        };
    }

    fn should_stop(&self) -> ExecuteResult {
        if self.should_exit {
            ExecuteResult::Stop(self.exit_code)
        } else {
            ExecuteResult::Continue
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use nes_core::mcu::RamMcu;
    use rstest::rstest;

    #[rstest]
    #[case(0, false, &[10])]
    #[case(1, false, &[10, 11, 10, 11])]
    #[case(2, false, &[10, 11, 10, 11, 10])]
    #[case(3, true, &[10, 11, 10, 11, 10, 11])]
    #[case(4, true, &[10, 11, 10, 11, 10, 11, 10])]
    #[case(0, false, &[10, 11, 10, 11, 9])]
    #[case(3, true, &[10, 11, 10, 11, 9, 10, 9, 10, 9, 10])]
    fn test(#[case] exp_count: u32, #[case] should_exit: bool, #[case] pcs: &[u16]) {
        let mcu = RamMcu::new([0; 65536]);
        let mut cpu = Cpu::new(mcu);

        let mut p = DetectDeadLoop::<2, 2>::new();
        for pc in pcs.iter().copied() {
            cpu.pc = pc;
            p.end(&mut cpu, 0); // cycles not used in this plugin
        }

        assert_eq!(exp_count, p.count);
        assert_eq!(should_exit, p.should_exit);

        // assert_eq!((0, false), feed_pcs(&mut cpu, &[10]));
        // assert_eq!((1, false), feed_pcs(&mut cpu, &[10, 11, 10, 11]));
        // assert_eq!((2, false), feed_pcs(&mut cpu, &[10, 11, 10, 11, 10]));
        // assert_eq!((0, false), feed_pcs(&mut cpu, &[10, 11, 10, 11, 10, 9]));
        // assert_eq!(
        //     (3, true),
        //     feed_pcs(&mut cpu, &[10, 11, 10, 11, 10, 9, 10, 9, 10, 9])
        // );
    }

    #[test]
    fn self_jmp_with_success_status_returns_zero_exit_code() {
        let mut mem = [0; 65536];
        mem[0x8000] = 0x4c;
        mem[0x8001] = 0x00;
        mem[0x8002] = 0x80;
        mem[0x6000] = 0x00;

        let mcu = RamMcu::new(mem);
        let mut cpu = Cpu::new(mcu);
        let mut p = DetectDeadLoop::<1, 0>::new();

        cpu.pc = 0x8000;
        p.end(&mut cpu, 0);
        p.end(&mut cpu, 0);

        assert!(p.should_exit);
        assert_eq!(0, p.exit_code);
    }

    #[test]
    fn dead_loop_without_success_signature_returns_nonzero_exit_code() {
        let mut mem = [0; 65536];
        mem[0x8000] = 0xea;
        mem[0x6000] = 0x00;

        let mcu = RamMcu::new(mem);
        let mut cpu = Cpu::new(mcu);
        let mut p = DetectDeadLoop::<1, 0>::new();

        cpu.pc = 0x8000;
        p.end(&mut cpu, 0);
        p.end(&mut cpu, 0);

        assert!(p.should_exit);
        assert_eq!(1, p.exit_code);
    }
}
