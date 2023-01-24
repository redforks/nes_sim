use nes_core::machine::CYCLES_PER_FRAME;
use nes_core::{Cpu, ExecuteResult, Plugin};
use std::collections::VecDeque;

pub struct DetectDeadLoop<const DEPTH: usize, const REPEATS: u16 = CYCLES_PER_FRAME> {
    recent_pc: VecDeque<u16>,
    should_exit: bool,
    count: u16,
}

impl<const DEPTH: usize, const REPEATS: u16> DetectDeadLoop<DEPTH, REPEATS> {
    pub fn new() -> Self {
        DetectDeadLoop {
            recent_pc: VecDeque::with_capacity(2 * DEPTH),
            should_exit: false,
            count: 0,
        }
    }
}

impl<const DEPTH: usize, const REPEATS: u16> Plugin for DetectDeadLoop<DEPTH, REPEATS> {
    fn start(&mut self, _: &Cpu) {}

    fn end(&mut self, cpu: &Cpu) {
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
            eprintln!("test failed: pc repeated ({})", DEPTH);
        };
    }

    fn should_stop(&self) -> ExecuteResult {
        if self.should_exit {
            ExecuteResult::Stop(1)
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
    #[case(0, false, &[10, 11, 10, 11, 9])]
    #[case(3, true, &[10, 11, 10, 11, 9, 10, 9, 10, 9, 10])]
    fn test(#[case] exp_count: u16, #[case] should_exit: bool, #[case] pcs: &[u16]) {
        let mcu = RamMcu::new([0; 65536]);
        let mut cpu = Cpu::new(Box::new(mcu));

        let mut p = DetectDeadLoop::<2, 2>::new();
        for pc in pcs.to_owned() {
            cpu.pc = pc;
            p.end(&cpu);
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
}
