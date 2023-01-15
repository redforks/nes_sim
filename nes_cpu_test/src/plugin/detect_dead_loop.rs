use nes_core::{Cpu, ExecuteResult, Plugin};
use std::collections::VecDeque;

pub struct DetectDeadLoop<const DEPTH: usize> {
    recent_pc: VecDeque<u16>,
    should_exit: bool,
    count: u16,
}

impl<const DEPTH: usize> DetectDeadLoop<DEPTH> {
    pub fn new() -> Self {
        DetectDeadLoop {
            recent_pc: VecDeque::with_capacity(2 * DEPTH),
            should_exit: false,
            count: 0,
        }
    }
}

impl<const DEPTH: usize> Plugin for DetectDeadLoop<DEPTH> {
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
                return;
            }
        }

        self.count += 1;
        self.should_exit = if self.count > 100 {
            eprintln!("test failed: pc repeated ({})", DEPTH);
            true
        } else {
            false
        };
    }

    fn should_stop(&self) -> ExecuteResult {
        if self.should_exit {
            ExecuteResult::Stop
        } else {
            ExecuteResult::Continue
        }
    }
}
