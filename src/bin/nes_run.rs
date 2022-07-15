use std::fs::File;
use nes_sim::{Cpu, SyncInstructionCycle};
use std::io::Read;

fn main() {
    let mut f = File::open("6502_functional_test.bin").unwrap();
    let mut cpu = Cpu::new();
    assert_eq!(65536, f.read(&mut cpu.memory).unwrap());
    let mut sync_cycle = TestSyncInstructionCycle(0);
    cpu.run(&mut sync_cycle);
}

struct TestSyncInstructionCycle(u8);

impl SyncInstructionCycle for TestSyncInstructionCycle {
    fn start(&mut self) {}

    fn end(&mut self, cycles: u8) {
        self.0 = cycles;
    }
}

impl TestSyncInstructionCycle {
    fn cycles(&self) -> u8 {
        self.0
    }
}
