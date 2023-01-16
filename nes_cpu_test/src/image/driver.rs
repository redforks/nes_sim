use nes_core::nes::ppu::{PpuCtrl, PpuDriver, PpuMask, PpuStatus};

pub struct EmptyPpuDriver();

impl PpuDriver for EmptyPpuDriver {
    fn set_ctrl(&mut self, _: PpuCtrl) {}

    fn set_mask(&mut self, _: PpuMask) {}

    fn get_status(&self) -> PpuStatus {
        0.into()
    }

    fn get_oma_address(&self) -> u8 {
        0.into()
    }

    fn set_oma_address(&mut self, _: u8) {}

    fn get_oma_data(&self) -> u8 {
        0.into()
    }

    fn set_oma_data(&mut self, _: u8) {}

    fn set_scroll_position(&mut self, _x: u8, _y: u8) {}

    fn set_address(&mut self, _: u16) {}

    fn get_data(&self) -> u8 {
        0.into()
    }

    fn set_data(&mut self, _: u8) {}

    fn set_dma(&mut self, _: u8) {}
}
