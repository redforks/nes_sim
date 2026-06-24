use super::*;
use crate::inc_system_clock;
use crate::nes::apu::Apu;
use crate::nes::controller::Button;
use crate::nes::mapper::{Mirroring, TestCartridge};
use crate::render::ImageRender;

fn test_mcu() -> NesMcu<ImageRender, ()> {
    NesMcu {
        lower_ram: LowerRam::new(),
        ppu: Ppu::new(
            ImageRender::default_dimension(),
            Mirroring::Horizontal,
            Box::new(TestCartridge::new()),
        ),
        controller: Controller::new(),
        apu: Apu::new(()),
        oam_dma_pending: None,
        oam_dma: None,
        open_bus: 0,
    }
}

#[test]
fn test_lower_ram_mirroring() {
    let mut mcu = test_mcu();

    mcu.write(0x0000, 0x42);
    assert_eq!(mcu.read(0x0000), 0x42);
    assert_eq!(mcu.read(0x0800), 0x42);
    assert_eq!(mcu.read(0x1000), 0x42);
    assert_eq!(mcu.read(0x1800), 0x42);
}

#[test]
fn test_frame_counter_inhibit_clears_irq() {
    let mut mcu = test_mcu();

    mcu.write(0x4017, 0x00);
    for _ in 0..(29_829 * 3) {
        mcu.tick_apu();
        inc_system_clock();
    }

    mcu.write(0x4017, 0x40);
    assert_eq!(mcu.read(0x4015) & 0x40, 0);
}

#[test]
fn test_length_counter_status_comes_from_apu_controller() {
    let mut mcu = test_mcu();

    mcu.write(0x4000, 0x00);
    mcu.write(0x4002, 0x34);
    mcu.write(0x4015, 0x01);
    mcu.write(0x4003, 0xF8);

    assert_eq!(mcu.read(0x4015) & 0x01, 0x01);

    mcu.write(0x4015, 0x00);
    assert_eq!(mcu.read(0x4015) & 0x01, 0x00);
}

#[test]
fn test_controller_reads_route_through_nes_mcu() {
    let mut mcu = test_mcu();

    mcu.press_controller_a(Button::A);
    mcu.press_controller_a(Button::Left);
    mcu.write(0x4016, 0);

    assert_eq!(mcu.read(0x4016), 0x41);
    assert_eq!(mcu.read(0x4016), 0x40);
    assert_eq!(mcu.read(0x4016), 0x40);
    assert_eq!(mcu.read(0x4016), 0x40);
    assert_eq!(mcu.read(0x4016), 0x40);
    assert_eq!(mcu.read(0x4016), 0x40);
    assert_eq!(mcu.read(0x4016), 0x41);
}
