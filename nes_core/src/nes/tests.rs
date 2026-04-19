use super::*;
use crate::nes::apu::Apu;
use crate::nes::controller::Button;
use crate::nes::mapper::{Cartridge, TestCartridge};
use crate::render::ImageRender;

fn read_bus(mcu: &mut NesMcu<ImageRender, ()>, address: u16) -> u8 {
    mcu.prepare_read(address);
    mcu.new_read()
}

fn write_bus(mcu: &mut NesMcu<ImageRender, ()>, address: u16, value: u8) {
    mcu.prepare_write(address);
    mcu.new_write(value);
}

fn test_mcu() -> NesMcu<ImageRender, ()> {
    NesMcu {
        lower_ram: LowerRam::new(),
        ppu: Ppu::new(ImageRender::default_dimension()),
        controller: Controller::new(),
        cartridge: Cartridge::Test(Box::new(TestCartridge::new())),
        apu: Apu::new(()),
        oam_dma_pending: false,
        dmc_dma_pending: None,
        open_bus: 0,
        address_latch: None,
    }
}

#[test]
fn test_lower_ram_mirroring() {
    let mut mcu = test_mcu();

    write_bus(&mut mcu, 0x0000, 0x42);
    assert_eq!(read_bus(&mut mcu, 0x0000), 0x42);
    assert_eq!(read_bus(&mut mcu, 0x0800), 0x42);
    assert_eq!(read_bus(&mut mcu, 0x1000), 0x42);
    assert_eq!(read_bus(&mut mcu, 0x1800), 0x42);
}

#[test]
fn test_frame_counter_inhibit_clears_irq() {
    let mut mcu = test_mcu();

    write_bus(&mut mcu, 0x4017, 0x00);
    for _ in 0..29_829 {
        mcu.tick_apu();
    }

    write_bus(&mut mcu, 0x4017, 0x40);
    assert_eq!(read_bus(&mut mcu, 0x4015) & 0x40, 0);
}

#[test]
fn test_length_counter_status_comes_from_apu_controller() {
    let mut mcu = test_mcu();

    write_bus(&mut mcu, 0x4000, 0x00);
    write_bus(&mut mcu, 0x4002, 0x34);
    // Enable channel first, then load length counter
    write_bus(&mut mcu, 0x4015, 0x01);
    write_bus(&mut mcu, 0x4003, 0xF8);

    assert_eq!(read_bus(&mut mcu, 0x4015) & 0x01, 0x01);

    write_bus(&mut mcu, 0x4015, 0x00);
    assert_eq!(read_bus(&mut mcu, 0x4015) & 0x01, 0x00);
}

#[test]
fn test_ppu_dma() {
    let mut mcu = test_mcu();

    write_bus(&mut mcu, 0x0200, 0x12);
    write_bus(&mut mcu, 0x0201, 0x34);
    write_bus(&mut mcu, 0x0202, 0x56);
    write_bus(&mut mcu, 0x4014, 0x02);
}

#[test]
fn test_ppu_pattern_writes_route_to_cartridge() {
    let mut mcu = test_mcu();

    write_bus(&mut mcu, 0x2006, 0x00);
    write_bus(&mut mcu, 0x2006, 0x10);
    write_bus(&mut mcu, 0x2007, 0xab);

    assert_eq!(mcu.cartridge.pattern_ref()[0x10], 0xab);
}

#[test]
fn test_controller_reads_route_through_nes_mcu() {
    let mut mcu = test_mcu();

    mcu.press_button(Button::A);
    mcu.press_button(Button::Left);
    write_bus(&mut mcu, 0x4016, 0);

    assert_eq!(read_bus(&mut mcu, 0x4016), 0x41);
    assert_eq!(read_bus(&mut mcu, 0x4016), 0x40);
    assert_eq!(read_bus(&mut mcu, 0x4016), 0x40);
    assert_eq!(read_bus(&mut mcu, 0x4016), 0x40);
    assert_eq!(read_bus(&mut mcu, 0x4016), 0x40);
    assert_eq!(read_bus(&mut mcu, 0x4016), 0x40);
    assert_eq!(read_bus(&mut mcu, 0x4016), 0x41);
}
