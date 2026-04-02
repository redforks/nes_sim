use super::*;

#[test]
fn name_table_control() {
    fn assert_start_addr(control: &mut NameTableControl, v1: u8, v2: u8, v3: u8, v4: u8) {
        assert_eq!(v1, control.read(0x2000));
        assert_eq!(v2, control.read(0x2400));
        assert_eq!(v3, control.read(0x2800));
        assert_eq!(v4, control.read(0x2c00));
    }

    let mut control = NameTableControl::default();
    assert_eq!(Mirroring::Four, control.mirroring());

    // default is four screen
    control.write(0x2000, 1);
    control.write(0x2400, 2);
    control.write(0x2800, 3);
    control.write(0x2c00, 4);
    control.write(0x23ff, 11);
    control.write(0x27ff, 12);
    control.write(0x2bff, 13);
    control.write(0x2fff, 14);
    assert_start_addr(&mut control, 1, 2, 3, 4);
    assert_eq!(11, control.mem[1023]);

    control.set_mirroring(Mirroring::LowerBank);
    assert_eq!(Mirroring::LowerBank, control.mirroring());
    assert_start_addr(&mut control, 1, 1, 1, 1);

    control.set_mirroring(Mirroring::UpperBank);
    assert_start_addr(&mut control, 2, 2, 2, 2);

    control.set_mirroring(Mirroring::Horizontal);
    assert_start_addr(&mut control, 1, 1, 2, 2);

    control.set_mirroring(Mirroring::Vertical);
    assert_start_addr(&mut control, 1, 2, 1, 2);

    control.set_mirroring(Mirroring::Four);
    assert_start_addr(&mut control, 1, 2, 3, 4);
}

#[test]
fn test_set_mirroring_same_value() {
    let mut control = NameTableControl::default();

    // Set to vertical mirroring
    control.set_mirroring(Mirroring::Vertical);
    assert_eq!(Mirroring::Vertical, control.mirroring());

    // Setting same mirroring again should do nothing (early return)
    control.set_mirroring(Mirroring::Vertical);
    assert_eq!(Mirroring::Vertical, control.mirroring());
}
