use super::*;

#[test]
fn literal_format() {
    assert_eq!(format!("{}", Literal(0x12)), "#$12");
}

#[test]
fn zero_page_format() {
    assert_eq!(format!("{}", ZeroPage(0x12)), "$12");
}

#[test]
fn absolute_format() {
    assert_eq!(format!("{}", Absolute(0x1234)), "$1234");
}

#[test]
fn zero_page_x_format() {
    assert_eq!(format!("{}", ZeroPageX(0x12)), "$12,X");
}

#[test]
fn zero_page_y_format() {
    assert_eq!(format!("{}", ZeroPageY(0x12)), "$12,Y");
}

#[test]
fn absolute_x_format() {
    assert_eq!(format!("{}", AbsoluteX(0x1234)), "$1234,X");
}

#[test]
fn absolut_y_format() {
    assert_eq!(format!("{}", AbsoluteY(0x1234)), "$1234,Y");
}

#[test]
fn indirect_x_format() {
    assert_eq!(format!("{}", IndirectX(0x12)), "($12,X)");
}

#[test]
fn indirect_y_format() {
    assert_eq!(format!("{}", IndirectY(0x12)), "($12),Y");
}

#[test]
fn register_a_format() {
    assert_eq!(format!("{}", RegisterA()), "A");
}

#[test]
fn register_x_format() {
    assert_eq!(format!("{}", RegisterX()), "X");
}

#[test]
fn register_y_format() {
    assert_eq!(format!("{}", RegisterY()), "Y");
}

#[test]
fn register_sp_format() {
    assert_eq!(format!("{}", RegisterSP()), "SP");
}

#[test]
fn register_status_format() {
    assert_eq!(format!("{}", RegisterStatus()), "status");
}

#[test]
fn flag_format() {
    assert_eq!(format!("{}", FlagAddr(Flag::Carry)), "C");
}
