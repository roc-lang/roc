use bumpalo::collections::Vec;

// Not sure exactly how I want to represent registers.
// If we want max speed, we would likely make them structs that impl the same trait to avoid ifs.
#[derive(Copy, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum Register {
    RAX,
    RCX,
    RDX,
    RBX,
    RSP,
    RBP,
    RSI,
    RDI,
    R8,
    R9,
    R10,
    R11,
    R12,
    R13,
    R14,
    R15,
}

const REX_W: u8 = 0x48;

fn add_rm_extension(reg: Register, byte: u8) -> u8 {
    if reg as u8 > 7 {
        byte + 1
    } else {
        byte
    }
}

fn add_reg_extension(reg: Register, byte: u8) -> u8 {
    if reg as u8 > 7 {
        byte + 4
    } else {
        byte
    }
}

/// Below here are the functions for all of the assembly instructions.
/// Their names are based on the instruction and operators combined.
/// Please call buf.reserve if you push or extend more than once.
/// Also, please keep these in alphanumeric order.

pub fn mov_register64bit_immediate32bit<'a>(buf: &mut Vec<'a, u8>, dst: Register, imm: i32) {
    let rex = add_rm_extension(dst, REX_W);
    let dst_mod = dst as u8 % 8;
    buf.reserve(7);
    buf.extend(&[rex, 0xC7, 0xC0 + dst_mod]);
    buf.extend(&imm.to_le_bytes());
}

pub fn mov_register64bit_immediate64bit<'a>(buf: &mut Vec<'a, u8>, dst: Register, imm: i64) {
    let rex = add_rm_extension(dst, REX_W);
    let dst_mod = dst as u8 % 8;
    buf.reserve(10);
    buf.extend(&[rex, 0xB8 + dst_mod]);
    buf.extend(&imm.to_le_bytes());
}

pub fn mov_register64bit_register64bit<'a>(buf: &mut Vec<'a, u8>, dst: Register, src: Register) {
    let rex = add_rm_extension(dst, REX_W);
    let rex = add_reg_extension(src, rex);
    let dst_mod = dst as u8 % 8;
    let src_mod = (src as u8 % 8) << 3;
    buf.extend(&[rex, 0x89, 0xC0 + dst_mod + src_mod]);
}
