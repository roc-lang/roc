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

const REX: u8 = 0x40;
const REX_W: u8 = REX + 0x8;

fn add_rm_extension(reg: Register, byte: u8) -> u8 {
    if reg as u8 > 7 {
        byte + 1
    } else {
        byte
    }
}

fn add_opcode_extension(reg: Register, byte: u8) -> u8 {
    add_rm_extension(reg, byte)
}

fn add_reg_extension(reg: Register, byte: u8) -> u8 {
    if reg as u8 > 7 {
        byte + 4
    } else {
        byte
    }
}

// Below here are the functions for all of the assembly instructions.
// Their names are based on the instruction and operators combined.
// You should call `buf.reserve()` if you push or extend more than once.
// Unit tests are added at the bottom of the file to ensure correct asm generation.
// Please keep these in alphanumeric order.

/// `MOV r/m64, imm32` -> Move imm32 sign extended to 64-bits to r/m64.
pub fn mov_register64bit_immediate32bit<'a>(buf: &mut Vec<'a, u8>, dst: Register, imm: i32) {
    let rex = add_rm_extension(dst, REX_W);
    let dst_mod = dst as u8 % 8;
    buf.reserve(7);
    buf.extend(&[rex, 0xC7, 0xC0 + dst_mod]);
    buf.extend(&imm.to_le_bytes());
}

/// `MOV r64, imm64` -> Move imm64 to r64.
pub fn mov_register64bit_immediate64bit<'a>(buf: &mut Vec<'a, u8>, dst: Register, imm: i64) {
    let rex = add_opcode_extension(dst, REX_W);
    let dst_mod = dst as u8 % 8;
    buf.reserve(10);
    buf.extend(&[rex, 0xB8 + dst_mod]);
    buf.extend(&imm.to_le_bytes());
}

/// `MOV r/m64,r64` -> Move r64 to r/m64.
pub fn mov_register64bit_register64bit<'a>(buf: &mut Vec<'a, u8>, dst: Register, src: Register) {
    let rex = add_rm_extension(dst, REX_W);
    let rex = add_reg_extension(src, rex);
    let dst_mod = dst as u8 % 8;
    let src_mod = (src as u8 % 8) << 3;
    buf.extend(&[rex, 0x89, 0xC0 + dst_mod + src_mod]);
}

/// `RET` -> Near return to calling procedure.
pub fn ret_near<'a>(buf: &mut Vec<'a, u8>) {
    buf.push(0xC3);
}

/// `POP r64` -> Pop top of stack into r64; increment stack pointer. Cannot encode 32-bit operand size.
pub fn pop_register64bit<'a>(buf: &mut Vec<'a, u8>, reg: Register) {
    let reg_mod = reg as u8 % 8;
    if reg as u8 > 7 {
        let rex = add_opcode_extension(reg, REX);
        buf.extend(&[rex, 0x58 + reg_mod]);
    } else {
        buf.push(0x58 + reg_mod);
    }
}

/// `PUSH r64` -> Push r64,
pub fn push_register64bit<'a>(buf: &mut Vec<'a, u8>, reg: Register) {
    let reg_mod = reg as u8 % 8;
    if reg as u8 > 7 {
        let rex = add_opcode_extension(reg, REX);
        buf.extend(&[rex, 0x50 + reg_mod]);
    } else {
        buf.push(0x50 + reg_mod);
    }
}

// When writing tests, it is a good idea to test both a number and unnumbered register.
// This is because R8-R15 often have special instruction prefixes.
#[cfg(test)]
mod tests {
    use super::*;

    const TEST_I32: i32 = 0x12345678;
    const TEST_I64: i64 = 0x12345678_9ABCDEF0;

    #[test]
    fn test_mov_register64bit_immediate32bit() {
        let arena = bumpalo::Bump::new();
        let mut buf = bumpalo::vec![in &arena];
        for (in1, expected) in &[
            (Register::RAX, [0x48, 0xC7, 0xC0]),
            (Register::R15, [0x49, 0xC7, 0xC7]),
        ] {
            buf.clear();
            mov_register64bit_immediate32bit(&mut buf, *in1, TEST_I32);
            assert_eq!(expected, &buf[..3]);
            assert_eq!(TEST_I32.to_le_bytes(), &buf[3..]);
        }
    }

    #[test]
    fn test_mov_register64bit_immediate64bit() {
        let arena = bumpalo::Bump::new();
        let mut buf = bumpalo::vec![in &arena];
        for (in1, expected) in &[(Register::RAX, [0x48, 0xB8]), (Register::R15, [0x49, 0xBF])] {
            buf.clear();
            mov_register64bit_immediate64bit(&mut buf, *in1, TEST_I64);
            assert_eq!(expected, &buf[..2]);
            assert_eq!(TEST_I64.to_le_bytes(), &buf[2..]);
        }
    }

    #[test]
    fn test_mov_register64bit_register64bit() {
        let arena = bumpalo::Bump::new();
        let mut buf = bumpalo::vec![in &arena];
        for ((in1, in2), expected) in &[
            ((Register::RAX, Register::RAX), [0x48, 0x89, 0xC0]),
            ((Register::RAX, Register::R15), [0x4C, 0x89, 0xF8]),
            ((Register::R15, Register::RAX), [0x49, 0x89, 0xC7]),
            ((Register::R15, Register::R15), [0x4D, 0x89, 0xFF]),
        ] {
            buf.clear();
            mov_register64bit_register64bit(&mut buf, *in1, *in2);
            assert_eq!(expected, &buf[..]);
        }
    }

    #[test]
    fn test_ret_near() {
        let arena = bumpalo::Bump::new();
        let mut buf = bumpalo::vec![in &arena];
        ret_near(&mut buf);
        assert_eq!(&[0xC3], &buf[..]);
    }

    #[test]
    fn test_pop_register64bit() {
        let arena = bumpalo::Bump::new();
        let mut buf = bumpalo::vec![in &arena];
        for (in1, expected) in &[
            (Register::RAX, vec![0x58]),
            (Register::R15, vec![0x41, 0x5F]),
        ] {
            buf.clear();
            pop_register64bit(&mut buf, *in1);
            assert_eq!(&expected[..], &buf[..]);
        }
    }

    #[test]
    fn test_push_register64bit() {
        let arena = bumpalo::Bump::new();
        let mut buf = bumpalo::vec![in &arena];
        for (in1, expected) in &[
            (Register::RAX, vec![0x50]),
            (Register::R15, vec![0x41, 0x57]),
        ] {
            buf.clear();
            push_register64bit(&mut buf, *in1);
            assert_eq!(&expected[..], &buf[..]);
        }
    }
}
