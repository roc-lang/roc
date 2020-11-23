use bumpalo::collections::Vec;

// Not sure exactly how I want to represent registers.
// If we want max speed, we would likely make them structs that impl the same trait to avoid ifs.
#[derive(Copy, Clone, PartialEq, Eq, Hash, PartialOrd, Ord, Debug)]
pub enum GPReg {
    RAX = 0,
    RCX = 1,
    RDX = 2,
    RBX = 3,
    RSP = 4,
    RBP = 5,
    RSI = 6,
    RDI = 7,
    R8 = 8,
    R9 = 9,
    R10 = 10,
    R11 = 11,
    R12 = 12,
    R13 = 13,
    R14 = 14,
    R15 = 15,
}

const REX: u8 = 0x40;
const REX_W: u8 = REX + 0x8;

fn add_rm_extension(reg: GPReg, byte: u8) -> u8 {
    if reg as u8 > 7 {
        byte + 1
    } else {
        byte
    }
}

fn add_opcode_extension(reg: GPReg, byte: u8) -> u8 {
    add_rm_extension(reg, byte)
}

fn add_reg_extension(reg: GPReg, byte: u8) -> u8 {
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

/// `ADD r/m64, imm32` -> Add imm32 sign-extended to 64-bits from r/m64.
pub fn add_register64bit_immediate32bit<'a>(buf: &mut Vec<'a, u8>, dst: GPReg, imm: i32) {
    // This can be optimized if the immediate is 1 byte.
    let rex = add_rm_extension(dst, REX_W);
    let dst_mod = dst as u8 % 8;
    buf.reserve(7);
    buf.extend(&[rex, 0x81, 0xC0 + dst_mod]);
    buf.extend(&imm.to_le_bytes());
}

/// `ADD r/m64,r64` -> Add r64 to r/m64.
pub fn add_register64bit_register64bit<'a>(buf: &mut Vec<'a, u8>, dst: GPReg, src: GPReg) {
    let rex = add_rm_extension(dst, REX_W);
    let rex = add_reg_extension(src, rex);
    let dst_mod = dst as u8 % 8;
    let src_mod = (src as u8 % 8) << 3;
    buf.extend(&[rex, 0x01, 0xC0 + dst_mod + src_mod]);
}

/// `CMOVL r64,r/m64` -> Move if less (SF≠ OF).
pub fn cmovl_register64bit_register64bit<'a>(buf: &mut Vec<'a, u8>, dst: GPReg, src: GPReg) {
    let rex = add_reg_extension(dst, REX_W);
    let rex = add_rm_extension(src, rex);
    let dst_mod = (dst as u8 % 8) << 3;
    let src_mod = src as u8 % 8;
    buf.extend(&[rex, 0x0F, 0x4C, 0xC0 + dst_mod + src_mod]);
}

/// `MOV r/m64, imm32` -> Move imm32 sign extended to 64-bits to r/m64.
pub fn mov_register64bit_immediate32bit<'a>(buf: &mut Vec<'a, u8>, dst: GPReg, imm: i32) {
    let rex = add_rm_extension(dst, REX_W);
    let dst_mod = dst as u8 % 8;
    buf.reserve(7);
    buf.extend(&[rex, 0xC7, 0xC0 + dst_mod]);
    buf.extend(&imm.to_le_bytes());
}

/// `MOV r64, imm64` -> Move imm64 to r64.
pub fn mov_register64bit_immediate64bit<'a>(buf: &mut Vec<'a, u8>, dst: GPReg, imm: i64) {
    if imm <= i32::MAX as i64 && imm >= i32::MIN as i64 {
        mov_register64bit_immediate32bit(buf, dst, imm as i32)
    } else {
        let rex = add_opcode_extension(dst, REX_W);
        let dst_mod = dst as u8 % 8;
        buf.reserve(10);
        buf.extend(&[rex, 0xB8 + dst_mod]);
        buf.extend(&imm.to_le_bytes());
    }
}

/// `MOV r/m64,r64` -> Move r64 to r/m64.
pub fn mov_register64bit_register64bit<'a>(buf: &mut Vec<'a, u8>, dst: GPReg, src: GPReg) {
    let rex = add_rm_extension(dst, REX_W);
    let rex = add_reg_extension(src, rex);
    let dst_mod = dst as u8 % 8;
    let src_mod = (src as u8 % 8) << 3;
    buf.extend(&[rex, 0x89, 0xC0 + dst_mod + src_mod]);
}

/// `MOV r64,r/m64` -> Move r/m64 to r64.
pub fn mov_register64bit_stackoffset32bit<'a>(buf: &mut Vec<'a, u8>, dst: GPReg, offset: i32) {
    // This can be optimized based on how many bytes the offset actually is.
    // This function can probably be made to take any memory offset, I didn't feel like figuring it out rn.
    // Also, this may technically be faster genration since stack operations should be so common.
    let rex = add_reg_extension(dst, REX_W);
    let dst_mod = (dst as u8 % 8) << 3;
    buf.reserve(8);
    buf.extend(&[rex, 0x8B, 0x84 + dst_mod, 0x24]);
    buf.extend(&offset.to_le_bytes());
}

/// `MOV r/m64,r64` -> Move r64 to r/m64.
pub fn mov_stackoffset32bit_register64bit<'a>(buf: &mut Vec<'a, u8>, offset: i32, src: GPReg) {
    // This can be optimized based on how many bytes the offset actually is.
    // This function can probably be made to take any memory offset, I didn't feel like figuring it out rn.
    // Also, this may technically be faster genration since stack operations should be so common.
    let rex = add_reg_extension(src, REX_W);
    let src_mod = (src as u8 % 8) << 3;
    buf.reserve(8);
    buf.extend(&[rex, 0x89, 0x84 + src_mod, 0x24]);
    buf.extend(&offset.to_le_bytes());
}

/// `NEG r/m64` -> Two's complement negate r/m64.
pub fn neg_register64bit<'a>(buf: &mut Vec<'a, u8>, reg: GPReg) {
    let rex = add_rm_extension(reg, REX_W);
    let reg_mod = reg as u8 % 8;
    buf.extend(&[rex, 0xF7, 0xD8 + reg_mod]);
}

/// `RET` -> Near return to calling procedure.
pub fn ret_near<'a>(buf: &mut Vec<'a, u8>) {
    buf.push(0xC3);
}

/// `SUB r/m64, imm32` -> Subtract imm32 sign-extended to 64-bits from r/m64.
pub fn sub_register64bit_immediate32bit<'a>(buf: &mut Vec<'a, u8>, dst: GPReg, imm: i32) {
    // This can be optimized if the immediate is 1 byte.
    let rex = add_rm_extension(dst, REX_W);
    let dst_mod = dst as u8 % 8;
    buf.reserve(7);
    buf.extend(&[rex, 0x81, 0xE8 + dst_mod]);
    buf.extend(&imm.to_le_bytes());
}

/// `POP r64` -> Pop top of stack into r64; increment stack pointer. Cannot encode 32-bit operand size.
pub fn pop_register64bit<'a>(buf: &mut Vec<'a, u8>, reg: GPReg) {
    let reg_mod = reg as u8 % 8;
    if reg as u8 > 7 {
        let rex = add_opcode_extension(reg, REX);
        buf.extend(&[rex, 0x58 + reg_mod]);
    } else {
        buf.push(0x58 + reg_mod);
    }
}

/// `PUSH r64` -> Push r64,
pub fn push_register64bit<'a>(buf: &mut Vec<'a, u8>, reg: GPReg) {
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
    fn test_add_register64bit_immediate32bit() {
        let arena = bumpalo::Bump::new();
        let mut buf = bumpalo::vec![in &arena];
        for (dst, expected) in &[
            (GPReg::RAX, [0x48, 0x81, 0xC0]),
            (GPReg::R15, [0x49, 0x81, 0xC7]),
        ] {
            buf.clear();
            add_register64bit_immediate32bit(&mut buf, *dst, TEST_I32);
            assert_eq!(expected, &buf[..3]);
            assert_eq!(TEST_I32.to_le_bytes(), &buf[3..]);
        }
    }

    #[test]
    fn test_add_register64bit_register64bit() {
        let arena = bumpalo::Bump::new();
        let mut buf = bumpalo::vec![in &arena];
        for ((dst, src), expected) in &[
            ((GPReg::RAX, GPReg::RAX), [0x48, 0x01, 0xC0]),
            ((GPReg::RAX, GPReg::R15), [0x4C, 0x01, 0xF8]),
            ((GPReg::R15, GPReg::RAX), [0x49, 0x01, 0xC7]),
            ((GPReg::R15, GPReg::R15), [0x4D, 0x01, 0xFF]),
        ] {
            buf.clear();
            add_register64bit_register64bit(&mut buf, *dst, *src);
            assert_eq!(expected, &buf[..]);
        }
    }

    #[test]
    fn test_cmovl_register64bit_register64bit() {
        let arena = bumpalo::Bump::new();
        let mut buf = bumpalo::vec![in &arena];
        for ((dst, src), expected) in &[
            ((GPReg::RAX, GPReg::RAX), [0x48, 0x0F, 0x4C, 0xC0]),
            ((GPReg::RAX, GPReg::R15), [0x49, 0x0F, 0x4C, 0xC7]),
            ((GPReg::R15, GPReg::RAX), [0x4C, 0x0F, 0x4C, 0xF8]),
            ((GPReg::R15, GPReg::R15), [0x4D, 0x0F, 0x4C, 0xFF]),
        ] {
            buf.clear();
            cmovl_register64bit_register64bit(&mut buf, *dst, *src);
            assert_eq!(expected, &buf[..]);
        }
    }

    #[test]
    fn test_mov_register64bit_immediate32bit() {
        let arena = bumpalo::Bump::new();
        let mut buf = bumpalo::vec![in &arena];
        for (dst, expected) in &[
            (GPReg::RAX, [0x48, 0xC7, 0xC0]),
            (GPReg::R15, [0x49, 0xC7, 0xC7]),
        ] {
            buf.clear();
            mov_register64bit_immediate32bit(&mut buf, *dst, TEST_I32);
            assert_eq!(expected, &buf[..3]);
            assert_eq!(TEST_I32.to_le_bytes(), &buf[3..]);
        }
    }

    #[test]
    fn test_mov_register64bit_immediate64bit() {
        let arena = bumpalo::Bump::new();
        let mut buf = bumpalo::vec![in &arena];
        for (dst, expected) in &[(GPReg::RAX, [0x48, 0xB8]), (GPReg::R15, [0x49, 0xBF])] {
            buf.clear();
            mov_register64bit_immediate64bit(&mut buf, *dst, TEST_I64);
            assert_eq!(expected, &buf[..2]);
            assert_eq!(TEST_I64.to_le_bytes(), &buf[2..]);
        }
        for (dst, expected) in &[
            (GPReg::RAX, [0x48, 0xC7, 0xC0]),
            (GPReg::R15, [0x49, 0xC7, 0xC7]),
        ] {
            buf.clear();
            mov_register64bit_immediate64bit(&mut buf, *dst, TEST_I32 as i64);
            assert_eq!(expected, &buf[..3]);
            assert_eq!(TEST_I32.to_le_bytes(), &buf[3..]);
        }
    }

    #[test]
    fn test_mov_register64bit_register64bit() {
        let arena = bumpalo::Bump::new();
        let mut buf = bumpalo::vec![in &arena];
        for ((dst, src), expected) in &[
            ((GPReg::RAX, GPReg::RAX), [0x48, 0x89, 0xC0]),
            ((GPReg::RAX, GPReg::R15), [0x4C, 0x89, 0xF8]),
            ((GPReg::R15, GPReg::RAX), [0x49, 0x89, 0xC7]),
            ((GPReg::R15, GPReg::R15), [0x4D, 0x89, 0xFF]),
        ] {
            buf.clear();
            mov_register64bit_register64bit(&mut buf, *dst, *src);
            assert_eq!(expected, &buf[..]);
        }
    }

    #[test]
    fn test_mov_register64bit_stackoffset32bit() {
        let arena = bumpalo::Bump::new();
        let mut buf = bumpalo::vec![in &arena];
        for ((dst, offset), expected) in &[
            ((GPReg::RAX, TEST_I32), [0x48, 0x8B, 0x84, 0x24]),
            ((GPReg::R15, TEST_I32), [0x4C, 0x8B, 0xBC, 0x24]),
        ] {
            buf.clear();
            mov_register64bit_stackoffset32bit(&mut buf, *dst, *offset);
            assert_eq!(expected, &buf[..4]);
            assert_eq!(TEST_I32.to_le_bytes(), &buf[4..]);
        }
    }

    #[test]
    fn test_mov_stackoffset32bit_register64bit() {
        let arena = bumpalo::Bump::new();
        let mut buf = bumpalo::vec![in &arena];
        for ((offset, src), expected) in &[
            ((TEST_I32, GPReg::RAX), [0x48, 0x89, 0x84, 0x24]),
            ((TEST_I32, GPReg::R15), [0x4C, 0x89, 0xBC, 0x24]),
        ] {
            buf.clear();
            mov_stackoffset32bit_register64bit(&mut buf, *offset, *src);
            assert_eq!(expected, &buf[..4]);
            assert_eq!(TEST_I32.to_le_bytes(), &buf[4..]);
        }
    }

    #[test]
    fn test_neg_register64bit() {
        let arena = bumpalo::Bump::new();
        let mut buf = bumpalo::vec![in &arena];
        for (reg, expected) in &[
            (GPReg::RAX, [0x48, 0xF7, 0xD8]),
            (GPReg::R15, [0x49, 0xF7, 0xDF]),
        ] {
            buf.clear();
            neg_register64bit(&mut buf, *reg);
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
    fn test_sub_register64bit_immediate32bit() {
        let arena = bumpalo::Bump::new();
        let mut buf = bumpalo::vec![in &arena];
        for (dst, expected) in &[
            (GPReg::RAX, [0x48, 0x81, 0xE8]),
            (GPReg::R15, [0x49, 0x81, 0xEF]),
        ] {
            buf.clear();
            sub_register64bit_immediate32bit(&mut buf, *dst, TEST_I32);
            assert_eq!(expected, &buf[..3]);
            assert_eq!(TEST_I32.to_le_bytes(), &buf[3..]);
        }
    }

    #[test]
    fn test_pop_register64bit() {
        let arena = bumpalo::Bump::new();
        let mut buf = bumpalo::vec![in &arena];
        for (dst, expected) in &[(GPReg::RAX, vec![0x58]), (GPReg::R15, vec![0x41, 0x5F])] {
            buf.clear();
            pop_register64bit(&mut buf, *dst);
            assert_eq!(&expected[..], &buf[..]);
        }
    }

    #[test]
    fn test_push_register64bit() {
        let arena = bumpalo::Bump::new();
        let mut buf = bumpalo::vec![in &arena];
        for (src, expected) in &[(GPReg::RAX, vec![0x50]), (GPReg::R15, vec![0x41, 0x57])] {
            buf.clear();
            push_register64bit(&mut buf, *src);
            assert_eq!(&expected[..], &buf[..]);
        }
    }
}
