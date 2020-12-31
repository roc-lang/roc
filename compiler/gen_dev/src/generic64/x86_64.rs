use crate::generic64::{Assembler, CallConv, GPRegTrait};
use bumpalo::collections::Vec;

// Not sure exactly how I want to represent registers.
// If we want max speed, we would likely make them structs that impl the same trait to avoid ifs.
#[derive(Copy, Clone, PartialEq, Eq, Hash, PartialOrd, Ord, Debug)]
pub enum X86_64GPReg {
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

impl GPRegTrait for X86_64GPReg {}

pub struct X86_64Assembler {}
pub struct X86_64WindowsFastcall {}
pub struct X86_64SystemV {}

const STACK_ALIGNMENT: u8 = 16;

impl CallConv<X86_64GPReg> for X86_64SystemV {
    const GP_PARAM_REGS: &'static [X86_64GPReg] = &[
        X86_64GPReg::RDI,
        X86_64GPReg::RSI,
        X86_64GPReg::RDX,
        X86_64GPReg::RCX,
        X86_64GPReg::R8,
        X86_64GPReg::R9,
    ];
    const GP_RETURN_REGS: &'static [X86_64GPReg] = &[X86_64GPReg::RAX, X86_64GPReg::RDX];

    const GP_DEFAULT_FREE_REGS: &'static [X86_64GPReg] = &[
        // The regs we want to use first should be at the end of this vec.
        // We will use pop to get which reg to use next
        // Use callee saved regs last.
        X86_64GPReg::RBX,
        // Don't use frame pointer: X86_64GPReg::RBP,
        X86_64GPReg::R12,
        X86_64GPReg::R13,
        X86_64GPReg::R14,
        X86_64GPReg::R15,
        // Use caller saved regs first.
        X86_64GPReg::RAX,
        X86_64GPReg::RCX,
        X86_64GPReg::RDX,
        // Don't use stack pionter: X86_64GPReg::RSP,
        X86_64GPReg::RSI,
        X86_64GPReg::RDI,
        X86_64GPReg::R8,
        X86_64GPReg::R9,
        X86_64GPReg::R10,
        X86_64GPReg::R11,
    ];
    const SHADOW_SPACE_SIZE: u8 = 0;

    #[inline(always)]
    fn callee_saved(reg: &X86_64GPReg) -> bool {
        matches!(
            reg,
            X86_64GPReg::RBX
                | X86_64GPReg::RBP
                | X86_64GPReg::R12
                | X86_64GPReg::R13
                | X86_64GPReg::R14
                | X86_64GPReg::R15
        )
    }

    #[inline(always)]
    fn setup_stack<'a>(
        buf: &mut Vec<'a, u8>,
        leaf_function: bool,
        saved_regs: &[X86_64GPReg],
        requested_stack_size: i32,
    ) -> Result<i32, String> {
        x86_64_generic_setup_stack(buf, leaf_function, saved_regs, requested_stack_size)
    }

    #[inline(always)]
    fn cleanup_stack<'a>(
        buf: &mut Vec<'a, u8>,
        leaf_function: bool,
        saved_regs: &[X86_64GPReg],
        aligned_stack_size: i32,
    ) -> Result<(), String> {
        x86_64_generic_cleanup_stack(buf, leaf_function, saved_regs, aligned_stack_size)
    }
}

impl CallConv<X86_64GPReg> for X86_64WindowsFastcall {
    const GP_PARAM_REGS: &'static [X86_64GPReg] = &[
        X86_64GPReg::RCX,
        X86_64GPReg::RDX,
        X86_64GPReg::R8,
        X86_64GPReg::R9,
    ];
    const GP_RETURN_REGS: &'static [X86_64GPReg] = &[X86_64GPReg::RAX];
    const GP_DEFAULT_FREE_REGS: &'static [X86_64GPReg] = &[
        // The regs we want to use first should be at the end of this vec.
        // We will use pop to get which reg to use next

        // Don't use stack pionter: X86_64GPReg::RSP,
        // Don't use frame pointer: X86_64GPReg::RBP,

        // Use callee saved regs last.
        X86_64GPReg::RBX,
        X86_64GPReg::RSI,
        X86_64GPReg::RDI,
        X86_64GPReg::R12,
        X86_64GPReg::R13,
        X86_64GPReg::R14,
        X86_64GPReg::R15,
        // Use caller saved regs first.
        X86_64GPReg::RAX,
        X86_64GPReg::RCX,
        X86_64GPReg::RDX,
        X86_64GPReg::R8,
        X86_64GPReg::R9,
        X86_64GPReg::R10,
        X86_64GPReg::R11,
    ];
    const SHADOW_SPACE_SIZE: u8 = 32;

    #[inline(always)]
    fn callee_saved(reg: &X86_64GPReg) -> bool {
        matches!(
            reg,
            X86_64GPReg::RBX
                | X86_64GPReg::RBP
                | X86_64GPReg::RSI
                | X86_64GPReg::RSP
                | X86_64GPReg::RDI
                | X86_64GPReg::R12
                | X86_64GPReg::R13
                | X86_64GPReg::R14
                | X86_64GPReg::R15
        )
    }

    #[inline(always)]
    fn setup_stack<'a>(
        buf: &mut Vec<'a, u8>,
        leaf_function: bool,
        saved_regs: &[X86_64GPReg],
        requested_stack_size: i32,
    ) -> Result<i32, String> {
        x86_64_generic_setup_stack(buf, leaf_function, saved_regs, requested_stack_size)
    }

    #[inline(always)]
    fn cleanup_stack<'a>(
        buf: &mut Vec<'a, u8>,
        leaf_function: bool,
        saved_regs: &[X86_64GPReg],
        aligned_stack_size: i32,
    ) -> Result<(), String> {
        x86_64_generic_cleanup_stack(buf, leaf_function, saved_regs, aligned_stack_size)
    }
}

#[inline(always)]
fn x86_64_generic_setup_stack<'a>(
    buf: &mut Vec<'a, u8>,
    leaf_function: bool,
    saved_regs: &[X86_64GPReg],
    requested_stack_size: i32,
) -> Result<i32, String> {
    if !leaf_function {
        X86_64Assembler::push_reg64(buf, X86_64GPReg::RBP);
        X86_64Assembler::mov_reg64_reg64(buf, X86_64GPReg::RBP, X86_64GPReg::RSP);
    }
    for reg in saved_regs {
        X86_64Assembler::push_reg64(buf, *reg);
    }

    // full size is upcast to i64 to make sure we don't overflow here.
    let full_size = 8 * saved_regs.len() as i64 + requested_stack_size as i64;
    let alignment = if full_size <= 0 {
        0
    } else {
        full_size % STACK_ALIGNMENT as i64
    };
    let offset = if alignment == 0 {
        0
    } else {
        STACK_ALIGNMENT - alignment as u8
    };
    if let Some(aligned_stack_size) = requested_stack_size.checked_add(offset as i32) {
        if aligned_stack_size > 0 {
            X86_64Assembler::sub_reg64_reg64_imm32(
                buf,
                X86_64GPReg::RSP,
                X86_64GPReg::RSP,
                aligned_stack_size,
            );
            Ok(aligned_stack_size)
        } else {
            Ok(0)
        }
    } else {
        Err("Ran out of stack space".to_string())
    }
}

#[inline(always)]
fn x86_64_generic_cleanup_stack<'a>(
    buf: &mut Vec<'a, u8>,
    leaf_function: bool,
    saved_regs: &[X86_64GPReg],
    aligned_stack_size: i32,
) -> Result<(), String> {
    if aligned_stack_size > 0 {
        X86_64Assembler::add_reg64_reg64_imm32(
            buf,
            X86_64GPReg::RSP,
            X86_64GPReg::RSP,
            aligned_stack_size,
        );
    }
    for reg in saved_regs.iter().rev() {
        X86_64Assembler::pop_reg64(buf, *reg);
    }
    if !leaf_function {
        X86_64Assembler::mov_reg64_reg64(buf, X86_64GPReg::RSP, X86_64GPReg::RBP);
        X86_64Assembler::pop_reg64(buf, X86_64GPReg::RBP);
    }
    Ok(())
}

impl Assembler<X86_64GPReg> for X86_64Assembler {
    // These functions should map to the raw assembly functions below.
    // In some cases, that means you can just directly call one of the direct assembly functions.
    #[inline(always)]
    fn abs_reg64_reg64(buf: &mut Vec<'_, u8>, dst: X86_64GPReg, src: X86_64GPReg) {
        mov_reg64_reg64(buf, dst, src);
        neg_reg64(buf, dst);
        cmovl_reg64_reg64(buf, dst, src);
    }
    #[inline(always)]
    fn add_reg64_reg64_imm32(
        buf: &mut Vec<'_, u8>,
        dst: X86_64GPReg,
        src1: X86_64GPReg,
        imm32: i32,
    ) {
        if dst == src1 {
            add_reg64_imm32(buf, dst, imm32);
        } else {
            mov_reg64_reg64(buf, dst, src1);
            add_reg64_imm32(buf, dst, imm32);
        }
    }
    #[inline(always)]
    fn add_reg64_reg64_reg64(
        buf: &mut Vec<'_, u8>,
        dst: X86_64GPReg,
        src1: X86_64GPReg,
        src2: X86_64GPReg,
    ) {
        if dst == src1 {
            add_reg64_reg64(buf, dst, src2);
        } else if dst == src2 {
            add_reg64_reg64(buf, dst, src1);
        } else {
            mov_reg64_reg64(buf, dst, src1);
            add_reg64_reg64(buf, dst, src2);
        }
    }
    #[inline(always)]
    fn mov_reg64_imm64(buf: &mut Vec<'_, u8>, dst: X86_64GPReg, imm: i64) {
        mov_reg64_imm64(buf, dst, imm);
    }
    #[inline(always)]
    fn mov_reg64_reg64(buf: &mut Vec<'_, u8>, dst: X86_64GPReg, src: X86_64GPReg) {
        mov_reg64_reg64(buf, dst, src);
    }
    #[inline(always)]
    fn mov_reg64_stack32(buf: &mut Vec<'_, u8>, dst: X86_64GPReg, offset: i32) {
        mov_reg64_stack32(buf, dst, offset);
    }
    #[inline(always)]
    fn mov_stack32_reg64(buf: &mut Vec<'_, u8>, offset: i32, src: X86_64GPReg) {
        mov_stack32_reg64(buf, offset, src);
    }
    #[inline(always)]
    fn sub_reg64_reg64_imm32(
        buf: &mut Vec<'_, u8>,
        dst: X86_64GPReg,
        src1: X86_64GPReg,
        imm32: i32,
    ) {
        if dst == src1 {
            sub_reg64_imm32(buf, dst, imm32);
        } else {
            mov_reg64_reg64(buf, dst, src1);
            sub_reg64_imm32(buf, dst, imm32);
        }
    }
    #[inline(always)]
    fn ret(buf: &mut Vec<'_, u8>) {
        ret(buf);
    }
}

impl X86_64Assembler {
    #[inline(always)]
    fn pop_reg64(buf: &mut Vec<'_, u8>, reg: X86_64GPReg) {
        pop_reg64(buf, reg);
    }

    #[inline(always)]
    fn push_reg64(buf: &mut Vec<'_, u8>, reg: X86_64GPReg) {
        push_reg64(buf, reg);
    }
}
const REX: u8 = 0x40;
const REX_W: u8 = REX + 0x8;

#[inline(always)]
const fn add_rm_extension(reg: X86_64GPReg, byte: u8) -> u8 {
    if reg as u8 > 7 {
        byte + 1
    } else {
        byte
    }
}

#[inline(always)]
const fn add_opcode_extension(reg: X86_64GPReg, byte: u8) -> u8 {
    add_rm_extension(reg, byte)
}

#[inline(always)]
const fn add_reg_extension(reg: X86_64GPReg, byte: u8) -> u8 {
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
#[inline(always)]
fn add_reg64_imm32(buf: &mut Vec<'_, u8>, dst: X86_64GPReg, imm: i32) {
    // This can be optimized if the immediate is 1 byte.
    let rex = add_rm_extension(dst, REX_W);
    let dst_mod = dst as u8 % 8;
    buf.reserve(7);
    buf.extend(&[rex, 0x81, 0xC0 + dst_mod]);
    buf.extend(&imm.to_le_bytes());
}

/// `ADD r/m64,r64` -> Add r64 to r/m64.
#[inline(always)]
fn add_reg64_reg64(buf: &mut Vec<'_, u8>, dst: X86_64GPReg, src: X86_64GPReg) {
    let rex = add_rm_extension(dst, REX_W);
    let rex = add_reg_extension(src, rex);
    let dst_mod = dst as u8 % 8;
    let src_mod = (src as u8 % 8) << 3;
    buf.extend(&[rex, 0x01, 0xC0 + dst_mod + src_mod]);
}

/// `CMOVL r64,r/m64` -> Move if less (SF≠ OF).
#[inline(always)]
fn cmovl_reg64_reg64(buf: &mut Vec<'_, u8>, dst: X86_64GPReg, src: X86_64GPReg) {
    let rex = add_reg_extension(dst, REX_W);
    let rex = add_rm_extension(src, rex);
    let dst_mod = (dst as u8 % 8) << 3;
    let src_mod = src as u8 % 8;
    buf.extend(&[rex, 0x0F, 0x4C, 0xC0 + dst_mod + src_mod]);
}

/// `MOV r/m64, imm32` -> Move imm32 sign extended to 64-bits to r/m64.
#[inline(always)]
fn mov_reg64_imm32(buf: &mut Vec<'_, u8>, dst: X86_64GPReg, imm: i32) {
    let rex = add_rm_extension(dst, REX_W);
    let dst_mod = dst as u8 % 8;
    buf.reserve(7);
    buf.extend(&[rex, 0xC7, 0xC0 + dst_mod]);
    buf.extend(&imm.to_le_bytes());
}

/// `MOV r64, imm64` -> Move imm64 to r64.
#[inline(always)]
fn mov_reg64_imm64(buf: &mut Vec<'_, u8>, dst: X86_64GPReg, imm: i64) {
    if imm <= i32::MAX as i64 && imm >= i32::MIN as i64 {
        mov_reg64_imm32(buf, dst, imm as i32)
    } else {
        let rex = add_opcode_extension(dst, REX_W);
        let dst_mod = dst as u8 % 8;
        buf.reserve(10);
        buf.extend(&[rex, 0xB8 + dst_mod]);
        buf.extend(&imm.to_le_bytes());
    }
}

/// `MOV r/m64,r64` -> Move r64 to r/m64.
#[inline(always)]
fn mov_reg64_reg64(buf: &mut Vec<'_, u8>, dst: X86_64GPReg, src: X86_64GPReg) {
    let rex = add_rm_extension(dst, REX_W);
    let rex = add_reg_extension(src, rex);
    let dst_mod = dst as u8 % 8;
    let src_mod = (src as u8 % 8) << 3;
    buf.extend(&[rex, 0x89, 0xC0 + dst_mod + src_mod]);
}

/// `MOV r64,r/m64` -> Move r/m64 to r64.
#[inline(always)]
fn mov_reg64_stack32(buf: &mut Vec<'_, u8>, dst: X86_64GPReg, offset: i32) {
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
#[inline(always)]
fn mov_stack32_reg64(buf: &mut Vec<'_, u8>, offset: i32, src: X86_64GPReg) {
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
#[inline(always)]
fn neg_reg64(buf: &mut Vec<'_, u8>, reg: X86_64GPReg) {
    let rex = add_rm_extension(reg, REX_W);
    let reg_mod = reg as u8 % 8;
    buf.extend(&[rex, 0xF7, 0xD8 + reg_mod]);
}

/// `RET` -> Near return to calling procedure.
#[inline(always)]
fn ret(buf: &mut Vec<'_, u8>) {
    buf.push(0xC3);
}

/// `SUB r/m64, imm32` -> Subtract imm32 sign-extended to 64-bits from r/m64.
#[inline(always)]
fn sub_reg64_imm32(buf: &mut Vec<'_, u8>, dst: X86_64GPReg, imm: i32) {
    // This can be optimized if the immediate is 1 byte.
    let rex = add_rm_extension(dst, REX_W);
    let dst_mod = dst as u8 % 8;
    buf.reserve(7);
    buf.extend(&[rex, 0x81, 0xE8 + dst_mod]);
    buf.extend(&imm.to_le_bytes());
}

/// `POP r64` -> Pop top of stack into r64; increment stack pointer. Cannot encode 32-bit operand size.
#[inline(always)]
fn pop_reg64(buf: &mut Vec<'_, u8>, reg: X86_64GPReg) {
    let reg_mod = reg as u8 % 8;
    if reg as u8 > 7 {
        let rex = add_opcode_extension(reg, REX);
        buf.extend(&[rex, 0x58 + reg_mod]);
    } else {
        buf.push(0x58 + reg_mod);
    }
}

/// `PUSH r64` -> Push r64,
#[inline(always)]
fn push_reg64(buf: &mut Vec<'_, u8>, reg: X86_64GPReg) {
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
    const TEST_I64: i64 = 0x1234_5678_9ABC_DEF0;

    #[test]
    fn test_add_reg64_imm32() {
        let arena = bumpalo::Bump::new();
        let mut buf = bumpalo::vec![in &arena];
        for (dst, expected) in &[
            (X86_64GPReg::RAX, [0x48, 0x81, 0xC0]),
            (X86_64GPReg::R15, [0x49, 0x81, 0xC7]),
        ] {
            buf.clear();
            add_reg64_imm32(&mut buf, *dst, TEST_I32);
            assert_eq!(expected, &buf[..3]);
            assert_eq!(TEST_I32.to_le_bytes(), &buf[3..]);
        }
    }

    #[test]
    fn test_add_reg64_reg64() {
        let arena = bumpalo::Bump::new();
        let mut buf = bumpalo::vec![in &arena];
        for ((dst, src), expected) in &[
            ((X86_64GPReg::RAX, X86_64GPReg::RAX), [0x48, 0x01, 0xC0]),
            ((X86_64GPReg::RAX, X86_64GPReg::R15), [0x4C, 0x01, 0xF8]),
            ((X86_64GPReg::R15, X86_64GPReg::RAX), [0x49, 0x01, 0xC7]),
            ((X86_64GPReg::R15, X86_64GPReg::R15), [0x4D, 0x01, 0xFF]),
        ] {
            buf.clear();
            add_reg64_reg64(&mut buf, *dst, *src);
            assert_eq!(expected, &buf[..]);
        }
    }

    #[test]
    fn test_cmovl_reg64_reg64() {
        let arena = bumpalo::Bump::new();
        let mut buf = bumpalo::vec![in &arena];
        for ((dst, src), expected) in &[
            (
                (X86_64GPReg::RAX, X86_64GPReg::RAX),
                [0x48, 0x0F, 0x4C, 0xC0],
            ),
            (
                (X86_64GPReg::RAX, X86_64GPReg::R15),
                [0x49, 0x0F, 0x4C, 0xC7],
            ),
            (
                (X86_64GPReg::R15, X86_64GPReg::RAX),
                [0x4C, 0x0F, 0x4C, 0xF8],
            ),
            (
                (X86_64GPReg::R15, X86_64GPReg::R15),
                [0x4D, 0x0F, 0x4C, 0xFF],
            ),
        ] {
            buf.clear();
            cmovl_reg64_reg64(&mut buf, *dst, *src);
            assert_eq!(expected, &buf[..]);
        }
    }

    #[test]
    fn test_mov_reg64_imm32() {
        let arena = bumpalo::Bump::new();
        let mut buf = bumpalo::vec![in &arena];
        for (dst, expected) in &[
            (X86_64GPReg::RAX, [0x48, 0xC7, 0xC0]),
            (X86_64GPReg::R15, [0x49, 0xC7, 0xC7]),
        ] {
            buf.clear();
            mov_reg64_imm32(&mut buf, *dst, TEST_I32);
            assert_eq!(expected, &buf[..3]);
            assert_eq!(TEST_I32.to_le_bytes(), &buf[3..]);
        }
    }

    #[test]
    fn test_mov_reg64_imm64() {
        let arena = bumpalo::Bump::new();
        let mut buf = bumpalo::vec![in &arena];
        for (dst, expected) in &[
            (X86_64GPReg::RAX, [0x48, 0xB8]),
            (X86_64GPReg::R15, [0x49, 0xBF]),
        ] {
            buf.clear();
            mov_reg64_imm64(&mut buf, *dst, TEST_I64);
            assert_eq!(expected, &buf[..2]);
            assert_eq!(TEST_I64.to_le_bytes(), &buf[2..]);
        }
        for (dst, expected) in &[
            (X86_64GPReg::RAX, [0x48, 0xC7, 0xC0]),
            (X86_64GPReg::R15, [0x49, 0xC7, 0xC7]),
        ] {
            buf.clear();
            mov_reg64_imm64(&mut buf, *dst, TEST_I32 as i64);
            assert_eq!(expected, &buf[..3]);
            assert_eq!(TEST_I32.to_le_bytes(), &buf[3..]);
        }
    }

    #[test]
    fn test_mov_reg64_reg64() {
        let arena = bumpalo::Bump::new();
        let mut buf = bumpalo::vec![in &arena];
        for ((dst, src), expected) in &[
            ((X86_64GPReg::RAX, X86_64GPReg::RAX), [0x48, 0x89, 0xC0]),
            ((X86_64GPReg::RAX, X86_64GPReg::R15), [0x4C, 0x89, 0xF8]),
            ((X86_64GPReg::R15, X86_64GPReg::RAX), [0x49, 0x89, 0xC7]),
            ((X86_64GPReg::R15, X86_64GPReg::R15), [0x4D, 0x89, 0xFF]),
        ] {
            buf.clear();
            mov_reg64_reg64(&mut buf, *dst, *src);
            assert_eq!(expected, &buf[..]);
        }
    }

    #[test]
    fn test_mov_reg64_stack32() {
        let arena = bumpalo::Bump::new();
        let mut buf = bumpalo::vec![in &arena];
        for ((dst, offset), expected) in &[
            ((X86_64GPReg::RAX, TEST_I32), [0x48, 0x8B, 0x84, 0x24]),
            ((X86_64GPReg::R15, TEST_I32), [0x4C, 0x8B, 0xBC, 0x24]),
        ] {
            buf.clear();
            mov_reg64_stack32(&mut buf, *dst, *offset);
            assert_eq!(expected, &buf[..4]);
            assert_eq!(TEST_I32.to_le_bytes(), &buf[4..]);
        }
    }

    #[test]
    fn test_mov_stack32_reg64() {
        let arena = bumpalo::Bump::new();
        let mut buf = bumpalo::vec![in &arena];
        for ((offset, src), expected) in &[
            ((TEST_I32, X86_64GPReg::RAX), [0x48, 0x89, 0x84, 0x24]),
            ((TEST_I32, X86_64GPReg::R15), [0x4C, 0x89, 0xBC, 0x24]),
        ] {
            buf.clear();
            mov_stack32_reg64(&mut buf, *offset, *src);
            assert_eq!(expected, &buf[..4]);
            assert_eq!(TEST_I32.to_le_bytes(), &buf[4..]);
        }
    }

    #[test]
    fn test_neg_reg64() {
        let arena = bumpalo::Bump::new();
        let mut buf = bumpalo::vec![in &arena];
        for (reg, expected) in &[
            (X86_64GPReg::RAX, [0x48, 0xF7, 0xD8]),
            (X86_64GPReg::R15, [0x49, 0xF7, 0xDF]),
        ] {
            buf.clear();
            neg_reg64(&mut buf, *reg);
            assert_eq!(expected, &buf[..]);
        }
    }

    #[test]
    fn test_ret() {
        let arena = bumpalo::Bump::new();
        let mut buf = bumpalo::vec![in &arena];
        ret(&mut buf);
        assert_eq!(&[0xC3], &buf[..]);
    }

    #[test]
    fn test_sub_reg64_imm32() {
        let arena = bumpalo::Bump::new();
        let mut buf = bumpalo::vec![in &arena];
        for (dst, expected) in &[
            (X86_64GPReg::RAX, [0x48, 0x81, 0xE8]),
            (X86_64GPReg::R15, [0x49, 0x81, 0xEF]),
        ] {
            buf.clear();
            sub_reg64_imm32(&mut buf, *dst, TEST_I32);
            assert_eq!(expected, &buf[..3]);
            assert_eq!(TEST_I32.to_le_bytes(), &buf[3..]);
        }
    }

    #[test]
    fn test_pop_reg64() {
        let arena = bumpalo::Bump::new();
        let mut buf = bumpalo::vec![in &arena];
        for (dst, expected) in &[
            (X86_64GPReg::RAX, vec![0x58]),
            (X86_64GPReg::R15, vec![0x41, 0x5F]),
        ] {
            buf.clear();
            pop_reg64(&mut buf, *dst);
            assert_eq!(&expected[..], &buf[..]);
        }
    }

    #[test]
    fn test_push_reg64() {
        let arena = bumpalo::Bump::new();
        let mut buf = bumpalo::vec![in &arena];
        for (src, expected) in &[
            (X86_64GPReg::RAX, vec![0x50]),
            (X86_64GPReg::R15, vec![0x41, 0x57]),
        ] {
            buf.clear();
            push_reg64(&mut buf, *src);
            assert_eq!(&expected[..], &buf[..]);
        }
    }
}
