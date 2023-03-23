use crate::generic64::{storage::StorageManager, Assembler, CallConv, RegTrait};
use crate::Relocation;
use bumpalo::collections::Vec;
use packed_struct::prelude::*;
use roc_builtins::bitcode::FloatWidth;
use roc_error_macros::internal_error;
use roc_module::symbol::Symbol;
use roc_mono::layout::{InLayout, STLayoutInterner};

use super::CompareOperation;

#[derive(Copy, Clone, PartialEq, Eq, Hash, PartialOrd, Ord, Debug)]
#[allow(dead_code)]
pub enum AArch64GeneralReg {
    X0 = 0,
    X1 = 1,
    X2 = 2,
    X3 = 3,
    X4 = 4,
    X5 = 5,
    X6 = 6,
    X7 = 7,
    XR = 8,
    X9 = 9,
    X10 = 10,
    X11 = 11,
    X12 = 12,
    X13 = 13,
    X14 = 14,
    X15 = 15,
    IP0 = 16,
    IP1 = 17,
    PR = 18,
    X19 = 19,
    X20 = 20,
    X21 = 21,
    X22 = 22,
    X23 = 23,
    X24 = 24,
    X25 = 25,
    X26 = 26,
    X27 = 27,
    X28 = 28,
    FP = 29,
    LR = 30,
    /// This can mean Zero or Stack Pointer depending on the context.
    ZRSP = 31,
}

impl RegTrait for AArch64GeneralReg {
    fn value(&self) -> u8 {
        *self as u8
    }
}
impl std::fmt::Display for AArch64GeneralReg {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                AArch64GeneralReg::X0 => "x0",
                AArch64GeneralReg::X1 => "x1",
                AArch64GeneralReg::X2 => "x2",
                AArch64GeneralReg::X3 => "x3",
                AArch64GeneralReg::X4 => "x4",
                AArch64GeneralReg::X5 => "x5",
                AArch64GeneralReg::X6 => "x6",
                AArch64GeneralReg::X7 => "x7",
                AArch64GeneralReg::XR => "xr",
                AArch64GeneralReg::X9 => "x9",
                AArch64GeneralReg::X10 => "x10",
                AArch64GeneralReg::X11 => "x11",
                AArch64GeneralReg::X12 => "x12",
                AArch64GeneralReg::X13 => "x13",
                AArch64GeneralReg::X14 => "x14",
                AArch64GeneralReg::X15 => "x15",
                AArch64GeneralReg::IP0 => "ip0",
                AArch64GeneralReg::IP1 => "ip1",
                AArch64GeneralReg::PR => "pr",
                AArch64GeneralReg::X19 => "x19",
                AArch64GeneralReg::X20 => "x20",
                AArch64GeneralReg::X21 => "x21",
                AArch64GeneralReg::X22 => "x22",
                AArch64GeneralReg::X23 => "x23",
                AArch64GeneralReg::X24 => "x24",
                AArch64GeneralReg::X25 => "x25",
                AArch64GeneralReg::X26 => "x26",
                AArch64GeneralReg::X27 => "x27",
                AArch64GeneralReg::X28 => "x28",
                AArch64GeneralReg::FP => "fp",
                AArch64GeneralReg::LR => "lr",
                AArch64GeneralReg::ZRSP => "zrsp",
            }
        )
    }
}

impl AArch64GeneralReg {
    #[inline(always)]
    fn id(&self) -> u8 {
        *self as u8
    }
}

#[derive(Copy, Clone, PartialEq, Eq, Hash, PartialOrd, Ord, Debug)]
#[allow(dead_code)]
pub enum AArch64FloatReg {}
impl RegTrait for AArch64FloatReg {
    fn value(&self) -> u8 {
        *self as u8
    }
}
impl std::fmt::Display for AArch64FloatReg {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "TODO",)
    }
}

#[derive(Copy, Clone)]
pub struct AArch64Assembler {}

// AArch64Call may need to eventually be split by OS,
// but I think with how we use it, they may all be the same.
#[derive(Copy, Clone)]
pub struct AArch64Call {}

const STACK_ALIGNMENT: u8 = 16;

impl CallConv<AArch64GeneralReg, AArch64FloatReg, AArch64Assembler> for AArch64Call {
    const BASE_PTR_REG: AArch64GeneralReg = AArch64GeneralReg::FP;
    const STACK_PTR_REG: AArch64GeneralReg = AArch64GeneralReg::ZRSP;

    const GENERAL_PARAM_REGS: &'static [AArch64GeneralReg] = &[
        AArch64GeneralReg::X0,
        AArch64GeneralReg::X1,
        AArch64GeneralReg::X2,
        AArch64GeneralReg::X3,
        AArch64GeneralReg::X4,
        AArch64GeneralReg::X5,
        AArch64GeneralReg::X6,
        AArch64GeneralReg::X7,
    ];
    const GENERAL_RETURN_REGS: &'static [AArch64GeneralReg] = Self::GENERAL_PARAM_REGS;
    const GENERAL_DEFAULT_FREE_REGS: &'static [AArch64GeneralReg] = &[
        // The regs we want to use first should be at the end of this vec.
        // We will use pop to get which reg to use next

        // Don't use frame pointer: AArch64GeneralReg::FP,
        // Don't user indirect result location: AArch64GeneralReg::XR,
        // Don't use platform register: AArch64GeneralReg::PR,
        // Don't use link register: AArch64GeneralReg::LR,
        // Don't use zero register/stack pointer: AArch64GeneralReg::ZRSP,

        // Use callee saved regs last.
        AArch64GeneralReg::X19,
        AArch64GeneralReg::X20,
        AArch64GeneralReg::X21,
        AArch64GeneralReg::X22,
        AArch64GeneralReg::X23,
        AArch64GeneralReg::X24,
        AArch64GeneralReg::X25,
        AArch64GeneralReg::X26,
        AArch64GeneralReg::X27,
        AArch64GeneralReg::X28,
        // Use caller saved regs first.
        AArch64GeneralReg::X0,
        AArch64GeneralReg::X1,
        AArch64GeneralReg::X2,
        AArch64GeneralReg::X3,
        AArch64GeneralReg::X4,
        AArch64GeneralReg::X5,
        AArch64GeneralReg::X6,
        AArch64GeneralReg::X7,
        AArch64GeneralReg::X9,
        AArch64GeneralReg::X10,
        AArch64GeneralReg::X11,
        AArch64GeneralReg::X12,
        AArch64GeneralReg::X13,
        AArch64GeneralReg::X14,
        AArch64GeneralReg::X15,
        AArch64GeneralReg::IP0,
        AArch64GeneralReg::IP1,
    ];
    const FLOAT_PARAM_REGS: &'static [AArch64FloatReg] = &[];
    const FLOAT_RETURN_REGS: &'static [AArch64FloatReg] = Self::FLOAT_PARAM_REGS;
    const FLOAT_DEFAULT_FREE_REGS: &'static [AArch64FloatReg] = &[];

    const SHADOW_SPACE_SIZE: u8 = 0;

    #[inline(always)]
    fn general_callee_saved(reg: &AArch64GeneralReg) -> bool {
        matches!(
            reg,
            AArch64GeneralReg::X19
                | AArch64GeneralReg::X20
                | AArch64GeneralReg::X21
                | AArch64GeneralReg::X22
                | AArch64GeneralReg::X23
                | AArch64GeneralReg::X24
                | AArch64GeneralReg::X25
                | AArch64GeneralReg::X26
                | AArch64GeneralReg::X27
                | AArch64GeneralReg::X28
        )
    }
    #[inline(always)]
    fn float_callee_saved(_reg: &AArch64FloatReg) -> bool {
        todo!("AArch64 FloatRegs");
    }

    #[inline(always)]
    fn setup_stack(
        buf: &mut Vec<'_, u8>,
        saved_general_regs: &[AArch64GeneralReg],
        saved_float_regs: &[AArch64FloatReg],
        requested_stack_size: i32,
        fn_call_stack_size: i32,
    ) -> i32 {
        // Full size is upcast to i64 to make sure we don't overflow here.
        let full_stack_size = match requested_stack_size
            .checked_add(8 * (saved_general_regs.len() + saved_float_regs.len()) as i32 + 8) // The extra 8 is space to store the frame pointer.
            .and_then(|size| size.checked_add(fn_call_stack_size))
        {
            Some(size) => size,
            _ => internal_error!("Ran out of stack space"),
        };
        let alignment = if full_stack_size <= 0 {
            0
        } else {
            full_stack_size % STACK_ALIGNMENT as i32
        };
        let offset = if alignment == 0 {
            0
        } else {
            STACK_ALIGNMENT - alignment as u8
        };
        if let Some(aligned_stack_size) = full_stack_size.checked_add(offset as i32) {
            if aligned_stack_size > 0 {
                AArch64Assembler::mov_reg64_reg64(
                    buf,
                    AArch64GeneralReg::FP,
                    AArch64GeneralReg::ZRSP,
                );
                AArch64Assembler::sub_reg64_reg64_imm32(
                    buf,
                    AArch64GeneralReg::ZRSP,
                    AArch64GeneralReg::ZRSP,
                    aligned_stack_size,
                );

                // All the following stores could be optimized by using `STP` to store pairs.
                let mut offset = aligned_stack_size;
                offset -= 8;
                AArch64Assembler::mov_stack32_reg64(buf, offset, AArch64GeneralReg::LR);
                offset -= 8;
                AArch64Assembler::mov_stack32_reg64(buf, offset, AArch64GeneralReg::FP);

                offset = aligned_stack_size - fn_call_stack_size;
                for reg in saved_general_regs {
                    offset -= 8;
                    AArch64Assembler::mov_base32_reg64(buf, offset, *reg);
                }
                for reg in saved_float_regs {
                    offset -= 8;
                    AArch64Assembler::mov_base32_freg64(buf, offset, *reg);
                }
                aligned_stack_size
            } else {
                0
            }
        } else {
            internal_error!("Ran out of stack space");
        }
    }

    #[inline(always)]
    fn cleanup_stack(
        buf: &mut Vec<'_, u8>,
        saved_general_regs: &[AArch64GeneralReg],
        saved_float_regs: &[AArch64FloatReg],
        aligned_stack_size: i32,
        fn_call_stack_size: i32,
    ) {
        if aligned_stack_size > 0 {
            // All the following stores could be optimized by using `STP` to store pairs.
            let mut offset = aligned_stack_size;
            offset -= 8;
            AArch64Assembler::mov_reg64_stack32(buf, AArch64GeneralReg::LR, offset);
            offset -= 8;
            AArch64Assembler::mov_reg64_stack32(buf, AArch64GeneralReg::FP, offset);

            offset = aligned_stack_size - fn_call_stack_size;
            for reg in saved_general_regs {
                offset -= 8;
                AArch64Assembler::mov_reg64_base32(buf, *reg, offset);
            }
            for reg in saved_float_regs {
                offset -= 8;
                AArch64Assembler::mov_freg64_base32(buf, *reg, offset);
            }
            AArch64Assembler::add_reg64_reg64_imm32(
                buf,
                AArch64GeneralReg::ZRSP,
                AArch64GeneralReg::ZRSP,
                aligned_stack_size,
            );
        }
    }

    #[inline(always)]
    fn load_args<'a, 'r>(
        _buf: &mut Vec<'a, u8>,
        _storage_manager: &mut StorageManager<
            'a,
            'r,
            AArch64GeneralReg,
            AArch64FloatReg,
            AArch64Assembler,
            AArch64Call,
        >,
        _layout_interner: &mut STLayoutInterner<'a>,
        _args: &'a [(InLayout<'a>, Symbol)],
        _ret_layout: &InLayout<'a>,
    ) {
        todo!("Loading args for AArch64");
    }

    #[inline(always)]
    fn store_args<'a, 'r>(
        _buf: &mut Vec<'a, u8>,
        _storage_manager: &mut StorageManager<
            'a,
            'r,
            AArch64GeneralReg,
            AArch64FloatReg,
            AArch64Assembler,
            AArch64Call,
        >,
        _layout_interner: &mut STLayoutInterner<'a>,
        _dst: &Symbol,
        _args: &[Symbol],
        _arg_layouts: &[InLayout<'a>],
        _ret_layout: &InLayout<'a>,
    ) {
        todo!("Storing args for AArch64");
    }

    fn return_complex_symbol<'a, 'r>(
        _buf: &mut Vec<'a, u8>,
        _storage_manager: &mut StorageManager<
            'a,
            'r,
            AArch64GeneralReg,
            AArch64FloatReg,
            AArch64Assembler,
            AArch64Call,
        >,
        _layout_interner: &mut STLayoutInterner<'a>,
        _sym: &Symbol,
        _layout: &InLayout<'a>,
    ) {
        todo!("Returning complex symbols for AArch64");
    }

    fn load_returned_complex_symbol<'a, 'r>(
        _buf: &mut Vec<'a, u8>,
        _storage_manager: &mut StorageManager<
            'a,
            'r,
            AArch64GeneralReg,
            AArch64FloatReg,
            AArch64Assembler,
            AArch64Call,
        >,
        _layout_interner: &mut STLayoutInterner<'a>,
        _sym: &Symbol,
        _layout: &InLayout<'a>,
    ) {
        todo!("Loading returned complex symbols for AArch64");
    }
}

impl Assembler<AArch64GeneralReg, AArch64FloatReg> for AArch64Assembler {
    #[inline(always)]
    fn abs_reg64_reg64(buf: &mut Vec<'_, u8>, dst: AArch64GeneralReg, src: AArch64GeneralReg) {
        cmp_reg64_imm12(buf, src, 0);
        cneg_reg64_reg64_cond(buf, dst, src, ConditionCode::MI);
    }

    #[inline(always)]
    fn abs_freg64_freg64(
        _buf: &mut Vec<'_, u8>,
        _relocs: &mut Vec<'_, Relocation>,
        _dst: AArch64FloatReg,
        _src: AArch64FloatReg,
    ) {
        todo!("abs_reg64_reg64 for AArch64");
    }

    #[inline(always)]
    fn add_reg64_reg64_imm32(
        buf: &mut Vec<'_, u8>,
        dst: AArch64GeneralReg,
        src: AArch64GeneralReg,
        imm32: i32,
    ) {
        if imm32 < 0 {
            todo!("immediate addition with values less than 0");
        } else if imm32 < 0xFFF {
            add_reg64_reg64_imm12(buf, dst, src, imm32 as u16);
        } else {
            todo!("immediate additions with values greater than 12bits");
        }
    }
    #[inline(always)]
    fn add_reg64_reg64_reg64(
        buf: &mut Vec<'_, u8>,
        dst: AArch64GeneralReg,
        src1: AArch64GeneralReg,
        src2: AArch64GeneralReg,
    ) {
        add_reg64_reg64_reg64(buf, dst, src1, src2);
    }
    #[inline(always)]
    fn add_freg32_freg32_freg32(
        _buf: &mut Vec<'_, u8>,
        _dst: AArch64FloatReg,
        _src1: AArch64FloatReg,
        _src2: AArch64FloatReg,
    ) {
        todo!("adding floats for AArch64");
    }
    #[inline(always)]
    fn add_freg64_freg64_freg64(
        _buf: &mut Vec<'_, u8>,
        _dst: AArch64FloatReg,
        _src1: AArch64FloatReg,
        _src2: AArch64FloatReg,
    ) {
        todo!("adding floats for AArch64");
    }

    #[inline(always)]
    fn call(_buf: &mut Vec<'_, u8>, _relocs: &mut Vec<'_, Relocation>, _fn_name: String) {
        todo!("calling functions literal for AArch64");
    }

    #[inline(always)]
    fn imul_reg64_reg64_reg64(
        _buf: &mut Vec<'_, u8>,
        _dst: AArch64GeneralReg,
        _src1: AArch64GeneralReg,
        _src2: AArch64GeneralReg,
    ) {
        todo!("register signed multiplication for AArch64");
    }

    fn umul_reg64_reg64_reg64<'a, 'r, ASM, CC>(
        _buf: &mut Vec<'a, u8>,
        _storage_manager: &mut StorageManager<'a, 'r, AArch64GeneralReg, AArch64FloatReg, ASM, CC>,
        _dst: AArch64GeneralReg,
        _src1: AArch64GeneralReg,
        _src2: AArch64GeneralReg,
    ) where
        ASM: Assembler<AArch64GeneralReg, AArch64FloatReg>,
        CC: CallConv<AArch64GeneralReg, AArch64FloatReg, ASM>,
    {
        todo!("register unsigned multiplication for AArch64");
    }

    fn idiv_reg64_reg64_reg64<'a, 'r, ASM, CC>(
        _buf: &mut Vec<'a, u8>,
        _storage_manager: &mut StorageManager<'a, 'r, AArch64GeneralReg, AArch64FloatReg, ASM, CC>,
        _dst: AArch64GeneralReg,
        _src1: AArch64GeneralReg,
        _src2: AArch64GeneralReg,
    ) where
        ASM: Assembler<AArch64GeneralReg, AArch64FloatReg>,
        CC: CallConv<AArch64GeneralReg, AArch64FloatReg, ASM>,
    {
        todo!("register signed division for AArch64");
    }

    fn udiv_reg64_reg64_reg64<'a, 'r, ASM, CC>(
        _buf: &mut Vec<'a, u8>,
        _storage_manager: &mut StorageManager<'a, 'r, AArch64GeneralReg, AArch64FloatReg, ASM, CC>,
        _dst: AArch64GeneralReg,
        _src1: AArch64GeneralReg,
        _src2: AArch64GeneralReg,
    ) where
        ASM: Assembler<AArch64GeneralReg, AArch64FloatReg>,
        CC: CallConv<AArch64GeneralReg, AArch64FloatReg, ASM>,
    {
        todo!("register unsigned division for AArch64");
    }

    #[inline(always)]
    fn mul_freg32_freg32_freg32(
        _buf: &mut Vec<'_, u8>,
        _dst: AArch64FloatReg,
        _src1: AArch64FloatReg,
        _src2: AArch64FloatReg,
    ) {
        todo!("multiplication for floats for AArch64");
    }
    #[inline(always)]
    fn mul_freg64_freg64_freg64(
        _buf: &mut Vec<'_, u8>,
        _dst: AArch64FloatReg,
        _src1: AArch64FloatReg,
        _src2: AArch64FloatReg,
    ) {
        todo!("multiplication for floats for AArch64");
    }

    #[inline(always)]
    fn div_freg32_freg32_freg32(
        _buf: &mut Vec<'_, u8>,
        _dst: AArch64FloatReg,
        _src1: AArch64FloatReg,
        _src2: AArch64FloatReg,
    ) {
        todo!("division for floats for AArch64");
    }
    #[inline(always)]
    fn div_freg64_freg64_freg64(
        _buf: &mut Vec<'_, u8>,
        _dst: AArch64FloatReg,
        _src1: AArch64FloatReg,
        _src2: AArch64FloatReg,
    ) {
        todo!("division for floats for AArch64");
    }

    #[inline(always)]
    fn jmp_imm32(_buf: &mut Vec<'_, u8>, _offset: i32) -> usize {
        todo!("jump instructions for AArch64");
    }

    #[inline(always)]
    fn tail_call(buf: &mut Vec<'_, u8>) -> u64 {
        Self::jmp_imm32(buf, 0);
        buf.len() as u64 - 4 // TODO is 4 the correct offset in ARM?
    }

    #[inline(always)]
    fn jne_reg64_imm64_imm32(
        _buf: &mut Vec<'_, u8>,
        _reg: AArch64GeneralReg,
        _imm: u64,
        _offset: i32,
    ) -> usize {
        todo!("jump not equal instructions for AArch64");
    }

    #[inline(always)]
    fn mov_freg32_imm32(
        _buf: &mut Vec<'_, u8>,
        _relocs: &mut Vec<'_, Relocation>,
        _dst: AArch64FloatReg,
        _imm: f32,
    ) {
        todo!("loading f32 literal for AArch64");
    }
    #[inline(always)]
    fn mov_freg64_imm64(
        _buf: &mut Vec<'_, u8>,
        _relocs: &mut Vec<'_, Relocation>,
        _dst: AArch64FloatReg,
        _imm: f64,
    ) {
        todo!("loading f64 literal for AArch64");
    }
    #[inline(always)]
    fn mov_reg64_imm64(buf: &mut Vec<'_, u8>, dst: AArch64GeneralReg, imm: i64) {
        let mut remaining = imm as u64;
        movz_reg64_imm16(buf, dst, remaining as u16, 0);
        remaining >>= 16;
        if remaining > 0 {
            movk_reg64_imm16(buf, dst, remaining as u16, 1);
        }
        remaining >>= 16;
        if remaining > 0 {
            movk_reg64_imm16(buf, dst, remaining as u16, 2);
        }
        remaining >>= 16;
        if remaining > 0 {
            movk_reg64_imm16(buf, dst, remaining as u16, 3);
        }
    }
    #[inline(always)]
    fn mov_freg64_freg64(_buf: &mut Vec<'_, u8>, _dst: AArch64FloatReg, _src: AArch64FloatReg) {
        todo!("moving data between float registers for AArch64");
    }
    #[inline(always)]
    fn mov_reg64_reg64(buf: &mut Vec<'_, u8>, dst: AArch64GeneralReg, src: AArch64GeneralReg) {
        mov_reg64_reg64(buf, dst, src);
    }

    #[inline(always)]
    fn mov_freg64_base32(_buf: &mut Vec<'_, u8>, _dst: AArch64FloatReg, _offset: i32) {
        todo!("loading floating point reg from base offset for AArch64");
    }
    #[inline(always)]
    fn mov_reg64_base32(buf: &mut Vec<'_, u8>, dst: AArch64GeneralReg, offset: i32) {
        if offset < 0 {
            todo!("negative base offsets for AArch64");
        } else if offset < (0xFFF << 8) {
            debug_assert!(offset % 8 == 0);
            ldr_reg64_reg64_imm12(buf, dst, AArch64GeneralReg::FP, (offset as u16) >> 3);
        } else {
            todo!("base offsets over 32k for AArch64");
        }
    }
    #[inline(always)]
    fn mov_reg32_base32(_buf: &mut Vec<'_, u8>, _dst: AArch64GeneralReg, _offset: i32) {
        todo!()
    }
    #[inline(always)]
    fn mov_reg16_base32(_buf: &mut Vec<'_, u8>, _dst: AArch64GeneralReg, _offset: i32) {
        todo!()
    }
    #[inline(always)]
    fn mov_reg8_base32(_buf: &mut Vec<'_, u8>, _dst: AArch64GeneralReg, _offset: i32) {
        todo!()
    }
    #[inline(always)]
    fn mov_base32_freg64(_buf: &mut Vec<'_, u8>, _offset: i32, _src: AArch64FloatReg) {
        todo!("saving floating point reg to base offset for AArch64");
    }
    #[inline(always)]
    fn movesd_mem64_offset32_freg64(
        _buf: &mut Vec<'_, u8>,
        _ptr: AArch64GeneralReg,
        _offset: i32,
        _src: AArch64FloatReg,
    ) {
        todo!()
    }

    #[inline(always)]
    fn mov_base32_reg64(buf: &mut Vec<'_, u8>, offset: i32, src: AArch64GeneralReg) {
        if offset < 0 {
            todo!("negative base offsets for AArch64");
        } else if offset < (0xFFF << 8) {
            debug_assert!(offset % 8 == 0);
            str_reg64_reg64_imm12(buf, src, AArch64GeneralReg::FP, (offset as u16) >> 3);
        } else {
            todo!("base offsets over 32k for AArch64");
        }
    }

    #[inline(always)]
    fn mov_base32_reg32(_buf: &mut Vec<'_, u8>, _offset: i32, _src: AArch64GeneralReg) {
        todo!()
    }
    #[inline(always)]
    fn mov_base32_reg16(_buf: &mut Vec<'_, u8>, _offset: i32, _src: AArch64GeneralReg) {
        todo!()
    }
    #[inline(always)]
    fn mov_base32_reg8(_buf: &mut Vec<'_, u8>, _offset: i32, _src: AArch64GeneralReg) {
        todo!()
    }

    #[inline(always)]
    fn mov_reg64_mem64_offset32(
        buf: &mut Vec<'_, u8>,
        dst: AArch64GeneralReg,
        src: AArch64GeneralReg,
        offset: i32,
    ) {
        if offset < 0 {
            todo!("negative mem offsets for AArch64");
        } else if offset < (0xFFF << 8) {
            debug_assert!(offset % 8 == 0);
            ldr_reg64_reg64_imm12(buf, dst, src, (offset as u16) >> 3);
        } else {
            todo!("mem offsets over 32k for AArch64");
        }
    }
    #[inline(always)]
    fn mov_reg32_mem32_offset32(
        buf: &mut Vec<'_, u8>,
        dst: AArch64GeneralReg,
        src: AArch64GeneralReg,
        offset: i32,
    ) {
        if offset < 0 {
            todo!("negative mem offsets for AArch64");
        } else if offset < (0xFFF << 8) {
            debug_assert!(offset % 8 == 0);
            ldr_reg64_reg64_imm12(buf, dst, src, (offset as u16) >> 3);
        } else {
            todo!("mem offsets over 32k for AArch64");
        }
    }
    #[inline(always)]
    fn mov_reg16_mem16_offset32(
        _buf: &mut Vec<'_, u8>,
        _dst: AArch64GeneralReg,
        _src: AArch64GeneralReg,
        _offset: i32,
    ) {
        todo!()
    }
    #[inline(always)]
    fn mov_reg8_mem8_offset32(
        _buf: &mut Vec<'_, u8>,
        _dst: AArch64GeneralReg,
        _src: AArch64GeneralReg,
        _offset: i32,
    ) {
        todo!()
    }

    #[inline(always)]
    fn mov_mem64_offset32_reg64(
        buf: &mut Vec<'_, u8>,
        dst: AArch64GeneralReg,
        offset: i32,
        src: AArch64GeneralReg,
    ) {
        if offset < 0 {
            todo!("negative mem offsets for AArch64");
        } else if offset < (0xFFF << 8) {
            debug_assert!(offset % 8 == 0);
            str_reg64_reg64_imm12(buf, src, dst, (offset as u16) >> 3);
        } else {
            todo!("mem offsets over 32k for AArch64");
        }
    }

    #[inline(always)]
    fn mov_mem32_offset32_reg32(
        _buf: &mut Vec<'_, u8>,
        _dst: AArch64GeneralReg,
        _offset: i32,
        _src: AArch64GeneralReg,
    ) {
        todo!()
    }

    #[inline(always)]
    fn mov_mem16_offset32_reg16(
        _buf: &mut Vec<'_, u8>,
        _dst: AArch64GeneralReg,
        _offset: i32,
        _src: AArch64GeneralReg,
    ) {
        todo!()
    }

    #[inline(always)]
    fn mov_mem8_offset32_reg8(
        _buf: &mut Vec<'_, u8>,
        _dst: AArch64GeneralReg,
        _offset: i32,
        _src: AArch64GeneralReg,
    ) {
        todo!()
    }

    #[inline(always)]
    fn movsx_reg64_base32(buf: &mut Vec<'_, u8>, dst: AArch64GeneralReg, offset: i32, size: u8) {
        debug_assert!(size <= 8);
        if size == 8 {
            Self::mov_reg64_base32(buf, dst, offset);
        } else if size == 4 {
            todo!("sign extending 4 byte values");
        } else if size == 2 {
            todo!("sign extending 2 byte values");
        } else if size == 1 {
            todo!("sign extending 1 byte values");
        } else {
            internal_error!("Invalid size for sign extension: {}", size);
        }
    }
    #[inline(always)]
    fn movzx_reg64_base32(buf: &mut Vec<'_, u8>, dst: AArch64GeneralReg, offset: i32, size: u8) {
        debug_assert!(size <= 8);
        if size == 8 {
            Self::mov_reg64_base32(buf, dst, offset);
        } else if size == 4 {
            todo!("zero extending 4 byte values");
        } else if size == 2 {
            todo!("zero extending 2 byte values");
        } else if size == 1 {
            todo!("zero extending 1 byte values");
        } else {
            internal_error!("Invalid size for zero extension: {}", size);
        }
    }

    #[inline(always)]
    fn mov_freg64_stack32(_buf: &mut Vec<'_, u8>, _dst: AArch64FloatReg, _offset: i32) {
        todo!("loading floating point reg from stack for AArch64");
    }
    #[inline(always)]
    fn mov_reg64_stack32(buf: &mut Vec<'_, u8>, dst: AArch64GeneralReg, offset: i32) {
        if offset < 0 {
            todo!("negative stack offsets for AArch64");
        } else if offset < (0xFFF << 8) {
            debug_assert!(offset % 8 == 0);
            ldr_reg64_reg64_imm12(buf, dst, AArch64GeneralReg::ZRSP, (offset as u16) >> 3);
        } else {
            todo!("stack offsets over 32k for AArch64");
        }
    }
    #[inline(always)]
    fn mov_stack32_freg64(_buf: &mut Vec<'_, u8>, _offset: i32, _src: AArch64FloatReg) {
        todo!("saving floating point reg to stack for AArch64");
    }
    #[inline(always)]
    fn mov_stack32_reg64(buf: &mut Vec<'_, u8>, offset: i32, src: AArch64GeneralReg) {
        if offset < 0 {
            todo!("negative stack offsets for AArch64");
        } else if offset < (0xFFF << 8) {
            debug_assert!(offset % 8 == 0);
            str_reg64_reg64_imm12(buf, src, AArch64GeneralReg::ZRSP, (offset as u16) >> 3);
        } else {
            todo!("stack offsets over 32k for AArch64");
        }
    }
    #[inline(always)]
    fn neg_reg64_reg64(buf: &mut Vec<'_, u8>, dst: AArch64GeneralReg, src: AArch64GeneralReg) {
        neg_reg64_reg64(buf, dst, src);
    }

    #[inline(always)]
    fn sub_reg64_reg64_imm32(
        buf: &mut Vec<'_, u8>,
        dst: AArch64GeneralReg,
        src: AArch64GeneralReg,
        imm32: i32,
    ) {
        if imm32 < 0 {
            todo!("immediate subtractions with values less than 0");
        } else if imm32 < 0xFFF {
            sub_reg64_reg64_imm12(buf, dst, src, imm32 as u16);
        } else {
            todo!("immediate subtractions with values greater than 12bits");
        }
    }
    #[inline(always)]
    fn sub_reg64_reg64_reg64(
        buf: &mut Vec<'_, u8>,
        dst: AArch64GeneralReg,
        src1: AArch64GeneralReg,
        src2: AArch64GeneralReg,
    ) {
        sub_reg64_reg64_reg64(buf, dst, src1, src2);
    }

    #[inline(always)]
    fn eq_reg64_reg64_reg64(
        buf: &mut Vec<'_, u8>,
        dst: AArch64GeneralReg,
        src1: AArch64GeneralReg,
        src2: AArch64GeneralReg,
    ) {
        cmp_reg64_reg64(buf, src1, src2);
        cset_reg64_cond(buf, dst, ConditionCode::EQ);
    }

    #[inline(always)]
    fn neq_reg64_reg64_reg64(
        buf: &mut Vec<'_, u8>,
        dst: AArch64GeneralReg,
        src1: AArch64GeneralReg,
        src2: AArch64GeneralReg,
    ) {
        cmp_reg64_reg64(buf, src1, src2);
        cset_reg64_cond(buf, dst, ConditionCode::NE);
    }

    #[inline(always)]
    fn ilt_reg64_reg64_reg64(
        buf: &mut Vec<'_, u8>,
        dst: AArch64GeneralReg,
        src1: AArch64GeneralReg,
        src2: AArch64GeneralReg,
    ) {
        cmp_reg64_reg64(buf, src1, src2);
        cset_reg64_cond(buf, dst, ConditionCode::LT);
    }

    #[inline(always)]
    fn ult_reg64_reg64_reg64(
        buf: &mut Vec<'_, u8>,
        dst: AArch64GeneralReg,
        src1: AArch64GeneralReg,
        src2: AArch64GeneralReg,
    ) {
        cmp_reg64_reg64(buf, src1, src2);
        cset_reg64_cond(buf, dst, ConditionCode::CCLO);
    }

    #[inline(always)]
    fn cmp_freg_freg_reg64(
        _buf: &mut Vec<'_, u8>,
        _dst: AArch64GeneralReg,
        _src1: AArch64FloatReg,
        _src2: AArch64FloatReg,
        _width: FloatWidth,
        _operation: CompareOperation,
    ) {
        todo!("registers float comparison for AArch64");
    }

    #[inline(always)]
    fn igt_reg64_reg64_reg64(
        buf: &mut Vec<'_, u8>,
        dst: AArch64GeneralReg,
        src1: AArch64GeneralReg,
        src2: AArch64GeneralReg,
    ) {
        cmp_reg64_reg64(buf, src1, src2);
        cset_reg64_cond(buf, dst, ConditionCode::GT);
    }

    #[inline(always)]
    fn ugt_reg64_reg64_reg64(
        buf: &mut Vec<'_, u8>,
        dst: AArch64GeneralReg,
        src1: AArch64GeneralReg,
        src2: AArch64GeneralReg,
    ) {
        cmp_reg64_reg64(buf, src1, src2);
        cset_reg64_cond(buf, dst, ConditionCode::HI);
    }

    #[inline(always)]
    fn to_float_freg64_reg64(
        _buf: &mut Vec<'_, u8>,
        _dst: AArch64FloatReg,
        _src: AArch64GeneralReg,
    ) {
        todo!("registers to float for AArch64");
    }

    #[inline(always)]
    fn to_float_freg32_reg64(
        _buf: &mut Vec<'_, u8>,
        _dst: AArch64FloatReg,
        _src: AArch64GeneralReg,
    ) {
        todo!("registers to float for AArch64");
    }

    #[inline(always)]
    fn to_float_freg32_freg64(
        _buf: &mut Vec<'_, u8>,
        _dst: AArch64FloatReg,
        _src: AArch64FloatReg,
    ) {
        todo!("registers to float for AArch64");
    }

    #[inline(always)]
    fn to_float_freg64_freg32(
        _buf: &mut Vec<'_, u8>,
        _dst: AArch64FloatReg,
        _src: AArch64FloatReg,
    ) {
        todo!("registers to float for AArch64");
    }

    // TODO: This next two are signed. Should they be?
    #[inline(always)]
    fn lte_reg64_reg64_reg64(
        buf: &mut Vec<'_, u8>,
        dst: AArch64GeneralReg,
        src1: AArch64GeneralReg,
        src2: AArch64GeneralReg,
    ) {
        cmp_reg64_reg64(buf, src1, src2);
        cset_reg64_cond(buf, dst, ConditionCode::LE);
    }

    #[inline(always)]
    fn gte_reg64_reg64_reg64(
        buf: &mut Vec<'_, u8>,
        dst: AArch64GeneralReg,
        src1: AArch64GeneralReg,
        src2: AArch64GeneralReg,
    ) {
        cmp_reg64_reg64(buf, src1, src2);
        cset_reg64_cond(buf, dst, ConditionCode::GE);
    }

    fn set_if_overflow(_buf: &mut Vec<'_, u8>, _dst: AArch64GeneralReg) {
        todo!("set if overflow for AArch64");
    }

    #[inline(always)]
    fn ret(buf: &mut Vec<'_, u8>) {
        ret_reg64(buf, AArch64GeneralReg::LR)
    }

    fn and_reg64_reg64_reg64(
        buf: &mut Vec<'_, u8>,
        dst: AArch64GeneralReg,
        src1: AArch64GeneralReg,
        src2: AArch64GeneralReg,
    ) {
        and_reg64_reg64_reg64(buf, dst, src1, src2);
    }

    fn or_reg64_reg64_reg64(
        buf: &mut Vec<'_, u8>,
        dst: AArch64GeneralReg,
        src1: AArch64GeneralReg,
        src2: AArch64GeneralReg,
    ) {
        orr_reg64_reg64_reg64(buf, dst, src1, src2);
    }

    fn xor_reg64_reg64_reg64(
        buf: &mut Vec<'_, u8>,
        dst: AArch64GeneralReg,
        src1: AArch64GeneralReg,
        src2: AArch64GeneralReg,
    ) {
        eor_reg64_reg64_reg64(buf, dst, src1, src2);
    }

    fn shl_reg64_reg64_reg64<'a, 'r, ASM, CC>(
        buf: &mut Vec<'a, u8>,
        _storage_manager: &mut StorageManager<'a, 'r, AArch64GeneralReg, AArch64FloatReg, ASM, CC>,
        dst: AArch64GeneralReg,
        src1: AArch64GeneralReg,
        src2: AArch64GeneralReg,
    ) where
        ASM: Assembler<AArch64GeneralReg, AArch64FloatReg>,
        CC: CallConv<AArch64GeneralReg, AArch64FloatReg, ASM>,
    {
        lsl_reg64_reg64_reg64(buf, dst, src1, src2);
    }

    fn shr_reg64_reg64_reg64<'a, 'r, ASM, CC>(
        buf: &mut Vec<'a, u8>,
        _storage_manager: &mut StorageManager<'a, 'r, AArch64GeneralReg, AArch64FloatReg, ASM, CC>,
        dst: AArch64GeneralReg,
        src1: AArch64GeneralReg,
        src2: AArch64GeneralReg,
    ) where
        ASM: Assembler<AArch64GeneralReg, AArch64FloatReg>,
        CC: CallConv<AArch64GeneralReg, AArch64FloatReg, ASM>,
    {
        lsr_reg64_reg64_reg64(buf, dst, src1, src2);
    }

    fn sar_reg64_reg64_reg64<'a, 'r, ASM, CC>(
        buf: &mut Vec<'a, u8>,
        _storage_manager: &mut StorageManager<'a, 'r, AArch64GeneralReg, AArch64FloatReg, ASM, CC>,
        dst: AArch64GeneralReg,
        src1: AArch64GeneralReg,
        src2: AArch64GeneralReg,
    ) where
        ASM: Assembler<AArch64GeneralReg, AArch64FloatReg>,
        CC: CallConv<AArch64GeneralReg, AArch64FloatReg, ASM>,
    {
        asr_reg64_reg64_reg64(buf, dst, src1, src2);
    }

    fn sqrt_freg64_freg64(_buf: &mut Vec<'_, u8>, _dst: AArch64FloatReg, _src: AArch64FloatReg) {
        todo!("sqrt")
    }

    fn sqrt_freg32_freg32(_buf: &mut Vec<'_, u8>, _dst: AArch64FloatReg, _src: AArch64FloatReg) {
        todo!("sqrt")
    }
}

impl AArch64Assembler {}

// Instructions
// ARM manual section C3
// https://developer.arm.com/documentation/ddi0487/ga
// Map all instructions to a packed struct.

trait Aarch64Bytes: PackedStruct {
    #[inline(always)]
    fn bytes(&self) -> [u8; 4] {
        let mut bytes: [u8; 4] = [0, 0, 0, 0];

        self.pack_to_slice(&mut bytes).unwrap();

        bytes.reverse();

        bytes
    }
}

#[derive(PackedStruct, Debug)]
#[packed_struct(endian = "msb")]
pub struct MoveWideImmediate {
    sf: bool,
    opc: Integer<u8, packed_bits::Bits<2>>,
    fixed: Integer<u8, packed_bits::Bits<6>>, // = 0b100101,
    hw: Integer<u8, packed_bits::Bits<2>>,
    imm16: u16,
    reg_d: Integer<u8, packed_bits::Bits<5>>, // AArch64GeneralReg
}

impl Aarch64Bytes for MoveWideImmediate {}

impl MoveWideImmediate {
    #[inline(always)]
    fn new(opc: u8, rd: AArch64GeneralReg, imm16: u16, hw: u8, sf: bool) -> Self {
        // TODO: revisit this is we change where we want to check the shift
        // currently this is done in the assembler above
        // assert!(shift % 16 == 0 && shift <= 48);
        debug_assert!(hw <= 0b11);
        debug_assert!(opc <= 0b11);

        Self {
            reg_d: rd.id().into(),
            imm16,
            hw: hw.into(),
            opc: opc.into(),
            sf,
            fixed: 0b100101.into(),
        }
    }
}

#[derive(PackedStruct, Debug)]
#[packed_struct(endian = "msb")]
pub struct ArithmeticImmediate {
    sf: bool,
    op: bool, // add or subtract
    s: bool,
    fixed: Integer<u8, packed_bits::Bits<6>>, // = 0b100010,
    sh: bool,                                 // shift
    imm12: Integer<u16, packed_bits::Bits<12>>,
    reg_n: Integer<u8, packed_bits::Bits<5>>,
    reg_d: Integer<u8, packed_bits::Bits<5>>,
}

impl Aarch64Bytes for ArithmeticImmediate {}

impl ArithmeticImmediate {
    #[inline(always)]
    fn new(
        op: bool,
        s: bool,
        rd: AArch64GeneralReg,
        rn: AArch64GeneralReg,
        imm12: u16,
        sh: bool,
    ) -> Self {
        debug_assert!(imm12 <= 0xFFF);

        Self {
            reg_d: rd.id().into(),
            reg_n: rn.id().into(),
            imm12: imm12.into(),
            sh,
            s,
            op,
            // true for 64 bit addition
            // false for 32 bit addition
            sf: true,
            fixed: 0b100010.into(),
        }
    }
}

#[derive(Clone, Copy)]
#[allow(dead_code)]
enum ShiftType {
    LSL = 0,
    LSR = 1,
    ASR = 2,
    ROR = 3,
}

impl ShiftType {
    #[inline(always)]
    fn id(&self) -> u8 {
        *self as u8
    }
}

#[derive(PackedStruct)]
#[packed_struct(endian = "msb")]
pub struct ArithmeticShifted {
    sf: bool,
    op: bool, // add or subtract
    s: bool,
    fixed: Integer<u8, packed_bits::Bits<5>>, // = 0b01011,
    shift: Integer<u8, packed_bits::Bits<2>>, // shift
    fixed2: bool,                             // = 0b0,
    reg_m: Integer<u8, packed_bits::Bits<5>>,
    imm6: Integer<u8, packed_bits::Bits<6>>,
    reg_n: Integer<u8, packed_bits::Bits<5>>,
    reg_d: Integer<u8, packed_bits::Bits<5>>,
}

impl Aarch64Bytes for ArithmeticShifted {}

impl ArithmeticShifted {
    #[inline(always)]
    fn new(
        op: bool,
        s: bool,
        shift: ShiftType,
        imm6: u8,
        rm: AArch64GeneralReg,
        rn: AArch64GeneralReg,
        rd: AArch64GeneralReg,
    ) -> Self {
        debug_assert!(imm6 <= 0b111111);

        Self {
            reg_d: rd.id().into(),
            reg_n: rn.id().into(),
            imm6: imm6.into(),
            reg_m: rm.id().into(),
            fixed2: false,
            shift: shift.id().into(),
            fixed: 0b01011.into(),
            s,
            op,
            // true for 64 bit addition
            // false for 32 bit addition
            sf: true,
        }
    }
}

// ARM manual section C1.2.4
#[derive(Copy, Clone, PartialEq)]
enum ConditionCode {
    EQ = 0b0000,
    NE = 0b0001,
    CSHS = 0b0010,
    CCLO = 0b0011,
    MI = 0b0100,
    PL = 0b0101,
    VS = 0b0110,
    VC = 0b0111,
    HI = 0b1000,
    LS = 0b1001,
    GE = 0b1010,
    LT = 0b1011,
    GT = 0b1100,
    LE = 0b1101,
    AL = 0b1110,
}

impl ConditionCode {
    #[inline(always)]
    fn id(&self) -> u8 {
        *self as u8
    }

    fn invert(self) -> Self {
        // TODO: check
        match self {
            ConditionCode::EQ => ConditionCode::NE,
            ConditionCode::NE => ConditionCode::EQ,
            ConditionCode::CSHS => ConditionCode::CCLO,
            ConditionCode::CCLO => ConditionCode::CSHS,
            ConditionCode::MI => ConditionCode::PL,
            ConditionCode::PL => ConditionCode::MI,
            ConditionCode::VS => ConditionCode::VC,
            ConditionCode::VC => ConditionCode::VS,
            ConditionCode::HI => ConditionCode::LS,
            ConditionCode::LS => ConditionCode::HI,
            ConditionCode::GE => ConditionCode::LT,
            ConditionCode::LT => ConditionCode::GE,
            ConditionCode::GT => ConditionCode::LE,
            ConditionCode::LE => ConditionCode::GT,
            ConditionCode::AL => ConditionCode::AL,
        }
    }
}

impl std::fmt::Display for ConditionCode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                ConditionCode::EQ => "eq",
                ConditionCode::NE => "ne",
                ConditionCode::CSHS => "hs",
                ConditionCode::CCLO => "lo",
                ConditionCode::MI => "mi",
                ConditionCode::PL => "pl",
                ConditionCode::VS => "vs",
                ConditionCode::VC => "vc",
                ConditionCode::HI => "hi",
                ConditionCode::LS => "ls",
                ConditionCode::GE => "ge",
                ConditionCode::LT => "lt",
                ConditionCode::GT => "gt",
                ConditionCode::LE => "le",
                ConditionCode::AL => "al",
            }
        )
    }
}

const ALL_CONDITIONS: &'static [ConditionCode] = &[
    ConditionCode::EQ,
    ConditionCode::NE,
    ConditionCode::CSHS,
    ConditionCode::CCLO,
    ConditionCode::MI,
    ConditionCode::PL,
    ConditionCode::VS,
    ConditionCode::VC,
    ConditionCode::HI,
    ConditionCode::LS,
    ConditionCode::GE,
    ConditionCode::LT,
    ConditionCode::GT,
    ConditionCode::LE,
    ConditionCode::AL,
];

#[derive(PackedStruct)]
#[packed_struct(endian = "msb")]
pub struct ConditionalBranchImmediate {
    fixed: Integer<u8, packed_bits::Bits<7>>,
    o1: bool,
    imm19: Integer<u32, packed_bits::Bits<19>>,
    o0: bool,
    cond: Integer<u8, packed_bits::Bits<4>>,
}

impl Aarch64Bytes for ConditionalBranchImmediate {}

impl ConditionalBranchImmediate {
    #[inline(always)]
    fn new(cond: ConditionCode, imm19: u32) -> Self {
        debug_assert!(imm19 >> 19 == 0);

        Self {
            cond: cond.id().into(),
            o0: false,
            imm19: imm19.into(),
            o1: false,
            fixed: 0b0101010.into(),
        }
    }
}

#[derive(PackedStruct)]
#[packed_struct(endian = "msb")]
pub struct ConditionalSelect {
    sf: bool,
    op: bool,
    s: bool,
    fixed: Integer<u8, packed_bits::Bits<8>>,
    reg_m: Integer<u8, packed_bits::Bits<5>>,
    cond: Integer<u8, packed_bits::Bits<4>>,
    op2: Integer<u8, packed_bits::Bits<2>>,
    reg_n: Integer<u8, packed_bits::Bits<5>>,
    reg_d: Integer<u8, packed_bits::Bits<5>>,
}

impl Aarch64Bytes for ConditionalSelect {}

impl ConditionalSelect {
    #[inline(always)]
    fn new(
        op: bool,
        s: bool,
        cond: ConditionCode,
        op2: u8,
        rm: AArch64GeneralReg,
        rn: AArch64GeneralReg,
        rd: AArch64GeneralReg,
    ) -> Self {
        debug_assert!(op2 <= 0b11);

        Self {
            reg_d: rd.id().into(),
            reg_n: rn.id().into(),
            op2: op2.into(),
            cond: cond.id().into(),
            reg_m: rm.id().into(),
            fixed: 0b11010100.into(),
            s,
            op,
            // true for 64 bit addition
            // false for 32 bit addition
            sf: true,
        }
    }
}

#[derive(PackedStruct)]
#[packed_struct(endian = "msb")]
pub struct DataProcessingTwoSource {
    sf: bool,
    fixed: bool,
    s: bool,
    fixed2: Integer<u8, packed_bits::Bits<8>>,
    reg_m: Integer<u8, packed_bits::Bits<5>>,
    op: Integer<u8, packed_bits::Bits<6>>,
    reg_n: Integer<u8, packed_bits::Bits<5>>,
    reg_d: Integer<u8, packed_bits::Bits<5>>,
}

impl Aarch64Bytes for DataProcessingTwoSource {}

impl DataProcessingTwoSource {
    #[inline(always)]
    fn new(op: u8, rm: AArch64GeneralReg, rn: AArch64GeneralReg, rd: AArch64GeneralReg) -> Self {
        debug_assert!(op <= 0b111111);

        Self {
            sf: true,
            fixed: false,
            s: false,
            fixed2: 0b11010110.into(),
            reg_m: rm.id().into(),
            op: op.into(),
            reg_n: rn.id().into(),
            reg_d: rd.id().into(),
        }
    }
}

#[derive(Debug)]
#[allow(dead_code)]
enum LogicalOp {
    AND,
    BIC,
    ORR,
    ORN,
    EOR,
    EON,
    ANDS,
    BICS,
}

#[derive(PackedStruct)]
#[packed_struct(endian = "msb")]
pub struct LogicalShiftedRegister {
    sf: bool,
    op: Integer<u8, packed_bits::Bits<2>>,
    fixed: Integer<u8, packed_bits::Bits<5>>, // = 0b01010,
    shift: Integer<u8, packed_bits::Bits<2>>, // shift
    n: bool,
    reg_m: Integer<u8, packed_bits::Bits<5>>,
    imm6: Integer<u8, packed_bits::Bits<6>>,
    reg_n: Integer<u8, packed_bits::Bits<5>>,
    reg_d: Integer<u8, packed_bits::Bits<5>>,
}

impl Aarch64Bytes for LogicalShiftedRegister {}

impl LogicalShiftedRegister {
    #[inline(always)]
    fn new(
        op: LogicalOp,
        shift: ShiftType,
        imm6: u8,
        rm: AArch64GeneralReg,
        rn: AArch64GeneralReg,
        rd: AArch64GeneralReg,
    ) -> Self {
        debug_assert!(imm6 <= 0b111111);

        let (op, n) = match op {
            LogicalOp::AND => (0b00, false),
            LogicalOp::BIC => (0b00, true),
            LogicalOp::ORR => (0b01, false),
            LogicalOp::ORN => (0b01, true),
            LogicalOp::EOR => (0b10, false),
            LogicalOp::EON => (0b10, true),
            LogicalOp::ANDS => (0b11, false),
            LogicalOp::BICS => (0b11, true),
        };

        Self {
            reg_d: rd.id().into(),
            reg_n: rn.id().into(),
            imm6: imm6.into(),
            reg_m: rm.id().into(),
            n,
            shift: shift.id().into(),
            fixed: 0b01010.into(),
            op: op.into(),
            // true for 64 bit addition
            // false for 32 bit addition
            sf: true,
        }
    }
}

#[derive(PackedStruct)]
pub struct UnconditionalBranchRegister {
    fixed: Integer<u8, packed_bits::Bits<7>>,
    z: bool,
    fixed2: bool,
    op: Integer<u8, packed_bits::Bits<2>>,
    fixed3: Integer<u8, packed_bits::Bits<5>>,
    fixed4: Integer<u8, packed_bits::Bits<4>>,
    a: bool,
    m: bool,
    rn: Integer<u8, packed_bits::Bits<5>>,
    fixed5: Integer<u8, packed_bits::Bits<5>>,
}

impl Aarch64Bytes for UnconditionalBranchRegister {}

impl UnconditionalBranchRegister {
    #[inline(always)]
    fn new(op: u8, rn: AArch64GeneralReg) -> Self {
        debug_assert!(op <= 0b11);

        Self {
            fixed5: 0b00000.into(),
            rn: rn.id().into(),
            m: false,
            a: false,
            fixed4: 0b0000.into(),
            fixed3: 0b11111.into(),
            op: op.into(),
            fixed2: false,
            z: false,
            fixed: 0b1101011.into(),
        }
    }
}

#[derive(PackedStruct)]
#[packed_struct(endian = "msb")]
pub struct UnconditionalBranchImmediate {
    op: bool, // false=B, true=BL
    fixed: Integer<u8, packed_bits::Bits<5>>,
    imm26: Integer<u32, packed_bits::Bits<26>>,
}

impl Aarch64Bytes for UnconditionalBranchImmediate {}

impl UnconditionalBranchImmediate {
    #[inline(always)]
    fn new(op: bool, imm26: u32) -> Self {
        debug_assert!(imm26 <= 0b11_1111_1111_1111_1111_1111_1111);
        Self {
            op,
            fixed: 0b00101.into(),
            imm26: imm26.into(),
        }
    }
}

// Uses unsigned Offset
// opc = 0b01 means load
// opc = 0b00 means store
#[derive(PackedStruct, Debug)]
#[packed_struct(endian = "msb")]
pub struct LoadStoreRegisterImmediate {
    size: Integer<u8, packed_bits::Bits<2>>,
    fixed: Integer<u8, packed_bits::Bits<3>>, // = 0b111,
    fixed2: bool,
    fixed3: Integer<u8, packed_bits::Bits<2>>,
    opc: Integer<u8, packed_bits::Bits<2>>,
    imm12: Integer<u16, packed_bits::Bits<12>>,
    rn: Integer<u8, packed_bits::Bits<5>>,
    rt: Integer<u8, packed_bits::Bits<5>>,
}

impl Aarch64Bytes for LoadStoreRegisterImmediate {}

impl LoadStoreRegisterImmediate {
    #[inline(always)]
    fn new(size: u8, opc: u8, imm12: u16, rn: AArch64GeneralReg, rt: AArch64GeneralReg) -> Self {
        debug_assert!(size <= 0b11);
        debug_assert!(imm12 <= 0xFFF);

        Self {
            rt: rt.id().into(),
            rn: rn.id().into(),
            imm12: imm12.into(),
            opc: opc.into(),
            fixed3: 0b01.into(),
            fixed2: false,
            fixed: 0b111.into(),
            size: size.into(),
        }
    }

    #[inline(always)]
    fn new_load(size: u8, imm12: u16, rn: AArch64GeneralReg, rt: AArch64GeneralReg) -> Self {
        Self::new(size, 0b01, imm12, rn, rt)
    }

    #[inline(always)]
    fn new_store(size: u8, imm12: u16, rn: AArch64GeneralReg, rt: AArch64GeneralReg) -> Self {
        Self::new(size, 0b00, imm12, rn, rt)
    }
}

// Below here are the functions for all of the assembly instructions.
// Their names are based on the instruction and operators combined.
// You should call `buf.reserve()` if you push or extend more than once.
// Unit tests are added at the bottom of the file to ensure correct asm generation.
// Please keep these in alphanumeric order.

/// `ADD Xd, Xn, imm12` -> Add Xn and imm12 and place the result into Xd.
#[inline(always)]
fn add_reg64_reg64_imm12(
    buf: &mut Vec<'_, u8>,
    dst: AArch64GeneralReg,
    src: AArch64GeneralReg,
    imm12: u16,
) {
    let inst = ArithmeticImmediate::new(false, false, dst, src, imm12, false);

    buf.extend(inst.bytes());
}

/// `ADD Xd, Xm, Xn` -> Add Xm and Xn and place the result into Xd.
#[inline(always)]
fn add_reg64_reg64_reg64(
    buf: &mut Vec<'_, u8>,
    dst: AArch64GeneralReg,
    src1: AArch64GeneralReg,
    src2: AArch64GeneralReg,
) {
    let inst = ArithmeticShifted::new(false, false, ShiftType::LSL, 0, src2, src1, dst);

    buf.extend(inst.bytes());
}

#[inline(always)]
fn and_reg64_reg64_reg64(
    buf: &mut Vec<'_, u8>,
    dst: AArch64GeneralReg,
    src1: AArch64GeneralReg,
    src2: AArch64GeneralReg,
) {
    let inst = LogicalShiftedRegister::new(LogicalOp::AND, ShiftType::LSL, 0, src2, src1, dst);

    buf.extend(inst.bytes());
}

/// TODO
#[inline(always)]
fn b_cond(buf: &mut Vec<'_, u8>, cond: ConditionCode, imm19: i32) {
    debug_assert!(imm19 & 0b11 == 0, "branch location must be 4-byte aligned");
    let shifted = imm19 >> 2;
    let unsigned = shifted as u32;
    let left_removed = (unsigned << 13) >> 13;
    if imm19 >= 0 {
        debug_assert!(left_removed == unsigned);
    } else {
        debug_assert!(left_removed | 0b1111_1111_1111_1100_0000_0000_0000_0000 == unsigned);
    }

    let inst = ConditionalBranchImmediate::new(cond, left_removed);

    buf.extend(inst.bytes());
}

/// `B imm26` -> Jump to PC + imm26.
#[inline(always)]
fn b_imm26(buf: &mut Vec<'_, u8>, imm26: i32) {
    debug_assert!(imm26 & 0b11 == 0, "branch location must be 4-byte aligned");
    let shifted = imm26 >> 2;
    let unsigned = shifted as u32;
    let left_removed = (unsigned << 6) >> 6;
    if imm26 >= 0 {
        debug_assert!(left_removed == unsigned);
    } else {
        debug_assert!(left_removed | 0b1111_1110_0000_0000_0000_0000_0000_0000 == unsigned);
    }

    let inst = UnconditionalBranchImmediate::new(false, left_removed);

    buf.extend(inst.bytes());
}

#[inline(always)]
fn cmp_reg64_imm12(buf: &mut Vec<'_, u8>, src: AArch64GeneralReg, imm12: u16) {
    subs_reg64_reg64_imm12(buf, AArch64GeneralReg::ZRSP, src, imm12);
}

#[inline(always)]
fn cmp_reg64_reg64(buf: &mut Vec<'_, u8>, src1: AArch64GeneralReg, src2: AArch64GeneralReg) {
    subs_reg64_reg64_reg64(buf, AArch64GeneralReg::ZRSP, src1, src2);
}

#[inline(always)]
fn cneg_reg64_reg64_cond(
    buf: &mut Vec<'_, u8>,
    dst: AArch64GeneralReg,
    src: AArch64GeneralReg,
    cond: ConditionCode,
) {
    csneg_reg64_reg64_reg64_cond(buf, dst, src, src, cond.invert());
}

#[inline(always)]
fn cset_reg64_cond(buf: &mut Vec<'_, u8>, dst: AArch64GeneralReg, cond: ConditionCode) {
    csinc_reg64_reg64_reg64_cond(
        buf,
        dst,
        AArch64GeneralReg::ZRSP,
        AArch64GeneralReg::ZRSP,
        cond.invert(),
    );
}

#[inline(always)]
fn csinc_reg64_reg64_reg64_cond(
    buf: &mut Vec<'_, u8>,
    dst: AArch64GeneralReg,
    src1: AArch64GeneralReg,
    src2: AArch64GeneralReg,
    cond: ConditionCode,
) {
    let inst = ConditionalSelect::new(false, false, cond, 0b01, src2, src1, dst);

    buf.extend(inst.bytes());
}

#[inline(always)]
fn csneg_reg64_reg64_reg64_cond(
    buf: &mut Vec<'_, u8>,
    dst: AArch64GeneralReg,
    src1: AArch64GeneralReg,
    src2: AArch64GeneralReg,
    cond: ConditionCode,
) {
    let inst = ConditionalSelect::new(true, false, cond, 0b01, src2, src1, dst);

    buf.extend(inst.bytes());
}

#[inline(always)]
fn eor_reg64_reg64_reg64(
    buf: &mut Vec<'_, u8>,
    dst: AArch64GeneralReg,
    src1: AArch64GeneralReg,
    src2: AArch64GeneralReg,
) {
    let inst = LogicalShiftedRegister::new(LogicalOp::EOR, ShiftType::LSL, 0, src2, src1, dst);

    buf.extend(inst.bytes());
}

/// `LDR Xt, [Xn, #offset]` -> Load Xn + Offset Xt. ZRSP is SP.
/// Note: imm12 is the offest divided by 8.
#[inline(always)]
fn ldr_reg64_reg64_imm12(
    buf: &mut Vec<'_, u8>,
    dst: AArch64GeneralReg,
    base: AArch64GeneralReg,
    imm12: u16,
) {
    let inst = LoadStoreRegisterImmediate::new_load(0b11, imm12, base, dst);

    buf.extend(inst.bytes());
}

#[inline(always)]
fn lsl_reg64_reg64_reg64(
    buf: &mut Vec<'_, u8>,
    dst: AArch64GeneralReg,
    src1: AArch64GeneralReg,
    src2: AArch64GeneralReg,
) {
    let inst = DataProcessingTwoSource::new(0b001000, src2, src1, dst);

    buf.extend(inst.bytes());
}

#[inline(always)]
fn lsr_reg64_reg64_reg64(
    buf: &mut Vec<'_, u8>,
    dst: AArch64GeneralReg,
    src1: AArch64GeneralReg,
    src2: AArch64GeneralReg,
) {
    let inst = DataProcessingTwoSource::new(0b001001, src2, src1, dst);

    buf.extend(inst.bytes());
}

#[inline(always)]
fn asr_reg64_reg64_reg64(
    buf: &mut Vec<'_, u8>,
    dst: AArch64GeneralReg,
    src1: AArch64GeneralReg,
    src2: AArch64GeneralReg,
) {
    let inst = DataProcessingTwoSource::new(0b001010, src2, src1, dst);

    buf.extend(inst.bytes());
}

/// `MOV Xd, Xm` -> Move Xm to Xd.
#[inline(always)]
fn mov_reg64_reg64(buf: &mut Vec<'_, u8>, dst: AArch64GeneralReg, src: AArch64GeneralReg) {
    // MOV is equvalent to `ORR Xd, XZR, XM` in AARCH64.
    let inst = LogicalShiftedRegister::new(
        LogicalOp::ORR,
        ShiftType::LSL,
        0,
        src,
        AArch64GeneralReg::ZRSP,
        dst,
    );

    buf.extend(inst.bytes());
}

/// `MOVK Xd, imm16` -> Keeps Xd and moves an optionally shifted imm16 to Xd.
#[inline(always)]
fn movk_reg64_imm16(buf: &mut Vec<'_, u8>, dst: AArch64GeneralReg, imm16: u16, hw: u8) {
    let inst = MoveWideImmediate::new(0b11, dst, imm16, hw, true);

    buf.extend(inst.bytes());
}

/// `MOVZ Xd, imm16` -> Zeros Xd and moves an optionally shifted imm16 to Xd.
#[inline(always)]
fn movz_reg64_imm16(buf: &mut Vec<'_, u8>, dst: AArch64GeneralReg, imm16: u16, hw: u8) {
    let inst = MoveWideImmediate::new(0b10, dst, imm16, hw, true);

    buf.extend(inst.bytes());
}

#[inline(always)]
fn neg_reg64_reg64(buf: &mut Vec<'_, u8>, dst: AArch64GeneralReg, src: AArch64GeneralReg) {
    sub_reg64_reg64_reg64(buf, dst, AArch64GeneralReg::ZRSP, src);
}

#[inline(always)]
fn orr_reg64_reg64_reg64(
    buf: &mut Vec<'_, u8>,
    dst: AArch64GeneralReg,
    src1: AArch64GeneralReg,
    src2: AArch64GeneralReg,
) {
    let inst = LogicalShiftedRegister::new(LogicalOp::ORR, ShiftType::LSL, 0, src2, src1, dst);

    buf.extend(inst.bytes());
}

/// `STR Xt, [Xn, #offset]` -> Store Xt to Xn + Offset. ZRSP is SP.
/// Note: imm12 is the offest divided by 8.
#[inline(always)]
fn str_reg64_reg64_imm12(
    buf: &mut Vec<'_, u8>,
    src: AArch64GeneralReg,
    base: AArch64GeneralReg,
    imm12: u16,
) {
    let inst = LoadStoreRegisterImmediate::new_store(0b11, imm12, base, src);

    buf.extend(inst.bytes());
}

/// `SUB Xd, Xn, imm12` -> Subtract Xn and imm12 and place the result into Xd.
#[inline(always)]
fn sub_reg64_reg64_imm12(
    buf: &mut Vec<'_, u8>,
    dst: AArch64GeneralReg,
    src: AArch64GeneralReg,
    imm12: u16,
) {
    let inst = ArithmeticImmediate::new(true, false, dst, src, imm12, false);

    buf.extend(inst.bytes());
}

/// `SUB Xd, Xm, Xn` -> Subtract Xm and Xn and place the result into Xd.
#[inline(always)]
fn sub_reg64_reg64_reg64(
    buf: &mut Vec<'_, u8>,
    dst: AArch64GeneralReg,
    src1: AArch64GeneralReg,
    src2: AArch64GeneralReg,
) {
    let inst = ArithmeticShifted::new(true, false, ShiftType::LSL, 0, src2, src1, dst);

    buf.extend(inst.bytes());
}

/// `SUBS Xd, Xn, imm12` ->
#[inline(always)]
fn subs_reg64_reg64_imm12(
    buf: &mut Vec<'_, u8>,
    dst: AArch64GeneralReg,
    src: AArch64GeneralReg,
    imm12: u16,
) {
    let inst = ArithmeticImmediate::new(true, true, dst, src, imm12, false);

    buf.extend(inst.bytes());
}

#[inline(always)]
fn subs_reg64_reg64_reg64(
    buf: &mut Vec<'_, u8>,
    dst: AArch64GeneralReg,
    src1: AArch64GeneralReg,
    src2: AArch64GeneralReg,
) {
    let inst = ArithmeticShifted::new(true, true, ShiftType::LSL, 0, src2, src1, dst);

    buf.extend(inst.bytes());
}

/// `RET Xn` -> Return to the address stored in Xn.
#[inline(always)]
fn ret_reg64(buf: &mut Vec<'_, u8>, xn: AArch64GeneralReg) {
    let inst = UnconditionalBranchRegister::new(0b10, xn);

    buf.extend(inst.bytes());
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::disassembler_test;
    use capstone::prelude::*;

    enum ZRSPKind {
        UsesZR,
        UsesSP,
    }
    use ZRSPKind::*;

    impl AArch64GeneralReg {
        fn capstone_string(&self, zrsp_kind: ZRSPKind) -> String {
            match self {
                AArch64GeneralReg::XR => "x8".to_owned(),
                AArch64GeneralReg::IP0 => "x16".to_owned(),
                AArch64GeneralReg::IP1 => "x17".to_owned(),
                AArch64GeneralReg::PR => "x18".to_owned(),
                AArch64GeneralReg::FP => "x29".to_owned(),
                AArch64GeneralReg::LR => "x30".to_owned(),
                AArch64GeneralReg::ZRSP => match zrsp_kind {
                    UsesZR => "xzr".to_owned(),
                    UsesSP => "sp".to_owned(),
                },
                _ => format!("{}", self),
            }
        }
    }

    const TEST_U16: u16 = 0x1234;
    //const TEST_I32: i32 = 0x12345678;
    //const TEST_I64: i64 = 0x12345678_9ABCDEF0;

    const ALL_GENERAL_REGS: &[AArch64GeneralReg] = &[
        AArch64GeneralReg::X0,
        AArch64GeneralReg::X1,
        AArch64GeneralReg::X2,
        AArch64GeneralReg::X3,
        AArch64GeneralReg::X4,
        AArch64GeneralReg::X5,
        AArch64GeneralReg::X6,
        AArch64GeneralReg::X7,
        AArch64GeneralReg::XR,
        AArch64GeneralReg::X9,
        AArch64GeneralReg::X10,
        AArch64GeneralReg::X11,
        AArch64GeneralReg::X12,
        AArch64GeneralReg::X13,
        AArch64GeneralReg::X14,
        AArch64GeneralReg::X15,
        AArch64GeneralReg::IP0,
        AArch64GeneralReg::IP1,
        AArch64GeneralReg::PR,
        AArch64GeneralReg::X19,
        AArch64GeneralReg::X20,
        AArch64GeneralReg::X21,
        AArch64GeneralReg::X22,
        AArch64GeneralReg::X23,
        AArch64GeneralReg::X24,
        AArch64GeneralReg::X25,
        AArch64GeneralReg::X26,
        AArch64GeneralReg::X27,
        AArch64GeneralReg::X28,
        AArch64GeneralReg::FP,
        AArch64GeneralReg::LR,
        AArch64GeneralReg::ZRSP,
    ];

    fn setup_capstone_and_arena<T>(
        arena: &bumpalo::Bump,
    ) -> (bumpalo::collections::Vec<T>, Capstone) {
        let buf = bumpalo::vec![in arena];
        let cs = Capstone::new()
            .arm64()
            .mode(arch::arm64::ArchMode::Arm)
            .detail(true)
            .build()
            .expect("Failed to create Capstone object");
        (buf, cs)
    }

    #[test]
    fn test_add_reg64_reg64_reg64() {
        disassembler_test!(
            add_reg64_reg64_reg64,
            |reg1: AArch64GeneralReg, reg2: AArch64GeneralReg, reg3: AArch64GeneralReg| format!(
                "add {}, {}, {}",
                reg1.capstone_string(UsesZR),
                reg2.capstone_string(UsesZR),
                reg3.capstone_string(UsesZR)
            ),
            ALL_GENERAL_REGS,
            ALL_GENERAL_REGS,
            ALL_GENERAL_REGS
        );
    }

    #[test]
    fn test_add_reg64_reg64_imm12() {
        disassembler_test!(
            add_reg64_reg64_imm12,
            |reg1: AArch64GeneralReg, reg2: AArch64GeneralReg, imm| format!(
                "add {}, {}, #0x{:x}",
                reg1.capstone_string(UsesSP),
                reg2.capstone_string(UsesSP),
                imm
            ),
            ALL_GENERAL_REGS,
            ALL_GENERAL_REGS,
            [0x123]
        );
    }

    #[test]
    fn test_and_reg64_reg64_reg64() {
        disassembler_test!(
            and_reg64_reg64_reg64,
            |reg1: AArch64GeneralReg, reg2: AArch64GeneralReg, reg3: AArch64GeneralReg| format!(
                "and {}, {}, {}",
                reg1.capstone_string(UsesZR),
                reg2.capstone_string(UsesZR),
                reg3.capstone_string(UsesZR)
            ),
            ALL_GENERAL_REGS,
            ALL_GENERAL_REGS,
            ALL_GENERAL_REGS
        );
    }

    #[test]
    fn test_asr_reg64_reg64_reg64() {
        disassembler_test!(
            asr_reg64_reg64_reg64,
            |reg1: AArch64GeneralReg, reg2: AArch64GeneralReg, reg3: AArch64GeneralReg| format!(
                "asr {}, {}, {}",
                reg1.capstone_string(UsesZR),
                reg2.capstone_string(UsesZR),
                reg3.capstone_string(UsesZR)
            ),
            ALL_GENERAL_REGS,
            ALL_GENERAL_REGS,
            ALL_GENERAL_REGS
        );
    }

    #[test]
    fn test_b_cond() {
        disassembler_test!(
            b_cond,
            |cond: ConditionCode, imm: i32| format!("b.{} #0x{:x}", cond, imm as i64),
            ALL_CONDITIONS,
            [0x120, -0x120, (1 << 20) - 4, -(1 << 20)]
        );
    }

    #[test]
    fn test_b_imm26() {
        disassembler_test!(
            b_imm26,
            |imm| format!("b #0x{:x}", imm as i64),
            [0x120, -0x120, (1 << 27) - 4, -(1 << 27)]
        );
    }

    #[test]
    fn test_cmp_reg64_imm12() {
        disassembler_test!(
            cmp_reg64_imm12,
            |reg1: AArch64GeneralReg, imm| format!(
                "cmp {}, #0x{:x}",
                reg1.capstone_string(UsesSP),
                imm
            ),
            ALL_GENERAL_REGS,
            [0x123]
        );
    }

    #[test]
    fn test_cmp_reg64_reg64() {
        disassembler_test!(
            cmp_reg64_reg64,
            |reg1: AArch64GeneralReg, reg2: AArch64GeneralReg| format!(
                "cmp {}, {}",
                reg1.capstone_string(UsesZR),
                reg2.capstone_string(UsesZR)
            ),
            ALL_GENERAL_REGS,
            ALL_GENERAL_REGS
        );
    }

    #[test]
    fn test_cneg_reg64_reg64_cond() {
        disassembler_test!(
            cneg_reg64_reg64_cond,
            |reg1: AArch64GeneralReg, reg2: AArch64GeneralReg, cond: ConditionCode| {
                if cond == ConditionCode::AL {
                    format!(
                        "csneg {}, {}, {}, {}",
                        reg1.capstone_string(UsesZR),
                        reg2.capstone_string(UsesZR),
                        reg2.capstone_string(UsesZR),
                        cond.invert()
                    )
                } else {
                    format!(
                        "cneg {}, {}, {}",
                        reg1.capstone_string(UsesZR),
                        reg2.capstone_string(UsesZR),
                        cond
                    )
                }
            },
            ALL_GENERAL_REGS,
            ALL_GENERAL_REGS,
            ALL_CONDITIONS
        );
    }

    #[test]
    fn test_cset() {
        disassembler_test!(
            cset_reg64_cond,
            |reg1: AArch64GeneralReg, cond: ConditionCode| {
                if cond == ConditionCode::AL {
                    format!(
                        "csinc {}, xzr, xzr, {}",
                        reg1.capstone_string(UsesZR),
                        cond.invert()
                    )
                } else {
                    format!("cset {}, {}", reg1.capstone_string(UsesZR), cond)
                }
            },
            ALL_GENERAL_REGS,
            ALL_CONDITIONS
        );
    }

    #[test]
    fn test_csinc() {
        disassembler_test!(
            csinc_reg64_reg64_reg64_cond,
            |reg1: AArch64GeneralReg,
             reg2: AArch64GeneralReg,
             reg3: AArch64GeneralReg,
             cond: ConditionCode| {
                if reg3 != AArch64GeneralReg::ZRSP
                    && cond != ConditionCode::AL
                    && reg2 != AArch64GeneralReg::ZRSP
                    && reg2 == reg3
                {
                    format!(
                        "cinc {}, {}, {}",
                        reg1.capstone_string(UsesZR),
                        reg2.capstone_string(UsesZR),
                        cond.invert()
                    )
                } else if reg3 == AArch64GeneralReg::ZRSP
                    && cond != ConditionCode::AL
                    && reg2 == AArch64GeneralReg::ZRSP
                {
                    format!("cset {}, {}", reg1.capstone_string(UsesZR), cond.invert())
                } else {
                    format!(
                        "csinc {}, {}, {}, {}",
                        reg1.capstone_string(UsesZR),
                        reg2.capstone_string(UsesZR),
                        reg3.capstone_string(UsesZR),
                        cond
                    )
                }
            },
            ALL_GENERAL_REGS,
            ALL_GENERAL_REGS,
            ALL_GENERAL_REGS,
            ALL_CONDITIONS
        )
    }

    #[test]
    fn test_csneg() {
        disassembler_test!(
            csneg_reg64_reg64_reg64_cond,
            |reg1: AArch64GeneralReg,
             reg2: AArch64GeneralReg,
             reg3: AArch64GeneralReg,
             cond: ConditionCode| {
                if cond != ConditionCode::AL && reg2 == reg3 {
                    format!(
                        "cneg {}, {}, {}",
                        reg1.capstone_string(UsesZR),
                        reg2.capstone_string(UsesZR),
                        cond.invert()
                    )
                } else {
                    format!(
                        "csneg {}, {}, {}, {}",
                        reg1.capstone_string(UsesZR),
                        reg2.capstone_string(UsesZR),
                        reg3.capstone_string(UsesZR),
                        cond
                    )
                }
            },
            ALL_GENERAL_REGS,
            ALL_GENERAL_REGS,
            ALL_GENERAL_REGS,
            ALL_CONDITIONS
        )
    }

    #[test]
    fn test_eor_reg64_reg64_reg64() {
        disassembler_test!(
            eor_reg64_reg64_reg64,
            |reg1: AArch64GeneralReg, reg2: AArch64GeneralReg, reg3: AArch64GeneralReg| format!(
                "eor {}, {}, {}",
                reg1.capstone_string(UsesZR),
                reg2.capstone_string(UsesZR),
                reg3.capstone_string(UsesZR)
            ),
            ALL_GENERAL_REGS,
            ALL_GENERAL_REGS,
            ALL_GENERAL_REGS
        );
    }

    #[test]
    fn test_ldr_reg64_reg64_imm12() {
        disassembler_test!(
            ldr_reg64_reg64_imm12,
            |reg1: AArch64GeneralReg, reg2: AArch64GeneralReg, imm| format!(
                "ldr {}, [{}, #0x{:x}]",
                reg1.capstone_string(UsesZR),
                reg2.capstone_string(UsesSP),
                imm << 3
            ),
            ALL_GENERAL_REGS,
            ALL_GENERAL_REGS,
            [0x123]
        );
    }

    #[test]
    fn test_lsl_reg64_reg64_reg64() {
        disassembler_test!(
            lsl_reg64_reg64_reg64,
            |reg1: AArch64GeneralReg, reg2: AArch64GeneralReg, reg3: AArch64GeneralReg| format!(
                "lsl {}, {}, {}",
                reg1.capstone_string(UsesZR),
                reg2.capstone_string(UsesZR),
                reg3.capstone_string(UsesZR)
            ),
            ALL_GENERAL_REGS,
            ALL_GENERAL_REGS,
            ALL_GENERAL_REGS
        );
    }

    #[test]
    fn test_lsr_reg64_reg64_reg64() {
        disassembler_test!(
            lsr_reg64_reg64_reg64,
            |reg1: AArch64GeneralReg, reg2: AArch64GeneralReg, reg3: AArch64GeneralReg| format!(
                "lsr {}, {}, {}",
                reg1.capstone_string(UsesZR),
                reg2.capstone_string(UsesZR),
                reg3.capstone_string(UsesZR)
            ),
            ALL_GENERAL_REGS,
            ALL_GENERAL_REGS,
            ALL_GENERAL_REGS
        );
    }

    #[test]
    fn test_mov_reg64_reg64() {
        disassembler_test!(
            mov_reg64_reg64,
            |reg1: AArch64GeneralReg, reg2: AArch64GeneralReg| format!(
                "mov {}, {}",
                reg1.capstone_string(UsesZR),
                reg2.capstone_string(UsesZR),
            ),
            ALL_GENERAL_REGS,
            ALL_GENERAL_REGS
        );
    }

    #[test]
    fn test_movk_reg64_imm16() {
        disassembler_test!(
            movk_reg64_imm16,
            |reg1: AArch64GeneralReg, imm, hw| format!(
                "movk {}, #0x{:x}{}",
                reg1.capstone_string(UsesZR),
                imm,
                if hw > 0 {
                    format!(", lsl #{}", hw * 16)
                } else {
                    "".to_owned()
                }
            ),
            ALL_GENERAL_REGS,
            [TEST_U16],
            [0, 1, 2, 3]
        );
    }

    #[test]
    fn test_movz_reg64_imm16() {
        disassembler_test!(
            movz_reg64_imm16,
            |reg1: AArch64GeneralReg, imm, hw| format!(
                "mov {}, #0x{:x}{}",
                reg1.capstone_string(UsesZR),
                imm,
                "0000".repeat(hw as usize)
            ),
            ALL_GENERAL_REGS,
            [TEST_U16],
            [0, 1, 2, 3]
        );
    }

    #[test]
    fn test_neg_reg64_reg64() {
        disassembler_test!(
            neg_reg64_reg64,
            |reg1: AArch64GeneralReg, reg2: AArch64GeneralReg| format!(
                "neg {}, {}",
                reg1.capstone_string(UsesZR),
                reg2.capstone_string(UsesZR)
            ),
            ALL_GENERAL_REGS,
            ALL_GENERAL_REGS
        );
    }

    #[test]
    fn test_orr_reg64_reg64_reg64() {
        disassembler_test!(
            orr_reg64_reg64_reg64,
            |reg1: AArch64GeneralReg, reg2: AArch64GeneralReg, reg3: AArch64GeneralReg| {
                if reg2 == AArch64GeneralReg::ZRSP {
                    format!(
                        "mov {}, {}",
                        reg1.capstone_string(UsesZR),
                        reg3.capstone_string(UsesZR),
                    )
                } else {
                    format!(
                        "orr {}, {}, {}",
                        reg1.capstone_string(UsesZR),
                        reg2.capstone_string(UsesZR),
                        reg3.capstone_string(UsesZR),
                    )
                }
            },
            ALL_GENERAL_REGS,
            ALL_GENERAL_REGS,
            ALL_GENERAL_REGS
        );
    }

    #[test]
    fn test_str_reg64_reg64_imm12() {
        disassembler_test!(
            str_reg64_reg64_imm12,
            |reg1: AArch64GeneralReg, reg2: AArch64GeneralReg, imm| format!(
                "str {}, [{}, #0x{:x}]",
                reg1.capstone_string(UsesZR),
                reg2.capstone_string(UsesSP),
                imm << 3
            ),
            ALL_GENERAL_REGS,
            ALL_GENERAL_REGS,
            [0x123]
        );
    }

    #[test]
    fn test_sub_reg64_reg64_imm12() {
        disassembler_test!(
            sub_reg64_reg64_imm12,
            |reg1: AArch64GeneralReg, reg2: AArch64GeneralReg, imm| format!(
                "sub {}, {}, #0x{:x}",
                reg1.capstone_string(UsesSP),
                reg2.capstone_string(UsesSP),
                imm
            ),
            ALL_GENERAL_REGS,
            ALL_GENERAL_REGS,
            [0x123]
        );
    }

    #[test]
    fn test_sub_reg64_reg64_reg64() {
        disassembler_test!(
            sub_reg64_reg64_reg64,
            |reg1: AArch64GeneralReg, reg2: AArch64GeneralReg, reg3: AArch64GeneralReg| {
                if reg2 == AArch64GeneralReg::ZRSP {
                    // When the second register is ZR, it gets disassembled as neg,
                    // which is an alias for sub.
                    format!(
                        "neg {}, {}",
                        reg1.capstone_string(UsesZR),
                        reg3.capstone_string(UsesZR)
                    )
                } else {
                    format!(
                        "sub {}, {}, {}",
                        reg1.capstone_string(UsesZR),
                        reg2.capstone_string(UsesZR),
                        reg3.capstone_string(UsesZR)
                    )
                }
            },
            ALL_GENERAL_REGS,
            ALL_GENERAL_REGS,
            ALL_GENERAL_REGS
        );
    }

    #[test]

    fn test_subs_reg64_reg64_imm12() {
        disassembler_test!(
            subs_reg64_reg64_imm12,
            |reg1: AArch64GeneralReg, reg2: AArch64GeneralReg, imm| {
                if reg1 == AArch64GeneralReg::ZRSP {
                    // When the first register is SP, it gets disassembled as cmp,
                    // which is an alias for subs.
                    format!("cmp {}, #0x{:x}", reg2.capstone_string(UsesSP), imm)
                } else {
                    format!(
                        "subs {}, {}, #0x{:x}",
                        reg1.capstone_string(UsesZR),
                        reg2.capstone_string(UsesSP),
                        imm
                    )
                }
            },
            ALL_GENERAL_REGS,
            ALL_GENERAL_REGS,
            [0x123]
        );
    }

    #[test]
    fn test_subs_reg64_reg64_reg64() {
        disassembler_test!(
            subs_reg64_reg64_reg64,
            |reg1: AArch64GeneralReg, reg2: AArch64GeneralReg, reg3: AArch64GeneralReg| {
                if reg1 == AArch64GeneralReg::ZRSP {
                    // When the first register is SP, it gets disassembled as cmp,
                    // which is an alias for subs.
                    format!(
                        "cmp {}, {}",
                        reg2.capstone_string(UsesZR),
                        reg3.capstone_string(UsesZR)
                    )
                } else if reg2 == AArch64GeneralReg::ZRSP {
                    // When the second register is ZR, it gets disassembled as negs,
                    // which is an alias for subs.
                    format!(
                        "negs {}, {}",
                        reg1.capstone_string(UsesZR),
                        reg3.capstone_string(UsesZR)
                    )
                } else {
                    format!(
                        "subs {}, {}, {}",
                        reg1.capstone_string(UsesZR),
                        reg2.capstone_string(UsesZR),
                        reg3.capstone_string(UsesZR)
                    )
                }
            },
            ALL_GENERAL_REGS,
            ALL_GENERAL_REGS,
            ALL_GENERAL_REGS
        );
    }

    #[test]
    fn test_ret_reg64() {
        disassembler_test!(
            ret_reg64,
            |reg1: AArch64GeneralReg| if reg1 == AArch64GeneralReg::LR {
                "ret".to_owned()
            } else {
                format!("ret {}", reg1.capstone_string(UsesZR))
            },
            ALL_GENERAL_REGS
        );
    }
}
