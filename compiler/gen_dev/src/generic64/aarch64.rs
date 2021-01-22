use crate::generic64::{Assembler, CallConv, RegTrait};
use crate::Relocation;
use bumpalo::collections::Vec;

#[derive(Copy, Clone, PartialEq, Eq, Hash, PartialOrd, Ord, Debug)]
#[allow(dead_code)]
pub enum AArch64GPReg {
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
impl RegTrait for AArch64GPReg {}

#[derive(Copy, Clone, PartialEq, Eq, Hash, PartialOrd, Ord, Debug)]
#[allow(dead_code)]
pub enum AArch64FPReg {}
impl RegTrait for AArch64FPReg {}

pub struct AArch64Assembler {}

// AArch64Call may need to eventually be split by OS,
// but I think with how we use it, they may all be the same.
pub struct AArch64Call {}

const STACK_ALIGNMENT: u8 = 16;

impl CallConv<AArch64GPReg, AArch64FPReg> for AArch64Call {
    const GP_PARAM_REGS: &'static [AArch64GPReg] = &[
        AArch64GPReg::X0,
        AArch64GPReg::X1,
        AArch64GPReg::X2,
        AArch64GPReg::X3,
        AArch64GPReg::X4,
        AArch64GPReg::X5,
        AArch64GPReg::X6,
        AArch64GPReg::X7,
    ];
    const GP_RETURN_REGS: &'static [AArch64GPReg] = Self::GP_PARAM_REGS;
    const GP_DEFAULT_FREE_REGS: &'static [AArch64GPReg] = &[
        // The regs we want to use first should be at the end of this vec.
        // We will use pop to get which reg to use next

        // Don't use frame pointer: AArch64GPReg::FP,
        // Don't user indirect result location: AArch64GPReg::XR,
        // Don't use platform register: AArch64GPReg::PR,
        // Don't use link register: AArch64GPReg::LR,
        // Don't use zero register/stack pointer: AArch64GPReg::ZRSP,

        // Use callee saved regs last.
        AArch64GPReg::X19,
        AArch64GPReg::X20,
        AArch64GPReg::X21,
        AArch64GPReg::X22,
        AArch64GPReg::X23,
        AArch64GPReg::X24,
        AArch64GPReg::X25,
        AArch64GPReg::X26,
        AArch64GPReg::X27,
        AArch64GPReg::X28,
        // Use caller saved regs first.
        AArch64GPReg::X0,
        AArch64GPReg::X1,
        AArch64GPReg::X2,
        AArch64GPReg::X3,
        AArch64GPReg::X4,
        AArch64GPReg::X5,
        AArch64GPReg::X6,
        AArch64GPReg::X7,
        AArch64GPReg::X9,
        AArch64GPReg::X10,
        AArch64GPReg::X11,
        AArch64GPReg::X12,
        AArch64GPReg::X13,
        AArch64GPReg::X14,
        AArch64GPReg::X15,
        AArch64GPReg::IP0,
        AArch64GPReg::IP1,
    ];
    const FP_PARAM_REGS: &'static [AArch64FPReg] = &[];
    const FP_RETURN_REGS: &'static [AArch64FPReg] = Self::FP_PARAM_REGS;
    const FP_DEFAULT_FREE_REGS: &'static [AArch64FPReg] = &[];

    const SHADOW_SPACE_SIZE: u8 = 0;

    #[inline(always)]
    fn gp_callee_saved(reg: &AArch64GPReg) -> bool {
        matches!(
            reg,
            AArch64GPReg::X19
                | AArch64GPReg::X20
                | AArch64GPReg::X21
                | AArch64GPReg::X22
                | AArch64GPReg::X23
                | AArch64GPReg::X24
                | AArch64GPReg::X25
                | AArch64GPReg::X26
                | AArch64GPReg::X27
                | AArch64GPReg::X28
        )
    }
    #[inline(always)]
    fn fp_callee_saved(_reg: &AArch64FPReg) -> bool {
        unimplemented!("AArch64 FPRegs not implemented yet");
    }

    #[inline(always)]
    fn setup_stack(
        buf: &mut Vec<'_, u8>,
        leaf_function: bool,
        saved_regs: &[AArch64GPReg],
        requested_stack_size: i32,
    ) -> Result<i32, String> {
        // full size is upcast to i64 to make sure we don't overflow here.
        let mut full_size = 8 * saved_regs.len() as i64 + requested_stack_size as i64;
        if !leaf_function {
            full_size += 8;
        }
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
        if let Some(aligned_stack_size) =
            requested_stack_size.checked_add(8 * saved_regs.len() as i32 + offset as i32)
        {
            if aligned_stack_size > 0 {
                AArch64Assembler::sub_reg64_reg64_imm32(
                    buf,
                    AArch64GPReg::ZRSP,
                    AArch64GPReg::ZRSP,
                    aligned_stack_size,
                );

                // All the following stores could be optimized by using `STP` to store pairs.
                let mut offset = aligned_stack_size;
                if !leaf_function {
                    offset -= 8;
                    AArch64Assembler::mov_stack32_reg64(buf, offset, AArch64GPReg::LR);
                    offset -= 8;
                    AArch64Assembler::mov_stack32_reg64(buf, offset, AArch64GPReg::FP);
                }
                for reg in saved_regs {
                    offset -= 8;
                    AArch64Assembler::mov_stack32_reg64(buf, offset, *reg);
                }
                Ok(aligned_stack_size)
            } else {
                Ok(0)
            }
        } else {
            Err("Ran out of stack space".to_string())
        }
    }

    #[inline(always)]
    fn cleanup_stack(
        buf: &mut Vec<'_, u8>,
        leaf_function: bool,
        saved_regs: &[AArch64GPReg],
        aligned_stack_size: i32,
    ) -> Result<(), String> {
        if aligned_stack_size > 0 {
            // All the following stores could be optimized by using `STP` to store pairs.
            let mut offset = aligned_stack_size;
            if !leaf_function {
                offset -= 8;
                AArch64Assembler::mov_reg64_stack32(buf, AArch64GPReg::LR, offset);
                offset -= 8;
                AArch64Assembler::mov_reg64_stack32(buf, AArch64GPReg::FP, offset);
            }
            for reg in saved_regs {
                offset -= 8;
                AArch64Assembler::mov_reg64_stack32(buf, *reg, offset);
            }
            AArch64Assembler::add_reg64_reg64_imm32(
                buf,
                AArch64GPReg::ZRSP,
                AArch64GPReg::ZRSP,
                aligned_stack_size,
            );
        }
        Ok(())
    }
}

impl Assembler<AArch64GPReg, AArch64FPReg> for AArch64Assembler {
    #[inline(always)]
    fn abs_reg64_reg64(_buf: &mut Vec<'_, u8>, _dst: AArch64GPReg, _src: AArch64GPReg) {
        unimplemented!("abs_reg64_reg64 is not yet implement for AArch64");
    }

    #[inline(always)]
    fn add_reg64_reg64_imm32(
        buf: &mut Vec<'_, u8>,
        dst: AArch64GPReg,
        src: AArch64GPReg,
        imm32: i32,
    ) {
        if imm32 < 0 {
            unimplemented!("immediate addition with values less than 0 are not yet implemented");
        } else if imm32 < 0xFFF {
            add_reg64_reg64_imm12(buf, dst, src, imm32 as u16);
        } else {
            unimplemented!(
                "immediate additions with values greater than 12bits are not yet implemented"
            );
        }
    }

    #[inline(always)]
    fn add_reg64_reg64_reg64(
        buf: &mut Vec<'_, u8>,
        dst: AArch64GPReg,
        src1: AArch64GPReg,
        src2: AArch64GPReg,
    ) {
        add_reg64_reg64_reg64(buf, dst, src1, src2);
    }

    #[inline(always)]
    fn add_freg64_freg64_freg64(
        _buf: &mut Vec<'_, u8>,
        _dst: AArch64FPReg,
        _src1: AArch64FPReg,
        _src2: AArch64FPReg,
    ) {
        unimplemented!("adding floats not yet implemented for AArch64");
    }

    #[inline(always)]
    fn mov_freg64_imm64(
        _buf: &mut Vec<'_, u8>,
        _relocs: &mut Vec<'_, Relocation>,
        _dst: AArch64FPReg,
        _imm: f64,
    ) {
        unimplemented!("loading float literal not yet implemented for AArch64");
    }

    #[inline(always)]
    fn mov_reg64_imm64(buf: &mut Vec<'_, u8>, dst: AArch64GPReg, imm: i64) {
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
    fn mov_freg64_freg64(_buf: &mut Vec<'_, u8>, _dst: AArch64FPReg, _src: AArch64FPReg) {
        unimplemented!("moving data between float registers not yet implemented for AArch64");
    }

    #[inline(always)]
    fn mov_reg64_reg64(buf: &mut Vec<'_, u8>, dst: AArch64GPReg, src: AArch64GPReg) {
        mov_reg64_reg64(buf, dst, src);
    }

    #[inline(always)]
    fn mov_reg64_stack32(buf: &mut Vec<'_, u8>, dst: AArch64GPReg, offset: i32) {
        if offset < 0 {
            unimplemented!("negative stack offsets are not yet implement for AArch64");
        } else if offset < (0xFFF << 8) {
            debug_assert!(offset % 8 == 0);
            ldr_reg64_imm12(buf, dst, AArch64GPReg::ZRSP, (offset as u16) >> 3);
        } else {
            unimplemented!("stack offsets over 32k are not yet implement for AArch64");
        }
    }

    fn mov_freg64_stack32(_buf: &mut Vec<'_, u8>, _dst: AArch64FPReg, _offset: i32) {
        unimplemented!("loading floating point reg from stack not yet implemented for AArch64");
    }

    #[inline(always)]
    fn mov_stack32_freg64(_buf: &mut Vec<'_, u8>, _offset: i32, _src: AArch64FPReg) {
        unimplemented!("saving floating point reg to stack not yet implemented for AArch64");
    }

    #[inline(always)]
    fn mov_stack32_reg64(buf: &mut Vec<'_, u8>, offset: i32, src: AArch64GPReg) {
        if offset < 0 {
            unimplemented!("negative stack offsets are not yet implement for AArch64");
        } else if offset < (0xFFF << 8) {
            debug_assert!(offset % 8 == 0);
            str_reg64_imm12(buf, src, AArch64GPReg::ZRSP, (offset as u16) >> 3);
        } else {
            unimplemented!("stack offsets over 32k are not yet implement for AArch64");
        }
    }

    #[inline(always)]
    fn sub_reg64_reg64_imm32(
        buf: &mut Vec<'_, u8>,
        dst: AArch64GPReg,
        src: AArch64GPReg,
        imm32: i32,
    ) {
        if imm32 < 0 {
            unimplemented!(
                "immediate subtractions with values less than 0 are not yet implemented"
            );
        } else if imm32 < 0xFFF {
            sub_reg64_reg64_imm12(buf, dst, src, imm32 as u16);
        } else {
            unimplemented!(
                "immediate subtractions with values greater than 12bits are not yet implemented"
            );
        }
    }

    #[inline(always)]
    fn sub_reg64_reg64_reg64(
        _buf: &mut Vec<'_, u8>,
        _dst: AArch64GPReg,
        _src1: AArch64GPReg,
        _src2: AArch64GPReg,
    ) {
        unimplemented!("registers subtractions not implemented yet for AArch64");
    }

    #[inline(always)]
    fn eq_reg64_reg64_reg64(
        _buf: &mut Vec<'_, u8>,
        _dst: AArch64GPReg,
        _src1: AArch64GPReg,
        _src2: AArch64GPReg,
    ) {
        unimplemented!("registers equality not implemented yet for AArch64");
    }
    #[inline(always)]
    fn ret(buf: &mut Vec<'_, u8>) {
        ret_reg64(buf, AArch64GPReg::LR)
    }
}

impl AArch64Assembler {}

/// AArch64Instruction, maps all instructions to an enum.
/// Decoding the function should be cheap because we will always inline.
/// All of the operations should resolved by constants, leave just some bit manipulation.
/// Enums may not be complete since we will only add what we need.
#[derive(Debug)]
enum AArch64Instruction {
    _Reserved,
    _SVE,
    DPImm(DPImmGroup),
    Branch(BranchGroup),
    LdStr(LdStrGroup),
    DPReg(DPRegGroup),
    _DPFloat,
}

#[derive(Debug)]
enum BranchGroup {
    UnconditionBranchReg {
        opc: u8,
        op2: u8,
        op3: u8,
        reg_n: AArch64GPReg,
        op4: u8,
    },
}

#[derive(Debug)]
enum DPRegGroup {
    AddSubShifted {
        sf: bool,
        subtract: bool,
        set_flags: bool,
        shift: u8,
        reg_m: AArch64GPReg,
        imm6: u8,
        reg_n: AArch64GPReg,
        reg_d: AArch64GPReg,
    },
    Logical {
        sf: bool,
        op: DPRegLogicalOp,
        shift: u8,
        reg_m: AArch64GPReg,
        imm6: u8,
        reg_n: AArch64GPReg,
        reg_d: AArch64GPReg,
    },
}

#[derive(Debug)]
enum DPImmGroup {
    AddSubImm {
        sf: bool,
        subtract: bool,
        set_flags: bool,
        shift: bool,
        imm12: u16,
        reg_n: AArch64GPReg,
        reg_d: AArch64GPReg,
    },
    MoveWide {
        sf: bool,
        opc: u8,
        hw: u8,
        imm16: u16,
        reg_d: AArch64GPReg,
    },
}

#[derive(Debug)]
enum LdStrGroup {
    UnsignedImm {
        size: u8,
        v: bool,
        opc: u8,
        imm12: u16,
        reg_n: AArch64GPReg,
        reg_t: AArch64GPReg,
    },
}

#[derive(Debug)]
#[allow(dead_code)]
enum DPRegLogicalOp {
    AND,
    BIC,
    ORR,
    ORN,
    EOR,
    EON,
    ANDS,
    BICS,
}

#[inline(always)]
fn build_instruction(inst: AArch64Instruction) -> [u8; 4] {
    let mut out: u32 = 0;
    match inst {
        AArch64Instruction::Branch(branch) => {
            out |= 0b101 << 26;
            match branch {
                BranchGroup::UnconditionBranchReg {
                    opc,
                    op2,
                    op3,
                    reg_n,
                    op4,
                } => {
                    debug_assert!(opc <= 0b1111);
                    debug_assert!(op2 <= 0b11111);
                    debug_assert!(op3 <= 0b111111);
                    debug_assert!(op4 <= 0b1111);
                    out |= 0b1101011 << 25;
                    out |= (opc as u32) << 21;
                    out |= (op2 as u32) << 16;
                    out |= (op3 as u32) << 10;
                    out |= (reg_n as u32) << 5;
                    out |= op4 as u32;
                }
            }
        }
        AArch64Instruction::DPImm(dpimm) => {
            out |= 0b100 << 26;
            match dpimm {
                DPImmGroup::MoveWide {
                    sf,
                    opc,
                    hw,
                    imm16,
                    reg_d,
                } => {
                    out |= (sf as u32) << 31;
                    out |= (opc as u32) << 29;
                    out |= 0b101 << 23;
                    out |= (hw as u32) << 21;
                    out |= (imm16 as u32) << 5;
                    out |= reg_d as u32;
                }
                DPImmGroup::AddSubImm {
                    sf,
                    subtract,
                    set_flags,
                    shift,
                    imm12,
                    reg_n,
                    reg_d,
                } => {
                    debug_assert!(imm12 <= 0xFFF);
                    out |= (sf as u32) << 31;
                    out |= (subtract as u32) << 30;
                    out |= (set_flags as u32) << 29;
                    out |= 0b010 << 23;
                    out |= (shift as u32) << 22;
                    out |= (imm12 as u32) << 10;
                    out |= (reg_n as u32) << 5;
                    out |= reg_d as u32;
                }
            }
        }
        AArch64Instruction::DPReg(dpreg) => {
            out |= 0b101 << 25;
            match dpreg {
                DPRegGroup::Logical {
                    sf,
                    op,
                    shift,
                    reg_m,
                    imm6,
                    reg_n,
                    reg_d,
                } => {
                    debug_assert!(shift <= 0b11);
                    debug_assert!(imm6 <= 0b111111);
                    let (opc, n) = match op {
                        DPRegLogicalOp::AND => (0b00, 0),
                        DPRegLogicalOp::BIC => (0b00, 1),
                        DPRegLogicalOp::ORR => (0b01, 0),
                        DPRegLogicalOp::ORN => (0b01, 1),
                        DPRegLogicalOp::EOR => (0b10, 0),
                        DPRegLogicalOp::EON => (0b10, 1),
                        DPRegLogicalOp::ANDS => (0b11, 0),
                        DPRegLogicalOp::BICS => (0b11, 1),
                    };
                    out |= (sf as u32) << 31;
                    out |= opc << 29;
                    out |= (shift as u32) << 22;
                    out |= n << 21;
                    out |= (reg_m as u32) << 16;
                    out |= (imm6 as u32) << 10;
                    out |= (reg_n as u32) << 5;
                    out |= reg_d as u32;
                }
                DPRegGroup::AddSubShifted {
                    sf,
                    subtract,
                    set_flags,
                    shift,
                    reg_m,
                    imm6,
                    reg_n,
                    reg_d,
                } => {
                    debug_assert!(shift <= 0b11);
                    debug_assert!(imm6 <= 0b111111);
                    out |= (sf as u32) << 31;
                    out |= (subtract as u32) << 30;
                    out |= (set_flags as u32) << 29;
                    out |= 0b1 << 24;
                    out |= (shift as u32) << 22;
                    out |= (reg_m as u32) << 16;
                    out |= (imm6 as u32) << 10;
                    out |= (reg_n as u32) << 5;
                    out |= reg_d as u32;
                }
            }
        }
        AArch64Instruction::LdStr(ldstr) => {
            out |= 0b1 << 27;
            match ldstr {
                LdStrGroup::UnsignedImm {
                    size,
                    v,
                    opc,
                    imm12,
                    reg_n,
                    reg_t,
                } => {
                    debug_assert!(size <= 0b11);
                    debug_assert!(imm12 <= 0xFFF);
                    out |= (size as u32) << 30;
                    out |= 0b11 << 28;
                    out |= (v as u32) << 26;
                    out |= 0b1 << 24;
                    out |= (opc as u32) << 22;
                    out |= (imm12 as u32) << 10;
                    out |= (reg_n as u32) << 5;
                    out |= reg_t as u32;
                }
            }
        }
        x => unimplemented!("The instruction, {:?}, has not be implemented yet", x),
    }
    out.to_le_bytes()
}

// Below here are the functions for all of the assembly instructions.
// Their names are based on the instruction and operators combined.
// You should call `buf.reserve()` if you push or extend more than once.
// Unit tests are added at the bottom of the file to ensure correct asm generation.
// Please keep these in alphanumeric order.

/// `ADD Xd, Xn, imm12` -> Add Xn and imm12 and place the result into Xd.
#[inline(always)]
fn add_reg64_reg64_imm12(buf: &mut Vec<'_, u8>, dst: AArch64GPReg, src: AArch64GPReg, imm12: u16) {
    buf.extend(&build_instruction(AArch64Instruction::DPImm(
        DPImmGroup::AddSubImm {
            sf: true,
            subtract: false,
            set_flags: false,
            shift: false,
            imm12,
            reg_n: src,
            reg_d: dst,
        },
    )));
}

/// `ADD Xd, Xm, Xn` -> Add Xm and Xn and place the result into Xd.
#[inline(always)]
fn add_reg64_reg64_reg64(
    buf: &mut Vec<'_, u8>,
    dst: AArch64GPReg,
    src1: AArch64GPReg,
    src2: AArch64GPReg,
) {
    buf.extend(&build_instruction(AArch64Instruction::DPReg(
        DPRegGroup::AddSubShifted {
            sf: true,
            subtract: false,
            set_flags: false,
            shift: 0,
            reg_m: src1,
            imm6: 0,
            reg_n: src2,
            reg_d: dst,
        },
    )));
}

/// `LDR Xt, [Xn, #offset]` -> Load Xn + Offset Xt. ZRSP is SP.
/// Note: imm12 is the offest divided by 8.
#[inline(always)]
fn ldr_reg64_imm12(buf: &mut Vec<'_, u8>, dst: AArch64GPReg, base: AArch64GPReg, imm12: u16) {
    debug_assert!(imm12 <= 0xFFF);
    buf.extend(&build_instruction(AArch64Instruction::LdStr(
        LdStrGroup::UnsignedImm {
            size: 0b11,
            v: false,
            opc: 0b01,
            imm12,
            reg_n: base,
            reg_t: dst,
        },
    )));
}

/// `MOV Xd, Xm` -> Move Xm to Xd.
#[inline(always)]
fn mov_reg64_reg64(buf: &mut Vec<'_, u8>, dst: AArch64GPReg, src: AArch64GPReg) {
    // MOV is equvalent to `ORR Xd, XZR, XM` in AARCH64.
    buf.extend(&build_instruction(AArch64Instruction::DPReg(
        DPRegGroup::Logical {
            sf: true,
            op: DPRegLogicalOp::ORR,
            shift: 0,
            reg_m: src,
            imm6: 0,
            reg_n: AArch64GPReg::ZRSP,
            reg_d: dst,
        },
    )));
}

/// `MOVK Xd, imm16` -> Keeps Xd and moves an optionally shifted imm16 to Xd.
#[inline(always)]
fn movk_reg64_imm16(buf: &mut Vec<'_, u8>, dst: AArch64GPReg, imm16: u16, hw: u8) {
    debug_assert!(hw <= 0b11);
    // MOV is equvalent to `ORR Xd, XZR, XM` in AARCH64.
    buf.extend(&build_instruction(AArch64Instruction::DPImm(
        DPImmGroup::MoveWide {
            sf: true,
            opc: 0b11,
            hw,
            imm16,
            reg_d: dst,
        },
    )));
}

/// `MOVZ Xd, imm16` -> Zeros Xd and moves an optionally shifted imm16 to Xd.
#[inline(always)]
fn movz_reg64_imm16(buf: &mut Vec<'_, u8>, dst: AArch64GPReg, imm16: u16, hw: u8) {
    debug_assert!(hw <= 0b11);
    // MOV is equvalent to `ORR Xd, XZR, XM` in AARCH64.
    buf.extend(&build_instruction(AArch64Instruction::DPImm(
        DPImmGroup::MoveWide {
            sf: true,
            opc: 0b10,
            hw,
            imm16,
            reg_d: dst,
        },
    )));
}

/// `STR Xt, [Xn, #offset]` -> Store Xt to Xn + Offset. ZRSP is SP.
/// Note: imm12 is the offest divided by 8.
#[inline(always)]
fn str_reg64_imm12(buf: &mut Vec<'_, u8>, src: AArch64GPReg, base: AArch64GPReg, imm12: u16) {
    debug_assert!(imm12 <= 0xFFF);
    buf.extend(&build_instruction(AArch64Instruction::LdStr(
        LdStrGroup::UnsignedImm {
            size: 0b11,
            v: false,
            opc: 0b00,
            imm12,
            reg_n: base,
            reg_t: src,
        },
    )));
}

/// `SUB Xd, Xn, imm12` -> Subtract Xn and imm12 and place the result into Xd.
#[inline(always)]
fn sub_reg64_reg64_imm12(buf: &mut Vec<'_, u8>, dst: AArch64GPReg, src: AArch64GPReg, imm12: u16) {
    buf.extend(&build_instruction(AArch64Instruction::DPImm(
        DPImmGroup::AddSubImm {
            sf: true,
            subtract: true,
            set_flags: false,
            shift: false,
            imm12,
            reg_n: src,
            reg_d: dst,
        },
    )));
}

/// `RET Xn` -> Return to the address stored in Xn.
#[inline(always)]
fn ret_reg64(buf: &mut Vec<'_, u8>, xn: AArch64GPReg) {
    buf.extend(&build_instruction(AArch64Instruction::Branch(
        BranchGroup::UnconditionBranchReg {
            opc: 0b0010,
            op2: 0b11111,
            op3: 0b000000,
            reg_n: xn,
            op4: 0b000,
        },
    )));
}

#[cfg(test)]
mod tests {
    use super::*;

    const TEST_U16: u16 = 0x1234;
    //const TEST_I32: i32 = 0x12345678;
    //const TEST_I64: i64 = 0x12345678_9ABCDEF0;

    #[test]
    fn test_add_reg64_reg64_reg64() {
        let arena = bumpalo::Bump::new();
        let mut buf = bumpalo::vec![in &arena];
        add_reg64_reg64_reg64(
            &mut buf,
            AArch64GPReg::X10,
            AArch64GPReg::ZRSP,
            AArch64GPReg::X21,
        );
        assert_eq!(&buf, &[0xAA, 0x02, 0x1F, 0x8B]);
    }

    #[test]
    fn test_add_reg64_reg64_imm12() {
        let arena = bumpalo::Bump::new();
        let mut buf = bumpalo::vec![in &arena];
        add_reg64_reg64_imm12(&mut buf, AArch64GPReg::X10, AArch64GPReg::X21, 0x123);
        assert_eq!(&buf, &[0xAA, 0x8E, 0x04, 0x91]);
    }

    #[test]
    fn test_ldr_reg64_imm12() {
        let arena = bumpalo::Bump::new();
        let mut buf = bumpalo::vec![in &arena];
        ldr_reg64_imm12(&mut buf, AArch64GPReg::X21, AArch64GPReg::ZRSP, 0x123);
        assert_eq!(&buf, &[0xF5, 0x8F, 0x44, 0xF9]);
    }

    #[test]
    fn test_mov_reg64_reg64() {
        let arena = bumpalo::Bump::new();
        let mut buf = bumpalo::vec![in &arena];
        mov_reg64_reg64(&mut buf, AArch64GPReg::X10, AArch64GPReg::X21);
        assert_eq!(&buf, &[0xEA, 0x03, 0x15, 0xAA]);
    }

    #[test]
    fn test_movk_reg64_imm16() {
        let arena = bumpalo::Bump::new();
        let mut buf = bumpalo::vec![in &arena];
        movk_reg64_imm16(&mut buf, AArch64GPReg::X21, TEST_U16, 3);
        assert_eq!(&buf, &[0x95, 0x46, 0xE2, 0xF2]);
    }

    #[test]
    fn test_movz_reg64_imm16() {
        let arena = bumpalo::Bump::new();
        let mut buf = bumpalo::vec![in &arena];
        movz_reg64_imm16(&mut buf, AArch64GPReg::X21, TEST_U16, 3);
        assert_eq!(&buf, &[0x95, 0x46, 0xE2, 0xD2]);
    }

    #[test]
    fn test_str_reg64_imm12() {
        let arena = bumpalo::Bump::new();
        let mut buf = bumpalo::vec![in &arena];
        str_reg64_imm12(&mut buf, AArch64GPReg::X21, AArch64GPReg::ZRSP, 0x123);
        assert_eq!(&buf, &[0xF5, 0x8F, 0x04, 0xF9]);
    }

    #[test]
    fn test_sub_reg64_reg64_imm12() {
        let arena = bumpalo::Bump::new();
        let mut buf = bumpalo::vec![in &arena];
        sub_reg64_reg64_imm12(&mut buf, AArch64GPReg::X10, AArch64GPReg::X21, 0x123);
        assert_eq!(&buf, &[0xAA, 0x8E, 0x04, 0xD1]);
    }

    #[test]
    fn test_ret_reg64() {
        let arena = bumpalo::Bump::new();
        let mut buf = bumpalo::vec![in &arena];
        ret_reg64(&mut buf, AArch64GPReg::LR);
        assert_eq!(&buf, &[0xC0, 0x03, 0x5F, 0xD6]);
    }
}
