use crate::generic64::{storage::StorageManager, Assembler, CallConv, RegTrait};
use crate::{
    single_register_floats, single_register_int_builtins, single_register_integers,
    single_register_layouts, Relocation,
};
use bumpalo::collections::Vec;
use roc_builtins::bitcode::{FloatWidth, IntWidth};
use roc_error_macros::internal_error;
use roc_module::symbol::Symbol;
use roc_mono::layout::{Builtin, Layout, STLayoutInterner};
use roc_target::TargetInfo;

const TARGET_INFO: TargetInfo = TargetInfo::default_x86_64();

// Not sure exactly how I want to represent registers.
// If we want max speed, we would likely make them structs that impl the same trait to avoid ifs.
#[derive(Copy, Clone, PartialEq, Eq, Hash, PartialOrd, Ord, Debug)]
pub enum X86_64GeneralReg {
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
impl RegTrait for X86_64GeneralReg {
    fn value(&self) -> u8 {
        *self as u8
    }
}
impl std::fmt::Display for X86_64GeneralReg {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                X86_64GeneralReg::RAX => "rax",
                X86_64GeneralReg::RBX => "rbx",
                X86_64GeneralReg::RCX => "rcx",
                X86_64GeneralReg::RDX => "rdx",
                X86_64GeneralReg::RBP => "rbp",
                X86_64GeneralReg::RSP => "rsp",
                X86_64GeneralReg::RDI => "rdi",
                X86_64GeneralReg::RSI => "rsi",
                X86_64GeneralReg::R8 => "r8",
                X86_64GeneralReg::R9 => "r9",
                X86_64GeneralReg::R10 => "r10",
                X86_64GeneralReg::R11 => "r11",
                X86_64GeneralReg::R12 => "r12",
                X86_64GeneralReg::R13 => "r13",
                X86_64GeneralReg::R14 => "r14",
                X86_64GeneralReg::R15 => "r15",
            }
        )
    }
}

#[derive(Copy, Clone, PartialEq, Eq, Hash, PartialOrd, Ord, Debug)]
pub enum X86_64FloatReg {
    XMM0 = 0,
    XMM1 = 1,
    XMM2 = 2,
    XMM3 = 3,
    XMM4 = 4,
    XMM5 = 5,
    XMM6 = 6,
    XMM7 = 7,
    XMM8 = 8,
    XMM9 = 9,
    XMM10 = 10,
    XMM11 = 11,
    XMM12 = 12,
    XMM13 = 13,
    XMM14 = 14,
    XMM15 = 15,
}
impl RegTrait for X86_64FloatReg {
    fn value(&self) -> u8 {
        *self as u8
    }
}
impl std::fmt::Display for X86_64FloatReg {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                X86_64FloatReg::XMM0 => "xmm0",
                X86_64FloatReg::XMM1 => "xmm1",
                X86_64FloatReg::XMM2 => "xmm2",
                X86_64FloatReg::XMM3 => "xmm3",
                X86_64FloatReg::XMM4 => "xmm4",
                X86_64FloatReg::XMM5 => "xmm5",
                X86_64FloatReg::XMM6 => "xmm6",
                X86_64FloatReg::XMM7 => "xmm7",
                X86_64FloatReg::XMM8 => "xmm8",
                X86_64FloatReg::XMM9 => "xmm9",
                X86_64FloatReg::XMM10 => "xmm10",
                X86_64FloatReg::XMM11 => "xmm11",
                X86_64FloatReg::XMM12 => "xmm12",
                X86_64FloatReg::XMM13 => "xmm13",
                X86_64FloatReg::XMM14 => "xmm14",
                X86_64FloatReg::XMM15 => "xmm15",
            }
        )
    }
}

#[derive(Copy, Clone)]
pub struct X86_64Assembler {}
#[derive(Copy, Clone)]
pub struct X86_64WindowsFastcall {}
#[derive(Copy, Clone)]
pub struct X86_64SystemV {}

const STACK_ALIGNMENT: u8 = 16;

impl CallConv<X86_64GeneralReg, X86_64FloatReg, X86_64Assembler> for X86_64SystemV {
    const BASE_PTR_REG: X86_64GeneralReg = X86_64GeneralReg::RBP;
    const STACK_PTR_REG: X86_64GeneralReg = X86_64GeneralReg::RSP;

    const GENERAL_PARAM_REGS: &'static [X86_64GeneralReg] = &[
        X86_64GeneralReg::RDI,
        X86_64GeneralReg::RSI,
        X86_64GeneralReg::RDX,
        X86_64GeneralReg::RCX,
        X86_64GeneralReg::R8,
        X86_64GeneralReg::R9,
    ];
    const GENERAL_RETURN_REGS: &'static [X86_64GeneralReg] =
        &[X86_64GeneralReg::RAX, X86_64GeneralReg::RDX];
    const GENERAL_DEFAULT_FREE_REGS: &'static [X86_64GeneralReg] = &[
        // The regs we want to use first should be at the end of this vec.
        // We will use pop to get which reg to use next
        // Use callee saved regs last.
        X86_64GeneralReg::RBX,
        // Don't use frame pointer: X86_64GeneralReg::RBP,
        X86_64GeneralReg::R12,
        X86_64GeneralReg::R13,
        X86_64GeneralReg::R14,
        X86_64GeneralReg::R15,
        // Use caller saved regs first.
        X86_64GeneralReg::RAX,
        X86_64GeneralReg::RCX,
        X86_64GeneralReg::RDX,
        // Don't use stack pointer: X86_64GeneralReg::RSP,
        X86_64GeneralReg::RSI,
        X86_64GeneralReg::RDI,
        X86_64GeneralReg::R8,
        X86_64GeneralReg::R9,
        X86_64GeneralReg::R10,
        X86_64GeneralReg::R11,
    ];

    const FLOAT_PARAM_REGS: &'static [X86_64FloatReg] = &[
        X86_64FloatReg::XMM0,
        X86_64FloatReg::XMM1,
        X86_64FloatReg::XMM2,
        X86_64FloatReg::XMM3,
        X86_64FloatReg::XMM4,
        X86_64FloatReg::XMM5,
        X86_64FloatReg::XMM6,
        X86_64FloatReg::XMM7,
    ];
    const FLOAT_RETURN_REGS: &'static [X86_64FloatReg] =
        &[X86_64FloatReg::XMM0, X86_64FloatReg::XMM1];
    const FLOAT_DEFAULT_FREE_REGS: &'static [X86_64FloatReg] = &[
        // The regs we want to use first should be at the end of this vec.
        // We will use pop to get which reg to use next
        // No callee saved regs.
        // Use caller saved regs first.
        X86_64FloatReg::XMM15,
        X86_64FloatReg::XMM14,
        X86_64FloatReg::XMM13,
        X86_64FloatReg::XMM12,
        X86_64FloatReg::XMM11,
        X86_64FloatReg::XMM10,
        X86_64FloatReg::XMM9,
        X86_64FloatReg::XMM8,
        X86_64FloatReg::XMM7,
        X86_64FloatReg::XMM6,
        X86_64FloatReg::XMM5,
        X86_64FloatReg::XMM4,
        X86_64FloatReg::XMM3,
        X86_64FloatReg::XMM2,
        X86_64FloatReg::XMM1,
        X86_64FloatReg::XMM0,
    ];
    const SHADOW_SPACE_SIZE: u8 = 0;

    #[inline(always)]
    fn general_callee_saved(reg: &X86_64GeneralReg) -> bool {
        matches!(
            reg,
            X86_64GeneralReg::RBX
                | X86_64GeneralReg::RBP
                | X86_64GeneralReg::R12
                | X86_64GeneralReg::R13
                | X86_64GeneralReg::R14
                | X86_64GeneralReg::R15
        )
    }

    #[inline(always)]
    fn float_callee_saved(_reg: &X86_64FloatReg) -> bool {
        false
    }

    #[inline(always)]
    fn setup_stack<'a>(
        buf: &mut Vec<'a, u8>,
        saved_general_regs: &[X86_64GeneralReg],
        saved_float_regs: &[X86_64FloatReg],
        requested_stack_size: i32,
        fn_call_stack_size: i32,
    ) -> i32 {
        x86_64_generic_setup_stack(
            buf,
            saved_general_regs,
            saved_float_regs,
            requested_stack_size,
            fn_call_stack_size,
        )
    }

    #[inline(always)]
    fn cleanup_stack<'a>(
        buf: &mut Vec<'a, u8>,
        saved_general_regs: &[X86_64GeneralReg],
        saved_float_regs: &[X86_64FloatReg],
        aligned_stack_size: i32,
        fn_call_stack_size: i32,
    ) {
        x86_64_generic_cleanup_stack(
            buf,
            saved_general_regs,
            saved_float_regs,
            aligned_stack_size,
            fn_call_stack_size,
        )
    }

    #[inline(always)]
    fn load_args<'a>(
        _buf: &mut Vec<'a, u8>,
        storage_manager: &mut StorageManager<
            'a,
            X86_64GeneralReg,
            X86_64FloatReg,
            X86_64Assembler,
            X86_64SystemV,
        >,
        args: &'a [(Layout<'a>, Symbol)],
        ret_layout: &Layout<'a>,
    ) {
        let mut arg_offset = Self::SHADOW_SPACE_SIZE as i32 + 16; // 16 is the size of the pushed return address and base pointer.
        let mut general_i = 0;
        let mut float_i = 0;
        if X86_64SystemV::returns_via_arg_pointer(storage_manager.env.layout_interner, ret_layout) {
            storage_manager.ret_pointer_arg(Self::GENERAL_PARAM_REGS[0]);
            general_i += 1;
        }
        for (layout, sym) in args.iter() {
            let stack_size = layout.stack_size(storage_manager.env.layout_interner, TARGET_INFO);
            match layout {
                single_register_integers!() => {
                    if general_i < Self::GENERAL_PARAM_REGS.len() {
                        storage_manager.general_reg_arg(sym, Self::GENERAL_PARAM_REGS[general_i]);
                        general_i += 1;
                    } else {
                        storage_manager.primitive_stack_arg(sym, arg_offset);
                        arg_offset += 8;
                    }
                }
                single_register_floats!() => {
                    if float_i < Self::FLOAT_PARAM_REGS.len() {
                        storage_manager.float_reg_arg(sym, Self::FLOAT_PARAM_REGS[float_i]);
                        float_i += 1;
                    } else {
                        storage_manager.primitive_stack_arg(sym, arg_offset);
                        arg_offset += 8;
                    }
                }
                _ if stack_size == 0 => {
                    storage_manager.no_data_arg(sym);
                }
                _ if stack_size > 16 => {
                    // TODO: Double check this.
                    storage_manager.complex_stack_arg(sym, arg_offset, stack_size);
                    arg_offset += stack_size as i32;
                }
                x => {
                    todo!("Loading args with layout {:?}", x);
                }
            }
        }
    }

    #[inline(always)]
    fn store_args<'a>(
        buf: &mut Vec<'a, u8>,
        storage_manager: &mut StorageManager<
            'a,
            X86_64GeneralReg,
            X86_64FloatReg,
            X86_64Assembler,
            X86_64SystemV,
        >,
        dst: &Symbol,
        args: &[Symbol],
        arg_layouts: &[Layout<'a>],
        ret_layout: &Layout<'a>,
    ) {
        let mut tmp_stack_offset = Self::SHADOW_SPACE_SIZE as i32;
        let mut general_i = 0;
        let mut float_i = 0;
        if Self::returns_via_arg_pointer(storage_manager.env.layout_interner, ret_layout) {
            // Save space on the stack for the result we will be return.
            let base_offset = storage_manager.claim_stack_area(
                dst,
                ret_layout.stack_size(storage_manager.env.layout_interner, TARGET_INFO),
            );
            // Set the first reg to the address base + offset.
            let ret_reg = Self::GENERAL_PARAM_REGS[general_i];
            general_i += 1;
            X86_64Assembler::add_reg64_reg64_imm32(
                buf,
                ret_reg,
                X86_64GeneralReg::RBP,
                base_offset,
            );
        }
        for (sym, layout) in args.iter().zip(arg_layouts.iter()) {
            match layout {
                single_register_integers!() => {
                    if general_i < Self::GENERAL_PARAM_REGS.len() {
                        storage_manager.load_to_specified_general_reg(
                            buf,
                            sym,
                            Self::GENERAL_PARAM_REGS[general_i],
                        );
                        general_i += 1;
                    } else {
                        // Copy to stack using return reg as buffer.
                        storage_manager.load_to_specified_general_reg(
                            buf,
                            sym,
                            Self::GENERAL_RETURN_REGS[0],
                        );
                        X86_64Assembler::mov_stack32_reg64(
                            buf,
                            tmp_stack_offset,
                            Self::GENERAL_RETURN_REGS[0],
                        );
                        tmp_stack_offset += 8;
                    }
                }
                single_register_floats!() => {
                    if float_i < Self::FLOAT_PARAM_REGS.len() {
                        storage_manager.load_to_specified_float_reg(
                            buf,
                            sym,
                            Self::FLOAT_PARAM_REGS[float_i],
                        );
                        float_i += 1;
                    } else {
                        // Copy to stack using return reg as buffer.
                        storage_manager.load_to_specified_float_reg(
                            buf,
                            sym,
                            Self::FLOAT_RETURN_REGS[0],
                        );
                        X86_64Assembler::mov_stack32_freg64(
                            buf,
                            tmp_stack_offset,
                            Self::FLOAT_RETURN_REGS[0],
                        );
                        tmp_stack_offset += 8;
                    }
                }
                x if x.stack_size(storage_manager.env.layout_interner, TARGET_INFO) == 0 => {}
                x if x.stack_size(storage_manager.env.layout_interner, TARGET_INFO) > 16 => {
                    // TODO: Double check this.
                    // Just copy onto the stack.
                    // Use return reg as buffer because it will be empty right now.
                    let (base_offset, size) = storage_manager.stack_offset_and_size(sym);
                    debug_assert_eq!(base_offset % 8, 0);
                    for i in (0..size as i32).step_by(8) {
                        X86_64Assembler::mov_reg64_base32(
                            buf,
                            Self::GENERAL_RETURN_REGS[0],
                            base_offset + i,
                        );
                        X86_64Assembler::mov_stack32_reg64(
                            buf,
                            tmp_stack_offset + i,
                            Self::GENERAL_RETURN_REGS[0],
                        );
                    }
                    tmp_stack_offset += size as i32;
                }
                x => {
                    todo!("calling with arg type, {:?}", x);
                }
            }
        }
        storage_manager.update_fn_call_stack_size(tmp_stack_offset as u32);
    }

    fn return_complex_symbol<'a>(
        buf: &mut Vec<'a, u8>,
        storage_manager: &mut StorageManager<
            'a,
            X86_64GeneralReg,
            X86_64FloatReg,
            X86_64Assembler,
            X86_64SystemV,
        >,
        sym: &Symbol,
        layout: &Layout<'a>,
    ) {
        match layout {
            single_register_layouts!() => {
                internal_error!("single register layouts are not complex symbols");
            }
            x if x.stack_size(storage_manager.env.layout_interner, TARGET_INFO) == 0 => {}
            x if !Self::returns_via_arg_pointer(storage_manager.env.layout_interner, x) => {
                let (base_offset, size) = storage_manager.stack_offset_and_size(sym);
                debug_assert_eq!(base_offset % 8, 0);
                if size <= 8 {
                    X86_64Assembler::mov_reg64_base32(
                        buf,
                        Self::GENERAL_RETURN_REGS[0],
                        base_offset,
                    );
                } else if size <= 16 {
                    X86_64Assembler::mov_reg64_base32(
                        buf,
                        Self::GENERAL_RETURN_REGS[0],
                        base_offset,
                    );
                    X86_64Assembler::mov_reg64_base32(
                        buf,
                        Self::GENERAL_RETURN_REGS[1],
                        base_offset + 8,
                    );
                } else {
                    internal_error!(
                        "types that don't return via arg pointer must be less than 16 bytes"
                    );
                }
            }
            _ => {
                // This is a large type returned via the arg pointer.
                storage_manager.copy_symbol_to_arg_pointer(buf, sym, layout);
                // Also set the return reg to the arg pointer.
                storage_manager.load_to_specified_general_reg(
                    buf,
                    &Symbol::RET_POINTER,
                    Self::GENERAL_RETURN_REGS[0],
                );
            }
        }
    }

    fn load_returned_complex_symbol<'a>(
        buf: &mut Vec<'a, u8>,
        storage_manager: &mut StorageManager<
            'a,
            X86_64GeneralReg,
            X86_64FloatReg,
            X86_64Assembler,
            X86_64SystemV,
        >,
        sym: &Symbol,
        layout: &Layout<'a>,
    ) {
        match layout {
            single_register_layouts!() => {
                internal_error!("single register layouts are not complex symbols");
            }
            x if x.stack_size(storage_manager.env.layout_interner, TARGET_INFO) == 0 => {}
            x if !Self::returns_via_arg_pointer(storage_manager.env.layout_interner, x) => {
                let size = layout.stack_size(storage_manager.env.layout_interner, TARGET_INFO);
                let offset = storage_manager.claim_stack_area(sym, size);
                if size <= 8 {
                    X86_64Assembler::mov_base32_reg64(buf, offset, Self::GENERAL_RETURN_REGS[0]);
                } else if size <= 16 {
                    X86_64Assembler::mov_base32_reg64(buf, offset, Self::GENERAL_RETURN_REGS[0]);
                    X86_64Assembler::mov_base32_reg64(
                        buf,
                        offset + 8,
                        Self::GENERAL_RETURN_REGS[1],
                    );
                } else {
                    internal_error!(
                        "types that don't return via arg pointer must be less than 16 bytes"
                    );
                }
            }
            _ => {
                // This should have been recieved via an arg pointer.
                // That means the value is already loaded onto the stack area we allocated before the call.
                // Nothing to do.
            }
        }
    }
}

impl X86_64SystemV {
    fn returns_via_arg_pointer<'a>(
        interner: &STLayoutInterner<'a>,
        ret_layout: &Layout<'a>,
    ) -> bool {
        // TODO: This will need to be more complex/extended to fully support the calling convention.
        // details here: https://github.com/hjl-tools/x86-psABI/wiki/x86-64-psABI-1.0.pdf
        ret_layout.stack_size(interner, TARGET_INFO) > 16
    }
}

impl CallConv<X86_64GeneralReg, X86_64FloatReg, X86_64Assembler> for X86_64WindowsFastcall {
    const BASE_PTR_REG: X86_64GeneralReg = X86_64GeneralReg::RBP;
    const STACK_PTR_REG: X86_64GeneralReg = X86_64GeneralReg::RSP;

    const GENERAL_PARAM_REGS: &'static [X86_64GeneralReg] = &[
        X86_64GeneralReg::RCX,
        X86_64GeneralReg::RDX,
        X86_64GeneralReg::R8,
        X86_64GeneralReg::R9,
    ];
    const GENERAL_RETURN_REGS: &'static [X86_64GeneralReg] = &[X86_64GeneralReg::RAX];
    const GENERAL_DEFAULT_FREE_REGS: &'static [X86_64GeneralReg] = &[
        // The regs we want to use first should be at the end of this vec.
        // We will use pop to get which reg to use next

        // Don't use stack pointer: X86_64GeneralReg::RSP,
        // Don't use frame pointer: X86_64GeneralReg::RBP,

        // Use callee saved regs last.
        X86_64GeneralReg::RBX,
        X86_64GeneralReg::RSI,
        X86_64GeneralReg::RDI,
        X86_64GeneralReg::R12,
        X86_64GeneralReg::R13,
        X86_64GeneralReg::R14,
        X86_64GeneralReg::R15,
        // Use caller saved regs first.
        X86_64GeneralReg::RAX,
        X86_64GeneralReg::RCX,
        X86_64GeneralReg::RDX,
        X86_64GeneralReg::R8,
        X86_64GeneralReg::R9,
        X86_64GeneralReg::R10,
        X86_64GeneralReg::R11,
    ];
    const FLOAT_PARAM_REGS: &'static [X86_64FloatReg] = &[
        X86_64FloatReg::XMM0,
        X86_64FloatReg::XMM1,
        X86_64FloatReg::XMM2,
        X86_64FloatReg::XMM3,
    ];
    const FLOAT_RETURN_REGS: &'static [X86_64FloatReg] = &[X86_64FloatReg::XMM0];
    const FLOAT_DEFAULT_FREE_REGS: &'static [X86_64FloatReg] = &[
        // The regs we want to use first should be at the end of this vec.
        // We will use pop to get which reg to use next
        // Use callee saved regs last.
        X86_64FloatReg::XMM15,
        X86_64FloatReg::XMM15,
        X86_64FloatReg::XMM13,
        X86_64FloatReg::XMM12,
        X86_64FloatReg::XMM11,
        X86_64FloatReg::XMM10,
        X86_64FloatReg::XMM9,
        X86_64FloatReg::XMM8,
        X86_64FloatReg::XMM7,
        X86_64FloatReg::XMM6,
        // Use caller saved regs first.
        X86_64FloatReg::XMM5,
        X86_64FloatReg::XMM4,
        X86_64FloatReg::XMM3,
        X86_64FloatReg::XMM2,
        X86_64FloatReg::XMM1,
        X86_64FloatReg::XMM0,
    ];
    const SHADOW_SPACE_SIZE: u8 = 32;

    #[inline(always)]
    fn general_callee_saved(reg: &X86_64GeneralReg) -> bool {
        matches!(
            reg,
            X86_64GeneralReg::RBX
                | X86_64GeneralReg::RBP
                | X86_64GeneralReg::RSI
                | X86_64GeneralReg::RSP
                | X86_64GeneralReg::RDI
                | X86_64GeneralReg::R12
                | X86_64GeneralReg::R13
                | X86_64GeneralReg::R14
                | X86_64GeneralReg::R15
        )
    }

    #[inline(always)]
    fn float_callee_saved(reg: &X86_64FloatReg) -> bool {
        matches!(
            reg,
            X86_64FloatReg::XMM0
                | X86_64FloatReg::XMM1
                | X86_64FloatReg::XMM2
                | X86_64FloatReg::XMM3
                | X86_64FloatReg::XMM4
                | X86_64FloatReg::XMM5
        )
    }

    #[inline(always)]
    fn setup_stack<'a>(
        buf: &mut Vec<'a, u8>,
        saved_general_regs: &[X86_64GeneralReg],
        saved_float_regs: &[X86_64FloatReg],
        requested_stack_size: i32,
        fn_call_stack_size: i32,
    ) -> i32 {
        x86_64_generic_setup_stack(
            buf,
            saved_general_regs,
            saved_float_regs,
            requested_stack_size,
            fn_call_stack_size,
        )
    }

    #[inline(always)]
    fn cleanup_stack<'a>(
        buf: &mut Vec<'a, u8>,
        saved_general_regs: &[X86_64GeneralReg],
        saved_float_regs: &[X86_64FloatReg],
        aligned_stack_size: i32,
        fn_call_stack_size: i32,
    ) {
        x86_64_generic_cleanup_stack(
            buf,
            saved_general_regs,
            saved_float_regs,
            aligned_stack_size,
            fn_call_stack_size,
        )
    }

    #[inline(always)]
    fn load_args<'a>(
        _buf: &mut Vec<'a, u8>,
        storage_manager: &mut StorageManager<
            'a,
            X86_64GeneralReg,
            X86_64FloatReg,
            X86_64Assembler,
            X86_64WindowsFastcall,
        >,
        args: &'a [(Layout<'a>, Symbol)],
        ret_layout: &Layout<'a>,
    ) {
        let mut arg_offset = Self::SHADOW_SPACE_SIZE as i32 + 16; // 16 is the size of the pushed return address and base pointer.
        let mut i = 0;
        if X86_64WindowsFastcall::returns_via_arg_pointer(
            storage_manager.env.layout_interner,
            ret_layout,
        ) {
            storage_manager.ret_pointer_arg(Self::GENERAL_PARAM_REGS[i]);
            i += 1;
        }
        for (layout, sym) in args.iter() {
            if i < Self::GENERAL_PARAM_REGS.len() {
                match layout {
                    single_register_integers!() => {
                        storage_manager.general_reg_arg(sym, Self::GENERAL_PARAM_REGS[i]);
                        i += 1;
                    }
                    single_register_floats!() => {
                        storage_manager.float_reg_arg(sym, Self::FLOAT_PARAM_REGS[i]);
                        i += 1;
                    }
                    x if x.stack_size(storage_manager.env.layout_interner, TARGET_INFO) == 0 => {}
                    x => {
                        todo!("Loading args with layout {:?}", x);
                    }
                }
            } else {
                match layout {
                    single_register_layouts!() => {
                        storage_manager.primitive_stack_arg(sym, arg_offset);
                        arg_offset += 8;
                    }
                    x => {
                        todo!("Loading args with layout {:?}", x);
                    }
                };
            }
        }
    }

    #[inline(always)]
    fn store_args<'a>(
        buf: &mut Vec<'a, u8>,
        storage_manager: &mut StorageManager<
            'a,
            X86_64GeneralReg,
            X86_64FloatReg,
            X86_64Assembler,
            X86_64WindowsFastcall,
        >,
        dst: &Symbol,
        args: &[Symbol],
        arg_layouts: &[Layout<'a>],
        ret_layout: &Layout<'a>,
    ) {
        let mut tmp_stack_offset = Self::SHADOW_SPACE_SIZE as i32;
        if Self::returns_via_arg_pointer(storage_manager.env.layout_interner, ret_layout) {
            // Save space on the stack for the arg we will return.
            storage_manager.claim_stack_area(
                dst,
                ret_layout.stack_size(storage_manager.env.layout_interner, TARGET_INFO),
            );
            todo!("claim first parama reg for the address");
        }
        for (i, (sym, layout)) in args.iter().zip(arg_layouts.iter()).enumerate() {
            match layout {
                single_register_integers!() => {
                    if i < Self::GENERAL_PARAM_REGS.len() {
                        storage_manager.load_to_specified_general_reg(
                            buf,
                            sym,
                            Self::GENERAL_PARAM_REGS[i],
                        );
                    } else {
                        // Copy to stack using return reg as buffer.
                        storage_manager.load_to_specified_general_reg(
                            buf,
                            sym,
                            Self::GENERAL_RETURN_REGS[0],
                        );
                        X86_64Assembler::mov_stack32_reg64(
                            buf,
                            tmp_stack_offset,
                            Self::GENERAL_RETURN_REGS[0],
                        );
                        tmp_stack_offset += 8;
                    }
                }
                single_register_floats!() => {
                    if i < Self::FLOAT_PARAM_REGS.len() {
                        storage_manager.load_to_specified_float_reg(
                            buf,
                            sym,
                            Self::FLOAT_PARAM_REGS[i],
                        );
                    } else {
                        // Copy to stack using return reg as buffer.
                        storage_manager.load_to_specified_float_reg(
                            buf,
                            sym,
                            Self::FLOAT_RETURN_REGS[0],
                        );
                        X86_64Assembler::mov_stack32_freg64(
                            buf,
                            tmp_stack_offset,
                            Self::FLOAT_RETURN_REGS[0],
                        );
                        tmp_stack_offset += 8;
                    }
                }
                x if x.stack_size(storage_manager.env.layout_interner, TARGET_INFO) == 0 => {}
                x => {
                    todo!("calling with arg type, {:?}", x);
                }
            }
        }
        storage_manager.update_fn_call_stack_size(tmp_stack_offset as u32);
    }

    fn return_complex_symbol<'a>(
        _buf: &mut Vec<'a, u8>,
        _storage_manager: &mut StorageManager<
            'a,
            X86_64GeneralReg,
            X86_64FloatReg,
            X86_64Assembler,
            X86_64WindowsFastcall,
        >,
        _sym: &Symbol,
        _layout: &Layout<'a>,
    ) {
        todo!("Returning complex symbols for X86_64");
    }

    fn load_returned_complex_symbol<'a>(
        _buf: &mut Vec<'a, u8>,
        _storage_manager: &mut StorageManager<
            'a,
            X86_64GeneralReg,
            X86_64FloatReg,
            X86_64Assembler,
            X86_64WindowsFastcall,
        >,
        _sym: &Symbol,
        _layout: &Layout<'a>,
    ) {
        todo!("Loading returned complex symbols for X86_64");
    }
}

impl X86_64WindowsFastcall {
    fn returns_via_arg_pointer<'a>(
        interner: &STLayoutInterner<'a>,
        ret_layout: &Layout<'a>,
    ) -> bool {
        // TODO: This is not fully correct there are some exceptions for "vector" types.
        // details here: https://docs.microsoft.com/en-us/cpp/build/x64-calling-convention?view=msvc-160#return-values
        ret_layout.stack_size(interner, TARGET_INFO) > 8
    }
}

#[inline(always)]
fn x86_64_generic_setup_stack<'a>(
    buf: &mut Vec<'a, u8>,
    saved_general_regs: &[X86_64GeneralReg],
    saved_float_regs: &[X86_64FloatReg],
    requested_stack_size: i32,
    fn_call_stack_size: i32,
) -> i32 {
    X86_64Assembler::push_reg64(buf, X86_64GeneralReg::RBP);
    X86_64Assembler::mov_reg64_reg64(buf, X86_64GeneralReg::RBP, X86_64GeneralReg::RSP);

    let full_stack_size = match requested_stack_size
        .checked_add(8 * (saved_general_regs.len() + saved_float_regs.len()) as i32)
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
            X86_64Assembler::sub_reg64_reg64_imm32(
                buf,
                X86_64GeneralReg::RSP,
                X86_64GeneralReg::RSP,
                aligned_stack_size,
            );

            // Put values at the top of the stack to avoid conflicts with previously saved variables.
            let mut offset = aligned_stack_size - fn_call_stack_size;
            for reg in saved_general_regs {
                X86_64Assembler::mov_base32_reg64(buf, -offset, *reg);
                offset -= 8;
            }
            for reg in saved_float_regs {
                X86_64Assembler::mov_base32_freg64(buf, -offset, *reg);
                offset -= 8;
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
#[allow(clippy::unnecessary_wraps)]
fn x86_64_generic_cleanup_stack<'a>(
    buf: &mut Vec<'a, u8>,
    saved_general_regs: &[X86_64GeneralReg],
    saved_float_regs: &[X86_64FloatReg],
    aligned_stack_size: i32,
    fn_call_stack_size: i32,
) {
    if aligned_stack_size > 0 {
        let mut offset = aligned_stack_size - fn_call_stack_size;
        for reg in saved_general_regs {
            X86_64Assembler::mov_reg64_base32(buf, *reg, -offset);
            offset -= 8;
        }
        for reg in saved_float_regs {
            X86_64Assembler::mov_freg64_base32(buf, *reg, -offset);
            offset -= 8;
        }
        X86_64Assembler::add_reg64_reg64_imm32(
            buf,
            X86_64GeneralReg::RSP,
            X86_64GeneralReg::RSP,
            aligned_stack_size,
        );
    }
    //X86_64Assembler::mov_reg64_reg64(buf, X86_64GeneralReg::RSP, X86_64GeneralReg::RBP);
    X86_64Assembler::pop_reg64(buf, X86_64GeneralReg::RBP);
}

impl Assembler<X86_64GeneralReg, X86_64FloatReg> for X86_64Assembler {
    // These functions should map to the raw assembly functions below.
    // In some cases, that means you can just directly call one of the direct assembly functions.
    #[inline(always)]
    fn abs_reg64_reg64(buf: &mut Vec<'_, u8>, dst: X86_64GeneralReg, src: X86_64GeneralReg) {
        mov_reg64_reg64(buf, dst, src);
        neg_reg64(buf, dst);
        cmovl_reg64_reg64(buf, dst, src);
    }

    #[inline(always)]
    fn abs_freg64_freg64(
        buf: &mut Vec<'_, u8>,
        relocs: &mut Vec<'_, Relocation>,
        dst: X86_64FloatReg,
        src: X86_64FloatReg,
    ) {
        movsd_freg64_rip_offset32(buf, dst, 0);

        // TODO: make sure this constant only loads once instead of every call to abs
        relocs.push(Relocation::LocalData {
            offset: buf.len() as u64 - 4,
            data: 0x7fffffffffffffffu64.to_le_bytes().to_vec(),
        });

        andpd_freg64_freg64(buf, dst, src);
    }

    #[inline(always)]
    fn add_reg64_reg64_imm32(
        buf: &mut Vec<'_, u8>,
        dst: X86_64GeneralReg,
        src1: X86_64GeneralReg,
        imm32: i32,
    ) {
        mov_reg64_reg64(buf, dst, src1);
        add_reg64_imm32(buf, dst, imm32);
    }
    #[inline(always)]
    fn add_reg64_reg64_reg64(
        buf: &mut Vec<'_, u8>,
        dst: X86_64GeneralReg,
        src1: X86_64GeneralReg,
        src2: X86_64GeneralReg,
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
    fn add_freg32_freg32_freg32(
        buf: &mut Vec<'_, u8>,
        dst: X86_64FloatReg,
        src1: X86_64FloatReg,
        src2: X86_64FloatReg,
    ) {
        if dst == src1 {
            addss_freg32_freg32(buf, dst, src2);
        } else if dst == src2 {
            addss_freg32_freg32(buf, dst, src1);
        } else {
            movss_freg32_freg32(buf, dst, src1);
            addss_freg32_freg32(buf, dst, src2);
        }
    }
    #[inline(always)]
    fn add_freg64_freg64_freg64(
        buf: &mut Vec<'_, u8>,
        dst: X86_64FloatReg,
        src1: X86_64FloatReg,
        src2: X86_64FloatReg,
    ) {
        if dst == src1 {
            addsd_freg64_freg64(buf, dst, src2);
        } else if dst == src2 {
            addsd_freg64_freg64(buf, dst, src1);
        } else {
            movsd_freg64_freg64(buf, dst, src1);
            addsd_freg64_freg64(buf, dst, src2);
        }
    }

    #[inline(always)]
    fn call(buf: &mut Vec<'_, u8>, relocs: &mut Vec<'_, Relocation>, fn_name: String) {
        buf.extend(&[0xE8, 0x00, 0x00, 0x00, 0x00]);
        relocs.push(Relocation::LinkedFunction {
            offset: buf.len() as u64 - 4,
            name: fn_name,
        });
    }

    #[inline(always)]
    fn imul_reg64_reg64_reg64(
        buf: &mut Vec<'_, u8>,
        dst: X86_64GeneralReg,
        src1: X86_64GeneralReg,
        src2: X86_64GeneralReg,
    ) {
        mov_reg64_reg64(buf, dst, src1);
        imul_reg64_reg64(buf, dst, src2);
    }

    fn umul_reg64_reg64_reg64<'a, ASM, CC>(
        buf: &mut Vec<'a, u8>,
        storage_manager: &mut StorageManager<'a, X86_64GeneralReg, X86_64FloatReg, ASM, CC>,
        dst: X86_64GeneralReg,
        src1: X86_64GeneralReg,
        src2: X86_64GeneralReg,
    ) where
        ASM: Assembler<X86_64GeneralReg, X86_64FloatReg>,
        CC: CallConv<X86_64GeneralReg, X86_64FloatReg, ASM>,
    {
        use crate::generic64::RegStorage;

        storage_manager.ensure_reg_free(buf, RegStorage::General(X86_64GeneralReg::RAX));
        storage_manager.ensure_reg_free(buf, RegStorage::General(X86_64GeneralReg::RDX));

        mov_reg64_reg64(buf, X86_64GeneralReg::RAX, src1);
        mul_reg64_reg64(buf, src2);
        mov_reg64_reg64(buf, dst, X86_64GeneralReg::RAX);
    }

    fn mul_freg32_freg32_freg32(
        buf: &mut Vec<'_, u8>,
        dst: X86_64FloatReg,
        src1: X86_64FloatReg,
        src2: X86_64FloatReg,
    ) {
        if dst == src1 {
            mulss_freg32_freg32(buf, dst, src2);
        } else if dst == src2 {
            mulss_freg32_freg32(buf, dst, src1);
        } else {
            movss_freg32_freg32(buf, dst, src1);
            mulss_freg32_freg32(buf, dst, src2);
        }
    }
    #[inline(always)]
    fn mul_freg64_freg64_freg64(
        buf: &mut Vec<'_, u8>,
        dst: X86_64FloatReg,
        src1: X86_64FloatReg,
        src2: X86_64FloatReg,
    ) {
        if dst == src1 {
            mulsd_freg64_freg64(buf, dst, src2);
        } else if dst == src2 {
            mulsd_freg64_freg64(buf, dst, src1);
        } else {
            movsd_freg64_freg64(buf, dst, src1);
            mulsd_freg64_freg64(buf, dst, src2);
        }
    }

    fn div_freg32_freg32_freg32(
        buf: &mut Vec<'_, u8>,
        dst: X86_64FloatReg,
        src1: X86_64FloatReg,
        src2: X86_64FloatReg,
    ) {
        if dst == src1 {
            divss_freg32_freg32(buf, dst, src2);
        } else if dst == src2 {
            divss_freg32_freg32(buf, dst, src1);
        } else {
            movsd_freg64_freg64(buf, dst, src1);
            divss_freg32_freg32(buf, dst, src2);
        }
    }

    fn div_freg64_freg64_freg64(
        buf: &mut Vec<'_, u8>,
        dst: X86_64FloatReg,
        src1: X86_64FloatReg,
        src2: X86_64FloatReg,
    ) {
        if dst == src1 {
            divsd_freg64_freg64(buf, dst, src2);
        } else if dst == src2 {
            divsd_freg64_freg64(buf, dst, src1);
        } else {
            movsd_freg64_freg64(buf, dst, src1);
            divsd_freg64_freg64(buf, dst, src2);
        }
    }

    fn idiv_reg64_reg64_reg64<'a, ASM, CC>(
        buf: &mut Vec<'a, u8>,
        storage_manager: &mut StorageManager<'a, X86_64GeneralReg, X86_64FloatReg, ASM, CC>,
        dst: X86_64GeneralReg,
        src1: X86_64GeneralReg,
        src2: X86_64GeneralReg,
    ) where
        ASM: Assembler<X86_64GeneralReg, X86_64FloatReg>,
        CC: CallConv<X86_64GeneralReg, X86_64FloatReg, ASM>,
    {
        use crate::generic64::RegStorage;

        storage_manager.ensure_reg_free(buf, RegStorage::General(X86_64GeneralReg::RAX));
        storage_manager.ensure_reg_free(buf, RegStorage::General(X86_64GeneralReg::RDX));

        mov_reg64_reg64(buf, X86_64GeneralReg::RAX, src1);
        idiv_reg64_reg64(buf, src2);
        mov_reg64_reg64(buf, dst, X86_64GeneralReg::RAX);
    }

    fn udiv_reg64_reg64_reg64<'a, ASM, CC>(
        buf: &mut Vec<'a, u8>,
        storage_manager: &mut StorageManager<'a, X86_64GeneralReg, X86_64FloatReg, ASM, CC>,
        dst: X86_64GeneralReg,
        src1: X86_64GeneralReg,
        src2: X86_64GeneralReg,
    ) where
        ASM: Assembler<X86_64GeneralReg, X86_64FloatReg>,
        CC: CallConv<X86_64GeneralReg, X86_64FloatReg, ASM>,
    {
        use crate::generic64::RegStorage;

        storage_manager.ensure_reg_free(buf, RegStorage::General(X86_64GeneralReg::RAX));
        storage_manager.ensure_reg_free(buf, RegStorage::General(X86_64GeneralReg::RDX));

        mov_reg64_reg64(buf, X86_64GeneralReg::RAX, src1);
        udiv_reg64_reg64(buf, src2);
        mov_reg64_reg64(buf, dst, X86_64GeneralReg::RAX);
    }

    #[inline(always)]
    fn jmp_imm32(buf: &mut Vec<'_, u8>, offset: i32) -> usize {
        jmp_imm32(buf, offset);
        buf.len()
    }

    #[inline(always)]
    fn tail_call(buf: &mut Vec<'_, u8>) -> u64 {
        Self::jmp_imm32(buf, 0);
        buf.len() as u64 - 4
    }

    #[inline(always)]
    fn jne_reg64_imm64_imm32(
        buf: &mut Vec<'_, u8>,
        reg: X86_64GeneralReg,
        imm: u64,
        offset: i32,
    ) -> usize {
        buf.reserve(13);
        if imm > i32::MAX as u64 {
            todo!("comparison with values greater than i32::max");
        }
        cmp_reg64_imm32(buf, reg, imm as i32);
        jne_imm32(buf, offset);
        buf.len()
    }

    #[inline(always)]
    fn mov_freg32_imm32(
        buf: &mut Vec<'_, u8>,
        relocs: &mut Vec<'_, Relocation>,
        dst: X86_64FloatReg,
        imm: f32,
    ) {
        movss_freg32_rip_offset32(buf, dst, 0);
        relocs.push(Relocation::LocalData {
            offset: buf.len() as u64 - 4,
            data: imm.to_le_bytes().to_vec(),
        });
    }
    #[inline(always)]
    fn mov_freg64_imm64(
        buf: &mut Vec<'_, u8>,
        relocs: &mut Vec<'_, Relocation>,
        dst: X86_64FloatReg,
        imm: f64,
    ) {
        movsd_freg64_rip_offset32(buf, dst, 0);
        relocs.push(Relocation::LocalData {
            offset: buf.len() as u64 - 4,
            data: imm.to_le_bytes().to_vec(),
        });
    }
    #[inline(always)]
    fn mov_reg64_imm64(buf: &mut Vec<'_, u8>, dst: X86_64GeneralReg, imm: i64) {
        mov_reg64_imm64(buf, dst, imm);
    }
    #[inline(always)]
    fn mov_freg64_freg64(buf: &mut Vec<'_, u8>, dst: X86_64FloatReg, src: X86_64FloatReg) {
        movsd_freg64_freg64(buf, dst, src);
    }
    #[inline(always)]
    fn mov_reg64_reg64(buf: &mut Vec<'_, u8>, dst: X86_64GeneralReg, src: X86_64GeneralReg) {
        mov_reg64_reg64(buf, dst, src);
    }

    #[inline(always)]
    fn mov_freg64_base32(buf: &mut Vec<'_, u8>, dst: X86_64FloatReg, offset: i32) {
        movsd_freg64_base64_offset32(buf, dst, X86_64GeneralReg::RBP, offset)
    }
    #[inline(always)]
    fn mov_reg64_base32(buf: &mut Vec<'_, u8>, dst: X86_64GeneralReg, offset: i32) {
        mov_reg64_base64_offset32(buf, dst, X86_64GeneralReg::RBP, offset)
    }
    #[inline(always)]
    fn mov_base32_freg64(buf: &mut Vec<'_, u8>, offset: i32, src: X86_64FloatReg) {
        movsd_base64_offset32_freg64(buf, X86_64GeneralReg::RBP, offset, src)
    }
    #[inline(always)]
    fn mov_base32_reg64(buf: &mut Vec<'_, u8>, offset: i32, src: X86_64GeneralReg) {
        mov_base64_offset32_reg64(buf, X86_64GeneralReg::RBP, offset, src)
    }

    #[inline(always)]
    fn mov_reg64_mem64_offset32(
        buf: &mut Vec<'_, u8>,
        dst: X86_64GeneralReg,
        src: X86_64GeneralReg,
        offset: i32,
    ) {
        mov_reg64_base64_offset32(buf, dst, src, offset)
    }
    #[inline(always)]
    fn mov_mem64_offset32_reg64(
        buf: &mut Vec<'_, u8>,
        dst: X86_64GeneralReg,
        offset: i32,
        src: X86_64GeneralReg,
    ) {
        mov_base64_offset32_reg64(buf, dst, offset, src)
    }

    #[inline(always)]
    fn movsx_reg64_base32(buf: &mut Vec<'_, u8>, dst: X86_64GeneralReg, offset: i32, size: u8) {
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
    fn movzx_reg64_base32(buf: &mut Vec<'_, u8>, dst: X86_64GeneralReg, offset: i32, size: u8) {
        debug_assert!(size <= 8);
        if size == 8 {
            Self::mov_reg64_base32(buf, dst, offset);
        } else if size == 4 {
            todo!("zero extending 4 byte values");
        } else if size == 2 {
            todo!("zero extending 2 byte values");
        } else if size == 1 {
            movzx_reg64_base8_offset32(buf, dst, X86_64GeneralReg::RBP, offset);
        } else {
            internal_error!("Invalid size for zero extension: {}", size);
        }
    }

    #[inline(always)]
    fn mov_freg64_stack32(buf: &mut Vec<'_, u8>, dst: X86_64FloatReg, offset: i32) {
        movsd_freg64_base64_offset32(buf, dst, X86_64GeneralReg::RSP, offset)
    }
    #[inline(always)]
    fn mov_reg64_stack32(buf: &mut Vec<'_, u8>, dst: X86_64GeneralReg, offset: i32) {
        mov_reg64_base64_offset32(buf, dst, X86_64GeneralReg::RSP, offset)
    }
    #[inline(always)]
    fn mov_stack32_freg64(buf: &mut Vec<'_, u8>, offset: i32, src: X86_64FloatReg) {
        movsd_base64_offset32_freg64(buf, X86_64GeneralReg::RSP, offset, src)
    }
    #[inline(always)]
    fn mov_stack32_reg64(buf: &mut Vec<'_, u8>, offset: i32, src: X86_64GeneralReg) {
        mov_base64_offset32_reg64(buf, X86_64GeneralReg::RSP, offset, src)
    }

    #[inline(always)]
    fn neg_reg64_reg64(buf: &mut Vec<'_, u8>, dst: X86_64GeneralReg, src: X86_64GeneralReg) {
        mov_reg64_reg64(buf, dst, src);
        neg_reg64(buf, dst);
    }

    #[inline(always)]
    fn sub_reg64_reg64_imm32(
        buf: &mut Vec<'_, u8>,
        dst: X86_64GeneralReg,
        src1: X86_64GeneralReg,
        imm32: i32,
    ) {
        mov_reg64_reg64(buf, dst, src1);
        sub_reg64_imm32(buf, dst, imm32);
    }
    #[inline(always)]
    fn sub_reg64_reg64_reg64(
        buf: &mut Vec<'_, u8>,
        dst: X86_64GeneralReg,
        src1: X86_64GeneralReg,
        src2: X86_64GeneralReg,
    ) {
        mov_reg64_reg64(buf, dst, src1);
        sub_reg64_reg64(buf, dst, src2);
    }

    #[inline(always)]
    fn eq_reg64_reg64_reg64(
        buf: &mut Vec<'_, u8>,
        dst: X86_64GeneralReg,
        src1: X86_64GeneralReg,
        src2: X86_64GeneralReg,
    ) {
        cmp_reg64_reg64(buf, src1, src2);
        sete_reg64(buf, dst);
    }

    #[inline(always)]
    fn neq_reg64_reg64_reg64(
        buf: &mut Vec<'_, u8>,
        dst: X86_64GeneralReg,
        src1: X86_64GeneralReg,
        src2: X86_64GeneralReg,
    ) {
        cmp_reg64_reg64(buf, src1, src2);
        setne_reg64(buf, dst);
    }

    #[inline(always)]
    fn lt_reg64_reg64_reg64(
        buf: &mut Vec<'_, u8>,
        dst: X86_64GeneralReg,
        src1: X86_64GeneralReg,
        src2: X86_64GeneralReg,
    ) {
        cmp_reg64_reg64(buf, src1, src2);
        setl_reg64(buf, dst);
    }

    #[inline(always)]
    fn to_float_freg32_reg64(buf: &mut Vec<'_, u8>, dst: X86_64FloatReg, src: X86_64GeneralReg) {
        cvtsi2ss_freg64_reg64(buf, dst, src);
    }

    #[inline(always)]
    fn to_float_freg32_freg64(buf: &mut Vec<'_, u8>, dst: X86_64FloatReg, src: X86_64FloatReg) {
        cvtsd2ss_freg32_freg64(buf, dst, src);
    }

    #[inline(always)]
    fn to_float_freg64_freg32(buf: &mut Vec<'_, u8>, dst: X86_64FloatReg, src: X86_64FloatReg) {
        cvtss2sd_freg64_freg32(buf, dst, src);
    }

    #[inline(always)]
    fn to_float_freg64_reg64(buf: &mut Vec<'_, u8>, dst: X86_64FloatReg, src: X86_64GeneralReg) {
        cvtsi2sd_freg64_reg64(buf, dst, src);
    }

    #[inline(always)]
    fn lte_reg64_reg64_reg64(
        buf: &mut Vec<'_, u8>,
        dst: X86_64GeneralReg,
        src1: X86_64GeneralReg,
        src2: X86_64GeneralReg,
    ) {
        cmp_reg64_reg64(buf, src1, src2);
        setle_reg64(buf, dst);
    }

    #[inline(always)]
    fn gte_reg64_reg64_reg64(
        buf: &mut Vec<'_, u8>,
        dst: X86_64GeneralReg,
        src1: X86_64GeneralReg,
        src2: X86_64GeneralReg,
    ) {
        cmp_reg64_reg64(buf, src1, src2);
        setge_reg64(buf, dst);
    }

    #[inline(always)]
    fn ret(buf: &mut Vec<'_, u8>) {
        ret(buf);
    }

    fn set_if_overflow(buf: &mut Vec<'_, u8>, dst: X86_64GeneralReg) {
        seto_reg64(buf, dst);
    }
}

impl X86_64Assembler {
    #[inline(always)]
    fn pop_reg64(buf: &mut Vec<'_, u8>, reg: X86_64GeneralReg) {
        pop_reg64(buf, reg);
    }

    #[inline(always)]
    fn push_reg64(buf: &mut Vec<'_, u8>, reg: X86_64GeneralReg) {
        push_reg64(buf, reg);
    }
}
const REX: u8 = 0x40;

// see https://wiki.osdev.org/X86-64_Instruction_Encoding#Encoding
/// If set, 64-bit operand size is used
const REX_PREFIX_W: u8 = 0b1000;
/// Extension to the MODRM.reg
const REX_PREFIX_R: u8 = 0b0100;
#[allow(unused)]
/// Extension to the SIB.index field
const REX_PREFIX_X: u8 = 0b0010;
/// Extension to the MODRM.rm
const REX_PREFIX_B: u8 = 0b0001;

/// Wide REX
const REX_W: u8 = REX | REX_PREFIX_W;

#[inline(always)]
fn add_rm_extension<T: RegTrait>(reg: T, byte: u8) -> u8 {
    if reg.value() > 7 {
        byte | REX_PREFIX_B
    } else {
        byte
    }
}

#[inline(always)]
fn add_opcode_extension(reg: X86_64GeneralReg, byte: u8) -> u8 {
    add_rm_extension(reg, byte)
}

#[inline(always)]
fn add_reg_extension<T: RegTrait>(reg: T, byte: u8) -> u8 {
    if reg.value() > 7 {
        byte | REX_PREFIX_R
    } else {
        byte
    }
}

#[inline(always)]
fn binop_reg64_reg64(
    op_code: u8,
    buf: &mut Vec<'_, u8>,
    dst: X86_64GeneralReg,
    src: X86_64GeneralReg,
) {
    let rex = add_rm_extension(dst, REX_W);
    let rex = add_reg_extension(src, rex);
    let dst_mod = dst as u8 % 8;
    let src_mod = (src as u8 % 8) << 3;
    buf.extend(&[rex, op_code, 0xC0 | dst_mod | src_mod]);
}

#[inline(always)]
fn extended_binop_reg64_reg64(
    op_code1: u8,
    op_code2: u8,
    buf: &mut Vec<'_, u8>,
    dst: X86_64GeneralReg,
    src: X86_64GeneralReg,
) {
    let rex = add_rm_extension(dst, REX_W);
    let rex = add_reg_extension(src, rex);
    let dst_mod = dst as u8 % 8;
    let src_mod = (src as u8 % 8) << 3;
    buf.extend(&[rex, op_code1, op_code2, 0xC0 | dst_mod | src_mod]);
}

// Below here are the functions for all of the assembly instructions.
// Their names are based on the instruction and operators combined.
// You should call `buf.reserve()` if you push or extend more than once.
// Unit tests are added at the bottom of the file to ensure correct asm generation.
// Please keep these in alphanumeric order.
/// `ADD r/m64, imm32` -> Add imm32 sign-extended to 64-bits from r/m64.
#[inline(always)]
fn add_reg64_imm32(buf: &mut Vec<'_, u8>, dst: X86_64GeneralReg, imm: i32) {
    // This can be optimized if the immediate is 1 byte.
    let rex = add_rm_extension(dst, REX_W);
    let dst_mod = dst as u8 % 8;
    buf.reserve(7);
    buf.extend(&[rex, 0x81, 0xC0 | dst_mod]);
    buf.extend(&imm.to_le_bytes());
}

/// `ADD r/m64,r64` -> Add r64 to r/m64.
#[inline(always)]
fn add_reg64_reg64(buf: &mut Vec<'_, u8>, dst: X86_64GeneralReg, src: X86_64GeneralReg) {
    binop_reg64_reg64(0x01, buf, dst, src);
}

/// `ADDSD xmm1,xmm2/m64` -> Add the low double-precision floating-point value from xmm2/mem to xmm1 and store the result in xmm1.
#[inline(always)]
fn addsd_freg64_freg64(buf: &mut Vec<'_, u8>, dst: X86_64FloatReg, src: X86_64FloatReg) {
    let dst_high = dst as u8 > 7;
    let dst_mod = dst as u8 % 8;
    let src_high = src as u8 > 7;
    let src_mod = src as u8 % 8;
    if dst_high || src_high {
        buf.extend(&[
            0xF2,
            0x40 | ((dst_high as u8) << 2) | (src_high as u8),
            0x0F,
            0x58,
            0xC0 | (dst_mod << 3) | (src_mod),
        ])
    } else {
        buf.extend(&[0xF2, 0x0F, 0x58, 0xC0 | (dst_mod << 3) | (src_mod)])
    }
}

/// `ADDSS xmm1,xmm2/m64` -> Add the low single-precision floating-point value from xmm2/mem to xmm1 and store the result in xmm1.
#[inline(always)]
fn addss_freg32_freg32(buf: &mut Vec<'_, u8>, dst: X86_64FloatReg, src: X86_64FloatReg) {
    let dst_high = dst as u8 > 7;
    let dst_mod = dst as u8 % 8;
    let src_high = src as u8 > 7;
    let src_mod = src as u8 % 8;
    if dst_high || src_high {
        buf.extend(&[
            0xF3,
            0x40 | ((dst_high as u8) << 2) | (src_high as u8),
            0x0F,
            0x58,
            0xC0 | (dst_mod << 3) | (src_mod),
        ])
    } else {
        buf.extend(&[0xF3, 0x0F, 0x58, 0xC0 | (dst_mod << 3) | (src_mod)])
    }
}

/// `MULSD xmm1,xmm2/m64` -> Multiply the low double-precision floating-point value from xmm2/mem to xmm1 and store the result in xmm1.
#[inline(always)]
fn mulsd_freg64_freg64(buf: &mut Vec<'_, u8>, dst: X86_64FloatReg, src: X86_64FloatReg) {
    let dst_high = dst as u8 > 7;
    let dst_mod = dst as u8 % 8;
    let src_high = src as u8 > 7;
    let src_mod = src as u8 % 8;
    if dst_high || src_high {
        buf.extend(&[
            0xF2,
            0x40 | ((dst_high as u8) << 2) | (src_high as u8),
            0x0F,
            0x59,
            0xC0 | (dst_mod << 3) | (src_mod),
        ])
    } else {
        buf.extend(&[0xF2, 0x0F, 0x59, 0xC0 | (dst_mod << 3) | (src_mod)])
    }
}

/// `DIVSS xmm1,xmm2/m64` -> Divide the low single-precision floating-point value from xmm2/mem to xmm1 and store the result in xmm1.
#[inline(always)]
fn divss_freg32_freg32(buf: &mut Vec<'_, u8>, dst: X86_64FloatReg, src: X86_64FloatReg) {
    let dst_high = dst as u8 > 7;
    let dst_mod = dst as u8 % 8;
    let src_high = src as u8 > 7;
    let src_mod = src as u8 % 8;
    if dst_high || src_high {
        buf.extend(&[
            0xF3,
            0x40 | ((dst_high as u8) << 2) | (src_high as u8),
            0x0F,
            0x5E,
            0xC0 | (dst_mod << 3) | (src_mod),
        ])
    } else {
        buf.extend(&[0xF3, 0x0F, 0x5E, 0xC0 | (dst_mod << 3) | (src_mod)])
    }
}

/// `DIVSD xmm1,xmm2/m64` -> Divide the low double-precision floating-point value from xmm2/mem to xmm1 and store the result in xmm1.
#[inline(always)]
fn divsd_freg64_freg64(buf: &mut Vec<'_, u8>, dst: X86_64FloatReg, src: X86_64FloatReg) {
    let dst_high = dst as u8 > 7;
    let dst_mod = dst as u8 % 8;
    let src_high = src as u8 > 7;
    let src_mod = src as u8 % 8;
    if dst_high || src_high {
        buf.extend(&[
            0xF2,
            0x40 | ((dst_high as u8) << 2) | (src_high as u8),
            0x0F,
            0x5E,
            0xC0 | (dst_mod << 3) | (src_mod),
        ])
    } else {
        buf.extend(&[0xF2, 0x0F, 0x5E, 0xC0 | (dst_mod << 3) | (src_mod)])
    }
}

/// `ADDSS xmm1,xmm2/m64` -> Add the low single-precision floating-point value from xmm2/mem to xmm1 and store the result in xmm1.
#[inline(always)]
fn mulss_freg32_freg32(buf: &mut Vec<'_, u8>, dst: X86_64FloatReg, src: X86_64FloatReg) {
    let dst_high = dst as u8 > 7;
    let dst_mod = dst as u8 % 8;
    let src_high = src as u8 > 7;
    let src_mod = src as u8 % 8;
    if dst_high || src_high {
        buf.extend(&[
            0xF3,
            0x40 | ((dst_high as u8) << 2) | (src_high as u8),
            0x0F,
            0x59,
            0xC0 | (dst_mod << 3) | (src_mod),
        ])
    } else {
        buf.extend(&[0xF3, 0x0F, 0x59, 0xC0 | (dst_mod << 3) | (src_mod)])
    }
}

#[inline(always)]
fn andpd_freg64_freg64(buf: &mut Vec<'_, u8>, dst: X86_64FloatReg, src: X86_64FloatReg) {
    let dst_high = dst as u8 > 7;
    let dst_mod = dst as u8 % 8;
    let src_high = src as u8 > 7;
    let src_mod = src as u8 % 8;

    if dst_high || src_high {
        buf.extend(&[
            0x66,
            0x40 | ((dst_high as u8) << 2) | (src_high as u8),
            0x0F,
            0x54,
            0xC0 | (dst_mod << 3) | (src_mod),
        ])
    } else {
        buf.extend(&[0x66, 0x0F, 0x54, 0xC0 | (dst_mod << 3) | (src_mod)])
    }
}

/// r/m64 AND imm8 (sign-extended).
#[inline(always)]
fn and_reg64_imm8(buf: &mut Vec<'_, u8>, dst: X86_64GeneralReg, imm: i8) {
    let rex = add_rm_extension(dst, REX_W);
    let dst_mod = dst as u8 % 8;
    buf.extend(&[rex, 0x83, 0xE0 | dst_mod, imm as u8]);
}

/// `CMOVL r64,r/m64` -> Move if less (SF≠ OF).
#[inline(always)]
fn cmovl_reg64_reg64(buf: &mut Vec<'_, u8>, dst: X86_64GeneralReg, src: X86_64GeneralReg) {
    let rex = add_reg_extension(dst, REX_W);
    let rex = add_rm_extension(src, rex);
    let dst_mod = (dst as u8 % 8) << 3;
    let src_mod = src as u8 % 8;
    buf.extend(&[rex, 0x0F, 0x4C, 0xC0 | dst_mod | src_mod]);
}

/// `CMP r/m64,i32` -> Compare i32 to r/m64.
#[inline(always)]
fn cmp_reg64_imm32(buf: &mut Vec<'_, u8>, dst: X86_64GeneralReg, imm: i32) {
    let rex = add_rm_extension(dst, REX_W);
    let dst_mod = dst as u8 % 8;
    buf.reserve(7);
    buf.extend(&[rex, 0x81, 0xF8 | dst_mod]);
    buf.extend(&imm.to_le_bytes());
}

/// `CMP r/m64,r64` -> Compare r64 to r/m64.
#[inline(always)]
fn cmp_reg64_reg64(buf: &mut Vec<'_, u8>, dst: X86_64GeneralReg, src: X86_64GeneralReg) {
    binop_reg64_reg64(0x39, buf, dst, src);
}

/// `TEST r/m64,r64` -> AND r64 with r/m64; set SF, ZF, PF according to result.
#[allow(dead_code)]
#[inline(always)]
fn test_reg64_reg64(buf: &mut Vec<'_, u8>, dst: X86_64GeneralReg, src: X86_64GeneralReg) {
    binop_reg64_reg64(0x85, buf, dst, src);
}

/// `IMUL r64,r/m64` -> Signed Multiply r/m64 to r64.
#[inline(always)]
fn imul_reg64_reg64(buf: &mut Vec<'_, u8>, dst: X86_64GeneralReg, src: X86_64GeneralReg) {
    // IMUL is strange, the parameters are reversed from must other binary ops.
    // The final encoding is (src, dst) instead of (dst, src).
    extended_binop_reg64_reg64(0x0F, 0xAF, buf, src, dst);
}

/// `MUL r/m64` -> Unsigned Multiply r/m64 to r64.
#[inline(always)]
fn mul_reg64_reg64(buf: &mut Vec<'_, u8>, src: X86_64GeneralReg) {
    let mut rex = REX_W;
    rex = add_reg_extension(src, rex);

    if src.value() > 7 {
        rex |= REX_PREFIX_B;
    }

    buf.extend(&[rex, 0xF7, 0b1110_0000 | (src as u8 % 8)]);
}

/// `IDIV r/m64` -> Signed divide RDX:RAX by r/m64, with result stored in RAX ← Quotient, RDX ← Remainder.
#[inline(always)]
fn idiv_reg64_reg64(buf: &mut Vec<'_, u8>, src: X86_64GeneralReg) {
    let mut rex = REX_W;
    rex = add_reg_extension(src, rex);

    if src.value() > 7 {
        rex |= REX_PREFIX_B;
    }

    // The CQO instruction can be used to produce a double quadword dividend
    // from a quadword before a quadword division.
    //
    // The CQO instruction (available in 64-bit mode only) copies the sign (bit 63)
    // of the value in the RAX register into every bit position in the RDX register
    buf.extend(&[0x48, 0x99]);

    buf.extend(&[rex, 0xF7, 0b1111_1000 | (src as u8 % 8)]);
}

/// `DIV r/m64` -> Unsigned divide RDX:RAX by r/m64, with result stored in RAX ← Quotient, RDX ← Remainder.
#[inline(always)]
fn udiv_reg64_reg64(buf: &mut Vec<'_, u8>, src: X86_64GeneralReg) {
    let mut rex = REX_W;
    rex = add_reg_extension(src, rex);

    if src.value() > 7 {
        rex |= REX_PREFIX_B;
    }

    // The CQO instruction can be used to produce a double quadword dividend
    // from a quadword before a quadword division.
    //
    // The CQO instruction (available in 64-bit mode only) copies the sign (bit 63)
    // of the value in the RAX register into every bit position in the RDX register
    buf.extend(&[0x48, 0x99]);

    // adds a cqo (convert doubleword to quadword)
    buf.extend(&[rex, 0xF7, 0b1111_0000 | (src as u8 % 8)]);
}

/// Jump near, relative, RIP = RIP + 32-bit displacement sign extended to 64-bits.
#[inline(always)]
fn jmp_imm32(buf: &mut Vec<'_, u8>, imm: i32) {
    buf.reserve(5);
    buf.push(0xE9);
    buf.extend(&imm.to_le_bytes());
}

/// Jump near if not equal (ZF=0).
#[inline(always)]
fn jne_imm32(buf: &mut Vec<'_, u8>, imm: i32) {
    buf.reserve(6);
    buf.push(0x0F);
    buf.push(0x85);
    buf.extend(&imm.to_le_bytes());
}

/// `MOV r/m64, imm32` -> Move imm32 sign extended to 64-bits to r/m64.
#[inline(always)]
fn mov_reg64_imm32(buf: &mut Vec<'_, u8>, dst: X86_64GeneralReg, imm: i32) {
    let rex = add_rm_extension(dst, REX_W);
    let dst_mod = dst as u8 % 8;
    buf.reserve(7);
    buf.extend(&[rex, 0xC7, 0xC0 | dst_mod]);
    buf.extend(&imm.to_le_bytes());
}

/// `MOV r64, imm64` -> Move imm64 to r64.
#[inline(always)]
fn mov_reg64_imm64(buf: &mut Vec<'_, u8>, dst: X86_64GeneralReg, imm: i64) {
    if imm <= i32::MAX as i64 && imm >= i32::MIN as i64 {
        mov_reg64_imm32(buf, dst, imm as i32)
    } else {
        let rex = add_opcode_extension(dst, REX_W);
        let dst_mod = dst as u8 % 8;
        buf.reserve(10);
        buf.extend(&[rex, 0xB8 | dst_mod]);
        buf.extend(&imm.to_le_bytes());
    }
}

/// `MOV r/m64,r64` -> Move r64 to r/m64.
/// This will not generate anything if dst and src are the same.
#[inline(always)]
fn mov_reg64_reg64(buf: &mut Vec<'_, u8>, dst: X86_64GeneralReg, src: X86_64GeneralReg) {
    if dst != src {
        raw_mov_reg64_reg64(buf, dst, src);
    }
}

/// `MOV r/m64,r64` -> Move r64 to r/m64.
/// This will always generate the move. It is used for verification.
#[inline(always)]
fn raw_mov_reg64_reg64(buf: &mut Vec<'_, u8>, dst: X86_64GeneralReg, src: X86_64GeneralReg) {
    binop_reg64_reg64(0x89, buf, dst, src);
}

// The following base and stack based operations could be optimized based on how many bytes the offset actually is.

/// `MOV r/m64,r64` -> Move r64 to r/m64, where m64 references a base + offset.
#[inline(always)]
fn mov_base64_offset32_reg64(
    buf: &mut Vec<'_, u8>,
    base: X86_64GeneralReg,
    offset: i32,
    src: X86_64GeneralReg,
) {
    let rex = add_rm_extension(base, REX_W);
    let rex = add_reg_extension(src, rex);
    let src_mod = (src as u8 % 8) << 3;
    let base_mod = base as u8 % 8;
    buf.reserve(8);
    buf.extend(&[rex, 0x89, 0x80 | src_mod | base_mod]);
    // Using RSP or R12 requires a secondary index byte.
    if base == X86_64GeneralReg::RSP || base == X86_64GeneralReg::R12 {
        buf.push(0x24);
    }
    buf.extend(&offset.to_le_bytes());
}

/// `MOV r64,r/m64` -> Move r/m64 to r64, where m64 references a base + offset.
#[inline(always)]
fn mov_reg64_base64_offset32(
    buf: &mut Vec<'_, u8>,
    dst: X86_64GeneralReg,
    base: X86_64GeneralReg,
    offset: i32,
) {
    let rex = add_rm_extension(base, REX_W);
    let rex = add_reg_extension(dst, rex);
    let dst_mod = (dst as u8 % 8) << 3;
    let base_mod = base as u8 % 8;
    buf.reserve(8);
    buf.extend(&[rex, 0x8B, 0x80 | dst_mod | base_mod]);
    // Using RSP or R12 requires a secondary index byte.
    if base == X86_64GeneralReg::RSP || base == X86_64GeneralReg::R12 {
        buf.push(0x24);
    }
    buf.extend(&offset.to_le_bytes());
}

/// `MOVZX r64,r/m8` -> Move r/m8 with zero extention to r64, where m8 references a base + offset.
#[inline(always)]
fn movzx_reg64_base8_offset32(
    buf: &mut Vec<'_, u8>,
    dst: X86_64GeneralReg,
    base: X86_64GeneralReg,
    offset: i32,
) {
    let rex = add_rm_extension(base, REX_W);
    let rex = add_reg_extension(dst, rex);
    let dst_mod = (dst as u8 % 8) << 3;
    let base_mod = base as u8 % 8;
    buf.reserve(9);
    buf.extend(&[rex, 0x0F, 0xB6, 0x80 | dst_mod | base_mod]);
    // Using RSP or R12 requires a secondary index byte.
    if base == X86_64GeneralReg::RSP || base == X86_64GeneralReg::R12 {
        buf.push(0x24);
    }
    buf.extend(&offset.to_le_bytes());
}

/// `MOVSD xmm1,xmm2` -> Move scalar double-precision floating-point value from xmm2 to xmm1 register.
/// This will not generate anything if dst and src are the same.
#[inline(always)]
fn movsd_freg64_freg64(buf: &mut Vec<'_, u8>, dst: X86_64FloatReg, src: X86_64FloatReg) {
    if dst != src {
        raw_movsd_freg64_freg64(buf, dst, src);
    }
}

/// `MOVSD xmm1,xmm2` -> Move scalar double-precision floating-point value from xmm2 to xmm1 register.
/// This will always generate the move. It is used for verification.
#[inline(always)]
fn raw_movsd_freg64_freg64(buf: &mut Vec<'_, u8>, dst: X86_64FloatReg, src: X86_64FloatReg) {
    let dst_high = dst as u8 > 7;
    let dst_mod = dst as u8 % 8;
    let src_high = src as u8 > 7;
    let src_mod = src as u8 % 8;
    if dst_high || src_high {
        buf.extend(&[
            0xF2,
            0x40 | ((dst_high as u8) << 2) | (src_high as u8),
            0x0F,
            0x10,
            0xC0 | (dst_mod << 3) | (src_mod),
        ])
    } else {
        buf.extend(&[0xF2, 0x0F, 0x10, 0xC0 | (dst_mod << 3) | (src_mod)])
    }
}

/// `MOVSS xmm1,xmm2` -> Move scalar low single-precision floating-point value from xmm2 to xmm1 register.
/// This will not generate anything if dst and src are the same.
#[inline(always)]
fn movss_freg32_freg32(buf: &mut Vec<'_, u8>, dst: X86_64FloatReg, src: X86_64FloatReg) {
    if dst != src {
        raw_movss_freg32_freg32(buf, dst, src);
    }
}

/// `MOVSS xmm1,xmm2` -> Move scalar low single-precision floating-point from xmm2 to xmm1 register.
/// This will always generate the move. It is used for verification.
#[inline(always)]
fn raw_movss_freg32_freg32(buf: &mut Vec<'_, u8>, dst: X86_64FloatReg, src: X86_64FloatReg) {
    let dst_high = dst as u8 > 7;
    let dst_mod = dst as u8 % 8;
    let src_high = src as u8 > 7;
    let src_mod = src as u8 % 8;
    if dst_high || src_high {
        buf.extend(&[
            0xF3,
            0x40 | ((dst_high as u8) << 2) | (src_high as u8),
            0x0F,
            0x10,
            0xC0 | (dst_mod << 3) | (src_mod),
        ])
    } else {
        buf.extend(&[0xF3, 0x0F, 0x10, 0xC0 | (dst_mod << 3) | (src_mod)])
    }
}

// `MOVSS xmm, m32` -> Load scalar single-precision floating-point value from m32 to xmm register.
#[inline(always)]
fn movss_freg32_rip_offset32(buf: &mut Vec<'_, u8>, dst: X86_64FloatReg, offset: u32) {
    let dst_mod = dst as u8 % 8;
    if dst as u8 > 7 {
        buf.reserve(9);
        buf.extend(&[0xF3, 0x44, 0x0F, 0x10, 0x05 | (dst_mod << 3)]);
    } else {
        buf.reserve(8);
        buf.extend(&[0xF3, 0x0F, 0x10, 0x05 | (dst_mod << 3)]);
    }
    buf.extend(&offset.to_le_bytes());
}

// `MOVSD xmm, m64` -> Load scalar double-precision floating-point value from m64 to xmm register.
#[inline(always)]
fn movsd_freg64_rip_offset32(buf: &mut Vec<'_, u8>, dst: X86_64FloatReg, offset: u32) {
    let dst_mod = dst as u8 % 8;
    if dst as u8 > 7 {
        buf.reserve(9);
        buf.extend(&[0xF2, 0x44, 0x0F, 0x10, 0x05 | (dst_mod << 3)]);
    } else {
        buf.reserve(8);
        buf.extend(&[0xF2, 0x0F, 0x10, 0x05 | (dst_mod << 3)]);
    }
    buf.extend(&offset.to_le_bytes());
}

/// `MOVSD r/m64,xmm1` -> Move xmm1 to r/m64. where m64 references the base pointer.
#[inline(always)]
fn movsd_base64_offset32_freg64(
    buf: &mut Vec<'_, u8>,
    base: X86_64GeneralReg,
    offset: i32,
    src: X86_64FloatReg,
) {
    let rex = add_rm_extension(base, REX_W);
    let rex = add_reg_extension(src, rex);
    let src_mod = (src as u8 % 8) << 3;
    let base_mod = base as u8 % 8;
    buf.reserve(10);
    buf.push(0xF2);
    if src as u8 > 7 || base as u8 > 7 {
        buf.push(rex);
    }
    buf.extend(&[0x0F, 0x11, 0x80 | src_mod | base_mod]);
    // Using RSP or R12 requires a secondary index byte.
    if base == X86_64GeneralReg::RSP || base == X86_64GeneralReg::R12 {
        buf.push(0x24);
    }
    buf.extend(&offset.to_le_bytes());
}

/// `MOVSD xmm1,r/m64` -> Move r/m64 to xmm1. where m64 references the base pointer.
#[inline(always)]
fn movsd_freg64_base64_offset32(
    buf: &mut Vec<'_, u8>,
    dst: X86_64FloatReg,
    base: X86_64GeneralReg,
    offset: i32,
) {
    let rex = add_rm_extension(base, REX_W);
    let rex = add_reg_extension(dst, rex);
    let dst_mod = (dst as u8 % 8) << 3;
    let base_mod = base as u8 % 8;
    buf.reserve(10);
    buf.push(0xF2);
    if dst as u8 > 7 || base as u8 > 7 {
        buf.push(rex);
    }
    buf.extend(&[0x0F, 0x10, 0x80 | dst_mod | base_mod]);
    // Using RSP or R12 requires a secondary index byte.
    if base == X86_64GeneralReg::RSP || base == X86_64GeneralReg::R12 {
        buf.push(0x24);
    }
    buf.extend(&offset.to_le_bytes());
}

/// `NEG r/m64` -> Two's complement negate r/m64.
#[inline(always)]
fn neg_reg64(buf: &mut Vec<'_, u8>, reg: X86_64GeneralReg) {
    let rex = add_rm_extension(reg, REX_W);
    let reg_mod = reg as u8 % 8;
    buf.extend(&[rex, 0xF7, 0xD8 | reg_mod]);
}

// helper function for `set*` instructions
#[inline(always)]
fn set_reg64_help(op_code: u8, buf: &mut Vec<'_, u8>, reg: X86_64GeneralReg) {
    // XOR needs 3 bytes, actual SETE instruction need 3 or 4 bytes
    buf.reserve(7);

    // Actually apply the SETE instruction
    let reg_mod = reg as u8 % 8;
    use X86_64GeneralReg::*;
    match reg {
        RAX | RCX | RDX | RBX => buf.extend(&[0x0F, op_code, 0xC0 | reg_mod]),
        RSP | RBP | RSI | RDI => buf.extend(&[REX, 0x0F, op_code, 0xC0 | reg_mod]),
        R8 | R9 | R10 | R11 | R12 | R13 | R14 | R15 => {
            buf.extend(&[REX | 1, 0x0F, op_code, 0xC0 | reg_mod])
        }
    }

    // We and reg with 1 because the SETE instruction only applies
    // to the lower bits of the register
    and_reg64_imm8(buf, reg, 1);
}

#[inline(always)]
fn cvtsi2_help<T: RegTrait, U: RegTrait>(
    buf: &mut Vec<'_, u8>,
    op_code1: u8,
    op_code2: u8,
    dst: T,
    src: U,
) {
    let rex = add_rm_extension(src, REX_W);
    let rex = add_reg_extension(dst, rex);
    let mod1 = (dst.value() % 8) << 3;
    let mod2 = src.value() % 8;

    buf.extend(&[op_code1, rex, 0x0F, op_code2, 0xC0 | mod1 | mod2])
}

#[inline(always)]
fn cvtsx2_help<T: RegTrait, V: RegTrait>(
    buf: &mut Vec<'_, u8>,
    op_code1: u8,
    op_code2: u8,
    dst: T,
    src: V,
) {
    let mod1 = (dst.value() % 8) << 3;
    let mod2 = src.value() % 8;

    buf.extend(&[op_code1, 0x0F, op_code2, 0xC0 | mod1 | mod2])
}

/// `SETE r/m64` -> Set Byte on Condition - zero/equal (ZF=1)
#[inline(always)]
fn sete_reg64(buf: &mut Vec<'_, u8>, reg: X86_64GeneralReg) {
    set_reg64_help(0x94, buf, reg);
}

/// `CVTSS2SD xmm` -> Convert one single-precision floating-point value in xmm/m32 to one double-precision floating-point value in xmm.
#[inline(always)]
fn cvtss2sd_freg64_freg32(buf: &mut Vec<'_, u8>, dst: X86_64FloatReg, src: X86_64FloatReg) {
    cvtsx2_help(buf, 0xF3, 0x5A, dst, src)
}

/// `CVTSD2SS xmm` -> Convert one double-precision floating-point value in xmm to one single-precision floating-point value and merge with high bits.
#[inline(always)]
fn cvtsd2ss_freg32_freg64(buf: &mut Vec<'_, u8>, dst: X86_64FloatReg, src: X86_64FloatReg) {
    cvtsx2_help(buf, 0xF2, 0x5A, dst, src)
}

/// `CVTSI2SD r/m64` -> Convert one signed quadword integer from r/m64 to one double-precision floating-point value in xmm.
#[inline(always)]
fn cvtsi2sd_freg64_reg64(buf: &mut Vec<'_, u8>, dst: X86_64FloatReg, src: X86_64GeneralReg) {
    cvtsi2_help(buf, 0xF2, 0x2A, dst, src)
}

/// `CVTSI2SS r/m64` -> Convert one signed quadword integer from r/m64 to one single-precision floating-point value in xmm.
#[allow(dead_code)]
#[inline(always)]
fn cvtsi2ss_freg64_reg64(buf: &mut Vec<'_, u8>, dst: X86_64FloatReg, src: X86_64GeneralReg) {
    cvtsi2_help(buf, 0xF3, 0x2A, dst, src)
}

/// `CVTTSS2SI xmm/m32` -> Convert one single-precision floating-point value from xmm/m32 to one signed quadword integer in r64 using truncation.
#[allow(dead_code)]
#[inline(always)]
fn cvttss2si_reg64_freg64(buf: &mut Vec<'_, u8>, dst: X86_64GeneralReg, src: X86_64FloatReg) {
    cvtsi2_help(buf, 0xF3, 0x2C, dst, src)
}

/// `SETNE r/m64` -> Set byte if not equal (ZF=0).
#[inline(always)]
fn setne_reg64(buf: &mut Vec<'_, u8>, reg: X86_64GeneralReg) {
    set_reg64_help(0x95, buf, reg);
}

/// `SETL r/m64` -> Set byte if less (SF≠ OF).
#[inline(always)]
fn setl_reg64(buf: &mut Vec<'_, u8>, reg: X86_64GeneralReg) {
    set_reg64_help(0x9c, buf, reg);
}

/// `SETLE r/m64` -> Set byte if less or equal (ZF=1 or SF≠ OF).
#[inline(always)]
fn setle_reg64(buf: &mut Vec<'_, u8>, reg: X86_64GeneralReg) {
    set_reg64_help(0x9e, buf, reg);
}

/// `SETGE r/m64` -> Set byte if greater or equal (SF=OF).
#[inline(always)]
fn setge_reg64(buf: &mut Vec<'_, u8>, reg: X86_64GeneralReg) {
    set_reg64_help(0x9d, buf, reg);
}

/// `SETO r/m64` -> Set byte if oveflow flag is set.
#[inline(always)]
fn seto_reg64(buf: &mut Vec<'_, u8>, reg: X86_64GeneralReg) {
    set_reg64_help(0x90, buf, reg);
}

/// `RET` -> Near return to calling procedure.
#[inline(always)]
fn ret(buf: &mut Vec<'_, u8>) {
    buf.push(0xC3);
}

/// `SUB r/m64, imm32` -> Subtract imm32 sign-extended to 64-bits from r/m64.
#[inline(always)]
fn sub_reg64_imm32(buf: &mut Vec<'_, u8>, dst: X86_64GeneralReg, imm: i32) {
    // This can be optimized if the immediate is 1 byte.
    let rex = add_rm_extension(dst, REX_W);
    let dst_mod = dst as u8 % 8;
    buf.reserve(7);
    buf.extend(&[rex, 0x81, 0xE8 | dst_mod]);
    buf.extend(&imm.to_le_bytes());
}

/// `SUB r/m64,r64` -> Sub r64 to r/m64.
#[inline(always)]
fn sub_reg64_reg64(buf: &mut Vec<'_, u8>, dst: X86_64GeneralReg, src: X86_64GeneralReg) {
    binop_reg64_reg64(0x29, buf, dst, src);
}

/// `POP r64` -> Pop top of stack into r64; increment stack pointer. Cannot encode 32-bit operand size.
#[inline(always)]
fn pop_reg64(buf: &mut Vec<'_, u8>, reg: X86_64GeneralReg) {
    let reg_mod = reg as u8 % 8;
    if reg as u8 > 7 {
        let rex = add_opcode_extension(reg, REX);
        buf.extend(&[rex, 0x58 | reg_mod]);
    } else {
        buf.push(0x58 | reg_mod);
    }
}

/// `PUSH r64` -> Push r64,
#[inline(always)]
fn push_reg64(buf: &mut Vec<'_, u8>, reg: X86_64GeneralReg) {
    let reg_mod = reg as u8 % 8;
    if reg as u8 > 7 {
        let rex = add_opcode_extension(reg, REX);
        buf.extend(&[rex, 0x50 | reg_mod]);
    } else {
        buf.push(0x50 | reg_mod);
    }
}

/// `XOR r/m64,r64` -> Xor r64 to r/m64.
#[inline(always)]
#[allow(dead_code)]
fn xor_reg64_reg64(buf: &mut Vec<'_, u8>, dst: X86_64GeneralReg, src: X86_64GeneralReg) {
    binop_reg64_reg64(0x31, buf, dst, src);
}

// When writing tests, it is a good idea to test both a number and unnumbered register.
// This is because R8-R15 often have special instruction prefixes.
#[cfg(test)]
mod tests {
    use super::*;
    use crate::disassembler_test;
    use capstone::prelude::*;

    impl X86_64GeneralReg {
        #[allow(dead_code)]
        fn low_8bits_string(&self) -> &str {
            match self {
                X86_64GeneralReg::RAX => "al",
                X86_64GeneralReg::RBX => "bl",
                X86_64GeneralReg::RCX => "cl",
                X86_64GeneralReg::RDX => "dl",
                X86_64GeneralReg::RBP => "bpl",
                X86_64GeneralReg::RSP => "spl",
                X86_64GeneralReg::RDI => "dil",
                X86_64GeneralReg::RSI => "sil",
                X86_64GeneralReg::R8 => "r8b",
                X86_64GeneralReg::R9 => "r9b",
                X86_64GeneralReg::R10 => "r10b",
                X86_64GeneralReg::R11 => "r11b",
                X86_64GeneralReg::R12 => "r12b",
                X86_64GeneralReg::R13 => "r13b",
                X86_64GeneralReg::R14 => "r14b",
                X86_64GeneralReg::R15 => "r15b",
            }
        }
    }
    const TEST_I32: i32 = 0x12345678;
    const TEST_I64: i64 = 0x1234_5678_9ABC_DEF0;

    const ALL_GENERAL_REGS: &[X86_64GeneralReg] = &[
        X86_64GeneralReg::RAX,
        X86_64GeneralReg::RBX,
        X86_64GeneralReg::RCX,
        X86_64GeneralReg::RDX,
        X86_64GeneralReg::RBP,
        X86_64GeneralReg::RSP,
        X86_64GeneralReg::RDI,
        X86_64GeneralReg::RSI,
        X86_64GeneralReg::R8,
        X86_64GeneralReg::R9,
        X86_64GeneralReg::R10,
        X86_64GeneralReg::R11,
        X86_64GeneralReg::R12,
        X86_64GeneralReg::R13,
        X86_64GeneralReg::R14,
        X86_64GeneralReg::R15,
    ];
    const ALL_FLOAT_REGS: &[X86_64FloatReg] = &[
        X86_64FloatReg::XMM0,
        X86_64FloatReg::XMM1,
        X86_64FloatReg::XMM2,
        X86_64FloatReg::XMM3,
        X86_64FloatReg::XMM4,
        X86_64FloatReg::XMM5,
        X86_64FloatReg::XMM6,
        X86_64FloatReg::XMM7,
        X86_64FloatReg::XMM8,
        X86_64FloatReg::XMM9,
        X86_64FloatReg::XMM10,
        X86_64FloatReg::XMM11,
        X86_64FloatReg::XMM12,
        X86_64FloatReg::XMM13,
        X86_64FloatReg::XMM14,
        X86_64FloatReg::XMM15,
    ];

    fn setup_capstone_and_arena<T>(
        arena: &bumpalo::Bump,
    ) -> (bumpalo::collections::Vec<T>, Capstone) {
        let buf = bumpalo::vec![in arena];
        let cs = Capstone::new()
            .x86()
            .mode(arch::x86::ArchMode::Mode64)
            .syntax(arch::x86::ArchSyntax::Intel)
            .detail(true)
            .build()
            .expect("Failed to create Capstone object");
        (buf, cs)
    }

    #[test]
    fn test_add_reg64_imm32() {
        disassembler_test!(
            add_reg64_imm32,
            |reg, imm| format!("add {}, 0x{:x}", reg, imm),
            ALL_GENERAL_REGS,
            [TEST_I32]
        );
    }

    #[test]
    fn test_add_reg64_reg64() {
        disassembler_test!(
            add_reg64_reg64,
            |reg1, reg2| format!("add {}, {}", reg1, reg2),
            ALL_GENERAL_REGS,
            ALL_GENERAL_REGS
        );
    }

    #[test]
    fn test_sub_reg64_reg64() {
        disassembler_test!(
            sub_reg64_reg64,
            |reg1, reg2| format!("sub {}, {}", reg1, reg2),
            ALL_GENERAL_REGS,
            ALL_GENERAL_REGS
        );
    }

    #[test]
    fn test_addsd_freg64_freg64() {
        disassembler_test!(
            addsd_freg64_freg64,
            |reg1, reg2| format!("addsd {}, {}", reg1, reg2),
            ALL_FLOAT_REGS,
            ALL_FLOAT_REGS
        );
    }

    #[test]
    fn test_addss_freg32_freg32() {
        disassembler_test!(
            addss_freg32_freg32,
            |reg1, reg2| format!("addss {}, {}", reg1, reg2),
            ALL_FLOAT_REGS,
            ALL_FLOAT_REGS
        );
    }

    #[test]
    fn test_andpd_freg64_freg64() {
        disassembler_test!(
            andpd_freg64_freg64,
            |reg1, reg2| format!("andpd {}, {}", reg1, reg2),
            ALL_FLOAT_REGS,
            ALL_FLOAT_REGS
        );
    }

    #[test]
    fn test_xor_reg64_reg64() {
        disassembler_test!(
            xor_reg64_reg64,
            |reg1, reg2| format!("xor {}, {}", reg1, reg2),
            ALL_GENERAL_REGS,
            ALL_GENERAL_REGS
        );
    }

    #[test]
    fn test_cmovl_reg64_reg64() {
        disassembler_test!(
            cmovl_reg64_reg64,
            |reg1, reg2| format!("cmovl {}, {}", reg1, reg2),
            ALL_GENERAL_REGS,
            ALL_GENERAL_REGS
        );
    }

    #[test]
    fn test_cmp_reg64_imm32() {
        disassembler_test!(
            cmp_reg64_imm32,
            |reg, imm| format!("cmp {}, 0x{:x}", reg, imm),
            ALL_GENERAL_REGS,
            [TEST_I32]
        );
    }

    #[test]
    fn test_imul_reg64_reg64() {
        disassembler_test!(
            imul_reg64_reg64,
            |reg1, reg2| format!("imul {}, {}", reg1, reg2),
            ALL_GENERAL_REGS,
            ALL_GENERAL_REGS
        );
    }

    #[test]
    fn test_mul_reg64_reg64() {
        disassembler_test!(
            mul_reg64_reg64,
            |reg| format!("mul {}", reg),
            ALL_GENERAL_REGS
        );
    }

    #[test]
    fn test_mulsd_freg64_freg64() {
        disassembler_test!(
            mulsd_freg64_freg64,
            |reg1, reg2| format!("mulsd {}, {}", reg1, reg2),
            ALL_FLOAT_REGS,
            ALL_FLOAT_REGS
        );
    }

    #[test]
    fn test_mulss_freg32_freg32() {
        disassembler_test!(
            mulss_freg32_freg32,
            |reg1, reg2| format!("mulss {}, {}", reg1, reg2),
            ALL_FLOAT_REGS,
            ALL_FLOAT_REGS
        );
    }

    #[test]
    fn test_idiv_reg64_reg64() {
        disassembler_test!(
            idiv_reg64_reg64,
            |reg| format!("cqo\nidiv {}", reg),
            ALL_GENERAL_REGS
        );
    }

    #[test]
    fn test_div_reg64_reg64() {
        disassembler_test!(
            udiv_reg64_reg64,
            |reg| format!("cqo\ndiv {}", reg),
            ALL_GENERAL_REGS
        );
    }

    #[test]
    fn test_divsd_freg64_freg64() {
        disassembler_test!(
            divsd_freg64_freg64,
            |reg1, reg2| format!("divsd {}, {}", reg1, reg2),
            ALL_FLOAT_REGS,
            ALL_FLOAT_REGS
        );
    }

    #[test]
    fn test_divss_freg32_freg32() {
        disassembler_test!(
            divss_freg32_freg32,
            |reg1, reg2| format!("divss {}, {}", reg1, reg2),
            ALL_FLOAT_REGS,
            ALL_FLOAT_REGS
        );
    }

    #[test]
    fn test_jmp_imm32() {
        const INST_SIZE: i32 = 5;
        disassembler_test!(
            jmp_imm32,
            |imm| format!("jmp 0x{:x}", imm + INST_SIZE),
            [TEST_I32]
        );
    }

    #[test]
    fn test_jne_imm32() {
        const INST_SIZE: i32 = 6;
        disassembler_test!(
            jne_imm32,
            |imm| format!("jne 0x{:x}", imm + INST_SIZE),
            [TEST_I32]
        );
    }

    #[test]
    fn test_mov_reg64_imm32() {
        disassembler_test!(
            mov_reg64_imm32,
            |reg, imm| format!("mov {}, 0x{:x}", reg, imm),
            ALL_GENERAL_REGS,
            [TEST_I32]
        );
    }

    #[test]
    fn test_mov_reg64_imm64() {
        disassembler_test!(
            mov_reg64_imm64,
            |reg, imm| format!("movabs {}, 0x{:x}", reg, imm),
            ALL_GENERAL_REGS,
            [TEST_I64]
        );
        disassembler_test!(
            mov_reg64_imm64,
            |reg, imm| format!("mov {}, 0x{:x}", reg, imm),
            ALL_GENERAL_REGS,
            [TEST_I32 as i64]
        );
    }

    #[test]
    fn test_mov_reg64_reg64() {
        disassembler_test!(
            raw_mov_reg64_reg64,
            |reg1, reg2| format!("mov {}, {}", reg1, reg2),
            ALL_GENERAL_REGS,
            ALL_GENERAL_REGS
        );
    }

    #[test]
    fn test_movsd_freg64_base64_offset32() {
        disassembler_test!(
            movsd_freg64_base64_offset32,
            |reg1, reg2, imm| format!("movsd {}, qword ptr [{} + 0x{:x}]", reg1, reg2, imm),
            ALL_FLOAT_REGS,
            ALL_GENERAL_REGS,
            [TEST_I32]
        );
    }

    #[test]
    fn test_movsd_base64_offset32_freg64() {
        disassembler_test!(
            movsd_base64_offset32_freg64,
            |reg1, imm, reg2| format!("movsd qword ptr [{} + 0x{:x}], {}", reg1, imm, reg2),
            ALL_GENERAL_REGS,
            [TEST_I32],
            ALL_FLOAT_REGS
        );
    }

    #[test]
    fn test_mov_reg64_base64_offset32() {
        disassembler_test!(
            mov_reg64_base64_offset32,
            |reg1, reg2, imm| format!("mov {}, qword ptr [{} + 0x{:x}]", reg1, reg2, imm),
            ALL_GENERAL_REGS,
            ALL_GENERAL_REGS,
            [TEST_I32]
        );
    }

    #[test]
    fn test_mov_base64_offset32_reg64() {
        disassembler_test!(
            mov_base64_offset32_reg64,
            |reg1, imm, reg2| format!("mov qword ptr [{} + 0x{:x}], {}", reg1, imm, reg2),
            ALL_GENERAL_REGS,
            [TEST_I32],
            ALL_GENERAL_REGS
        );
    }

    #[test]
    fn test_movzx_reg64_base8_offset32() {
        disassembler_test!(
            movzx_reg64_base8_offset32,
            |reg1, reg2, imm| format!("movzx {}, byte ptr [{} + 0x{:x}]", reg1, reg2, imm),
            ALL_GENERAL_REGS,
            ALL_GENERAL_REGS,
            [TEST_I32]
        );
    }

    #[test]
    fn test_movsd_freg64_freg64() {
        disassembler_test!(
            raw_movsd_freg64_freg64,
            |reg1, reg2| format!("movsd {}, {}", reg1, reg2),
            ALL_FLOAT_REGS,
            ALL_FLOAT_REGS
        );
    }

    #[test]
    fn test_movss_freg32_freg32() {
        disassembler_test!(
            raw_movss_freg32_freg32,
            |reg1, reg2| format!("movss {}, {}", reg1, reg2),
            ALL_FLOAT_REGS,
            ALL_FLOAT_REGS
        );
    }

    #[test]
    fn test_movss_freg32_rip_offset32() {
        disassembler_test!(
            movss_freg32_rip_offset32,
            |reg, imm| format!("movss {}, dword ptr [rip + 0x{:x}]", reg, imm),
            ALL_FLOAT_REGS,
            [TEST_I32 as u32]
        );
    }

    #[test]
    fn test_movsd_freg64_rip_offset32() {
        disassembler_test!(
            movsd_freg64_rip_offset32,
            |reg, imm| format!("movsd {}, qword ptr [rip + 0x{:x}]", reg, imm),
            ALL_FLOAT_REGS,
            [TEST_I32 as u32]
        );
    }

    #[test]
    fn test_neg_reg64() {
        disassembler_test!(neg_reg64, |reg| format!("neg {}", reg), ALL_GENERAL_REGS);
    }

    #[test]
    fn test_cvtsi2_help() {
        const CVTSI2SS_CODE: u8 = 0x2A;
        const CVTTSS2SI_CODE: u8 = 0x2C;
        disassembler_test!(
            |buf, r1, r2| cvtsi2_help(buf, 0xF3, CVTSI2SS_CODE, r1, r2),
            |reg1, reg2| format!("cvtsi2ss {}, {}", reg1, reg2),
            ALL_FLOAT_REGS,
            ALL_GENERAL_REGS
        );
        disassembler_test!(
            |buf, r1, r2| cvtsi2_help(buf, 0xF3, CVTTSS2SI_CODE, r1, r2),
            |reg1, reg2| format!("cvttss2si {}, {}", reg1, reg2),
            ALL_GENERAL_REGS,
            ALL_FLOAT_REGS
        );
    }

    #[test]
    fn test_cvtsx2_help() {
        const CVTSS2SD_CODE: u8 = 0x5A;
        disassembler_test!(
            |buf, r1, r2| cvtsi2_help(buf, 0xF3, CVTSS2SD_CODE, r1, r2),
            |reg1, reg2| format!("cvtss2sd {}, {}", reg1, reg2),
            ALL_FLOAT_REGS,
            ALL_FLOAT_REGS
        );
    }

    #[test]
    fn test_set_reg64_help() {
        disassembler_test!(
            |buf, reg| set_reg64_help(0x94, buf, reg),
            |reg: X86_64GeneralReg| format!("sete {}\nand {}, 1", reg.low_8bits_string(), reg),
            ALL_GENERAL_REGS
        );
    }

    #[test]
    fn test_ret() {
        disassembler_test!(ret, || "ret");
    }

    #[test]
    fn test_sub_reg64_imm32() {
        disassembler_test!(
            sub_reg64_imm32,
            |reg, imm| format!("sub {}, 0x{:x}", reg, imm),
            ALL_GENERAL_REGS,
            [TEST_I32]
        );
    }

    #[test]
    fn test_pop_reg64() {
        disassembler_test!(pop_reg64, |reg| format!("pop {}", reg), ALL_GENERAL_REGS);
    }

    #[test]
    fn test_push_reg64() {
        disassembler_test!(push_reg64, |reg| format!("push {}", reg), ALL_GENERAL_REGS);
    }
}
