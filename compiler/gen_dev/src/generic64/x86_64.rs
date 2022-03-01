use crate::generic64::{storage::StorageManager, Assembler, CallConv, RegTrait};
use crate::{
    single_register_floats, single_register_integers, single_register_layouts, Relocation,
};
use bumpalo::collections::Vec;
use roc_builtins::bitcode::{FloatWidth, IntWidth};
use roc_error_macros::internal_error;
use roc_module::symbol::Symbol;
use roc_mono::layout::{Builtin, Layout};
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

pub struct X86_64Assembler {}
pub struct X86_64WindowsFastcall {}
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
        buf: &mut Vec<'a, u8>,
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
        if X86_64SystemV::returns_via_arg_pointer(ret_layout) {
            storage_manager.ret_pointer_arg(Self::GENERAL_PARAM_REGS[0]);
            general_i += 1;
        }
        for (layout, sym) in args.iter() {
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
                Layout::Builtin(Builtin::Str | Builtin::List(_)) => {
                    if general_i + 1 < Self::GENERAL_PARAM_REGS.len() {
                        // Load the value from the param reg into a useable base offset.
                        let src1 = Self::GENERAL_PARAM_REGS[general_i];
                        let src2 = Self::GENERAL_PARAM_REGS[general_i + 1];
                        let base_offset = storage_manager.claim_stack_area(sym, 16);
                        X86_64Assembler::mov_base32_reg64(buf, base_offset, src1);
                        X86_64Assembler::mov_base32_reg64(buf, base_offset + 8, src2);
                        general_i += 2;
                    } else {
                        todo!("loading lists and strings args on the stack");
                    }
                }
                x if x.stack_size(TARGET_INFO) == 0 => {}
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
        args: &'a [Symbol],
        arg_layouts: &[Layout<'a>],
        ret_layout: &Layout<'a>,
    ) {
        let mut tmp_stack_offset = Self::SHADOW_SPACE_SIZE as i32;
        if Self::returns_via_arg_pointer(ret_layout) {
            // Save space on the stack for the arg we will return.
            storage_manager
                .claim_stack_area(&Symbol::RET_POINTER, ret_layout.stack_size(TARGET_INFO));
            todo!("claim first parama reg for the address");
        }
        let mut general_i = 0;
        let mut float_i = 0;
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
                Layout::Builtin(Builtin::Str) => {
                    if general_i + 1 < Self::GENERAL_PARAM_REGS.len() {
                        let (base_offset, _size) = storage_manager.stack_offset_and_size(sym);
                        debug_assert_eq!(base_offset % 8, 0);
                        X86_64Assembler::mov_reg64_base32(
                            buf,
                            Self::GENERAL_PARAM_REGS[general_i],
                            base_offset,
                        );
                        X86_64Assembler::mov_reg64_base32(
                            buf,
                            Self::GENERAL_PARAM_REGS[general_i + 1],
                            base_offset + 8,
                        );
                        general_i += 2;
                    } else {
                        todo!("calling functions with strings on the stack");
                    }
                }
                x if x.stack_size(TARGET_INFO) == 0 => {}
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
            Layout::Builtin(Builtin::Str | Builtin::List(_)) => {
                let (base_offset, _size) = storage_manager.stack_offset_and_size(sym);
                debug_assert_eq!(base_offset % 8, 0);
                X86_64Assembler::mov_reg64_base32(buf, Self::GENERAL_RETURN_REGS[0], base_offset);
                X86_64Assembler::mov_reg64_base32(
                    buf,
                    Self::GENERAL_RETURN_REGS[1],
                    base_offset + 8,
                );
            }
            x if x.stack_size(TARGET_INFO) == 0 => {}
            x => todo!("returning complex type, {:?}", x),
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
            Layout::Builtin(Builtin::Str | Builtin::List(_)) => {
                let offset = storage_manager.claim_stack_area(sym, 16);
                X86_64Assembler::mov_base32_reg64(buf, offset, Self::GENERAL_RETURN_REGS[0]);
                X86_64Assembler::mov_base32_reg64(buf, offset + 8, Self::GENERAL_RETURN_REGS[1]);
            }
            x if x.stack_size(TARGET_INFO) == 0 => {}
            x => todo!("receiving complex return type, {:?}", x),
        }
    }
}

impl X86_64SystemV {
    fn returns_via_arg_pointer(ret_layout: &Layout) -> bool {
        // TODO: This will need to be more complex/extended to fully support the calling convention.
        // details here: https://github.com/hjl-tools/x86-psABI/wiki/x86-64-psABI-1.0.pdf
        ret_layout.stack_size(TARGET_INFO) > 16
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
        if X86_64WindowsFastcall::returns_via_arg_pointer(ret_layout) {
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
                    Layout::Builtin(Builtin::Str) => {
                        // I think this just needs to be passed on the stack, so not a huge deal.
                        todo!("Passing str args with Windows fast call");
                    }
                    x if x.stack_size(TARGET_INFO) == 0 => {}
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
        args: &'a [Symbol],
        arg_layouts: &[Layout<'a>],
        ret_layout: &Layout<'a>,
    ) {
        let mut tmp_stack_offset = Self::SHADOW_SPACE_SIZE as i32;
        if Self::returns_via_arg_pointer(ret_layout) {
            // Save space on the stack for the arg we will return.
            storage_manager
                .claim_stack_area(&Symbol::RET_POINTER, ret_layout.stack_size(TARGET_INFO));
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
                Layout::Builtin(Builtin::Str) => {
                    // I think this just needs to be passed on the stack, so not a huge deal.
                    todo!("Passing str args with Windows fast call");
                }
                x if x.stack_size(TARGET_INFO) == 0 => {}
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
    fn returns_via_arg_pointer(ret_layout: &Layout) -> bool {
        // TODO: This is not fully correct there are some exceptions for "vector" types.
        // details here: https://docs.microsoft.com/en-us/cpp/build/x64-calling-convention?view=msvc-160#return-values
        ret_layout.stack_size(TARGET_INFO) > 8
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
        if dst != src1 {
            mov_reg64_reg64(buf, dst, src1);
        }

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
        if dst != src1 {
            mov_reg64_reg64(buf, dst, src1);
        }

        imul_reg64_reg64(buf, dst, src2);
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
        if dst != src1 {
            mov_reg64_reg64(buf, dst, src1);
        }

        sub_reg64_imm32(buf, dst, imm32);
    }
    #[inline(always)]
    fn sub_reg64_reg64_reg64(
        buf: &mut Vec<'_, u8>,
        dst: X86_64GeneralReg,
        src1: X86_64GeneralReg,
        src2: X86_64GeneralReg,
    ) {
        if dst != src1 {
            mov_reg64_reg64(buf, dst, src1);
        }

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
const REX_W: u8 = REX + 0x8;

#[inline(always)]
fn add_rm_extension<T: RegTrait>(reg: T, byte: u8) -> u8 {
    if reg.value() > 7 {
        byte + 1
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
        byte + 4
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
    buf.extend(&[rex, op_code, 0xC0 + dst_mod + src_mod]);
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
    buf.extend(&[rex, op_code1, op_code2, 0xC0 + dst_mod + src_mod]);
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
    buf.extend(&[rex, 0x81, 0xC0 + dst_mod]);
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
            0x40 + ((dst_high as u8) << 2) + (src_high as u8),
            0x0F,
            0x58,
            0xC0 + (dst_mod << 3) + (src_mod),
        ])
    } else {
        buf.extend(&[0xF2, 0x0F, 0x58, 0xC0 + (dst_mod << 3) + (src_mod)])
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
            0x40 + ((dst_high as u8) << 2) + (src_high as u8),
            0x0F,
            0x54,
            0xC0 + (dst_mod << 3) + (src_mod),
        ])
    } else {
        buf.extend(&[0x66, 0x0F, 0x54, 0xC0 + (dst_mod << 3) + (src_mod)])
    }
}

/// r/m64 AND imm8 (sign-extended).
#[inline(always)]
fn and_reg64_imm8(buf: &mut Vec<'_, u8>, dst: X86_64GeneralReg, imm: i8) {
    let rex = add_rm_extension(dst, REX_W);
    let dst_mod = dst as u8 % 8;
    buf.extend(&[rex, 0x83, 0xE0 + dst_mod, imm as u8]);
}

/// `CMOVL r64,r/m64` -> Move if less (SF≠ OF).
#[inline(always)]
fn cmovl_reg64_reg64(buf: &mut Vec<'_, u8>, dst: X86_64GeneralReg, src: X86_64GeneralReg) {
    let rex = add_reg_extension(dst, REX_W);
    let rex = add_rm_extension(src, rex);
    let dst_mod = (dst as u8 % 8) << 3;
    let src_mod = src as u8 % 8;
    buf.extend(&[rex, 0x0F, 0x4C, 0xC0 + dst_mod + src_mod]);
}

/// `CMP r/m64,i32` -> Compare i32 to r/m64.
#[inline(always)]
fn cmp_reg64_imm32(buf: &mut Vec<'_, u8>, dst: X86_64GeneralReg, imm: i32) {
    let rex = add_rm_extension(dst, REX_W);
    let dst_mod = dst as u8 % 8;
    buf.reserve(7);
    buf.extend(&[rex, 0x81, 0xF8 + dst_mod]);
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
    buf.extend(&[rex, 0xC7, 0xC0 + dst_mod]);
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
        buf.extend(&[rex, 0xB8 + dst_mod]);
        buf.extend(&imm.to_le_bytes());
    }
}

/// `MOV r/m64,r64` -> Move r64 to r/m64.
#[inline(always)]
fn mov_reg64_reg64(buf: &mut Vec<'_, u8>, dst: X86_64GeneralReg, src: X86_64GeneralReg) {
    if dst != src {
        binop_reg64_reg64(0x89, buf, dst, src);
    }
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
    buf.extend(&[rex, 0x89, 0x80 + src_mod + base_mod]);
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
    buf.extend(&[rex, 0x8B, 0x80 + dst_mod + base_mod]);
    // Using RSP or R12 requires a secondary index byte.
    if base == X86_64GeneralReg::RSP || base == X86_64GeneralReg::R12 {
        buf.push(0x24);
    }
    buf.extend(&offset.to_le_bytes());
}

/// `MOVSD xmm1,xmm2` -> Move scalar double-precision floating-point value from xmm2 to xmm1 register.
#[inline(always)]
fn movsd_freg64_freg64(buf: &mut Vec<'_, u8>, dst: X86_64FloatReg, src: X86_64FloatReg) {
    if dst == src {
        return;
    }
    let dst_high = dst as u8 > 7;
    let dst_mod = dst as u8 % 8;
    let src_high = src as u8 > 7;
    let src_mod = src as u8 % 8;
    if dst_high || src_high {
        buf.extend(&[
            0xF2,
            0x40 + ((dst_high as u8) << 2) + (src_high as u8),
            0x0F,
            0x10,
            0xC0 + (dst_mod << 3) + (src_mod),
        ])
    } else {
        buf.extend(&[0xF2, 0x0F, 0x10, 0xC0 + (dst_mod << 3) + (src_mod)])
    }
}

// `MOVSS xmm, m32` -> Load scalar single-precision floating-point value from m32 to xmm register.
#[inline(always)]
fn movss_freg32_rip_offset32(buf: &mut Vec<'_, u8>, dst: X86_64FloatReg, offset: u32) {
    let dst_mod = dst as u8 % 8;
    if dst as u8 > 7 {
        buf.reserve(9);
        buf.extend(&[0xF3, 0x44, 0x0F, 0x10, 0x05 + (dst_mod << 3)]);
    } else {
        buf.reserve(8);
        buf.extend(&[0xF3, 0x0F, 0x10, 0x05 + (dst_mod << 3)]);
    }
    buf.extend(&offset.to_le_bytes());
}

// `MOVSD xmm, m64` -> Load scalar double-precision floating-point value from m64 to xmm register.
#[inline(always)]
fn movsd_freg64_rip_offset32(buf: &mut Vec<'_, u8>, dst: X86_64FloatReg, offset: u32) {
    let dst_mod = dst as u8 % 8;
    if dst as u8 > 7 {
        buf.reserve(9);
        buf.extend(&[0xF2, 0x44, 0x0F, 0x10, 0x05 + (dst_mod << 3)]);
    } else {
        buf.reserve(8);
        buf.extend(&[0xF2, 0x0F, 0x10, 0x05 + (dst_mod << 3)]);
    }
    buf.extend(&offset.to_le_bytes());
}

/// `MOVSD r/m64,xmm1` -> Move xmm1 to r/m64. where m64 references the base pionter.
#[inline(always)]
fn movsd_base64_offset32_freg64(
    buf: &mut Vec<'_, u8>,
    base: X86_64GeneralReg,
    offset: i32,
    src: X86_64FloatReg,
) {
    let src_mod = (src as u8 % 8) << 3;
    let base_mod = base as u8 % 8;
    buf.reserve(10);
    buf.push(0xF2);
    if src as u8 > 7 || base as u8 > 7 {
        buf.push(0x44);
    }
    buf.extend(&[0x0F, 0x11, 0x80 + src_mod + base_mod]);
    // Using RSP or R12 requires a secondary index byte.
    if base == X86_64GeneralReg::RSP || base == X86_64GeneralReg::R12 {
        buf.push(0x24);
    }
    buf.extend(&offset.to_le_bytes());
}

/// `MOVSD xmm1,r/m64` -> Move r/m64 to xmm1. where m64 references the base pionter.
#[inline(always)]
fn movsd_freg64_base64_offset32(
    buf: &mut Vec<'_, u8>,
    dst: X86_64FloatReg,
    base: X86_64GeneralReg,
    offset: i32,
) {
    let dst_mod = (dst as u8 % 8) << 3;
    let base_mod = base as u8 % 8;
    buf.reserve(10);
    buf.push(0xF2);
    if dst as u8 > 7 || base as u8 > 7 {
        buf.push(0x44);
    }
    buf.extend(&[0x0F, 0x10, 0x80 + dst_mod + base_mod]);
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
    buf.extend(&[rex, 0xF7, 0xD8 + reg_mod]);
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
        RAX | RCX | RDX | RBX => buf.extend(&[0x0F, op_code, 0xC0 + reg_mod]),
        RSP | RBP | RSI | RDI => buf.extend(&[REX, 0x0F, op_code, 0xC0 + reg_mod]),
        R8 | R9 | R10 | R11 | R12 | R13 | R14 | R15 => {
            buf.extend(&[REX + 1, 0x0F, op_code, 0xC0 + reg_mod])
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

    buf.extend(&[op_code1, rex, 0x0F, op_code2, 0xC0 + mod1 + mod2])
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

    buf.extend(&[op_code1, 0x0F, op_code2, 0xC0 + mod1 + mod2])
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

/// `SETGE r/m64` -> Set byte if greater or equal (SF=OF).
#[inline(always)]
fn setge_reg64(buf: &mut Vec<'_, u8>, reg: X86_64GeneralReg) {
    set_reg64_help(0x9d, buf, reg);
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
    buf.extend(&[rex, 0x81, 0xE8 + dst_mod]);
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
        buf.extend(&[rex, 0x58 + reg_mod]);
    } else {
        buf.push(0x58 + reg_mod);
    }
}

/// `PUSH r64` -> Push r64,
#[inline(always)]
fn push_reg64(buf: &mut Vec<'_, u8>, reg: X86_64GeneralReg) {
    let reg_mod = reg as u8 % 8;
    if reg as u8 > 7 {
        let rex = add_opcode_extension(reg, REX);
        buf.extend(&[rex, 0x50 + reg_mod]);
    } else {
        buf.push(0x50 + reg_mod);
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

    const TEST_I32: i32 = 0x12345678;
    const TEST_I64: i64 = 0x1234_5678_9ABC_DEF0;

    #[test]
    fn test_add_reg64_imm32() {
        let arena = bumpalo::Bump::new();
        let mut buf = bumpalo::vec![in &arena];
        for (dst, expected) in &[
            (X86_64GeneralReg::RAX, [0x48, 0x81, 0xC0]),
            (X86_64GeneralReg::R15, [0x49, 0x81, 0xC7]),
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
            (
                (X86_64GeneralReg::RAX, X86_64GeneralReg::RAX),
                [0x48, 0x01, 0xC0],
            ),
            (
                (X86_64GeneralReg::RAX, X86_64GeneralReg::R15),
                [0x4C, 0x01, 0xF8],
            ),
            (
                (X86_64GeneralReg::R15, X86_64GeneralReg::RAX),
                [0x49, 0x01, 0xC7],
            ),
            (
                (X86_64GeneralReg::R15, X86_64GeneralReg::R15),
                [0x4D, 0x01, 0xFF],
            ),
        ] {
            buf.clear();
            add_reg64_reg64(&mut buf, *dst, *src);
            assert_eq!(expected, &buf[..]);
        }
    }

    #[test]
    fn test_sub_reg64_reg64() {
        let arena = bumpalo::Bump::new();
        let mut buf = bumpalo::vec![in &arena];
        for ((dst, src), expected) in &[
            (
                (X86_64GeneralReg::RAX, X86_64GeneralReg::RAX),
                [0x48, 0x29, 0xC0],
            ),
            (
                (X86_64GeneralReg::RAX, X86_64GeneralReg::R15),
                [0x4C, 0x29, 0xF8],
            ),
            (
                (X86_64GeneralReg::R15, X86_64GeneralReg::RAX),
                [0x49, 0x29, 0xC7],
            ),
            (
                (X86_64GeneralReg::R15, X86_64GeneralReg::R15),
                [0x4D, 0x29, 0xFF],
            ),
        ] {
            buf.clear();
            sub_reg64_reg64(&mut buf, *dst, *src);
            assert_eq!(expected, &buf[..]);
        }
    }

    #[test]
    fn test_addsd_freg64_freg64() {
        let arena = bumpalo::Bump::new();
        let mut buf = bumpalo::vec![in &arena];
        for ((dst, src), expected) in &[
            (
                (X86_64FloatReg::XMM0, X86_64FloatReg::XMM0),
                vec![0xF2, 0x0F, 0x58, 0xC0],
            ),
            (
                (X86_64FloatReg::XMM0, X86_64FloatReg::XMM15),
                vec![0xF2, 0x41, 0x0F, 0x58, 0xC7],
            ),
            (
                (X86_64FloatReg::XMM15, X86_64FloatReg::XMM0),
                vec![0xF2, 0x44, 0x0F, 0x58, 0xF8],
            ),
            (
                (X86_64FloatReg::XMM15, X86_64FloatReg::XMM15),
                vec![0xF2, 0x45, 0x0F, 0x58, 0xFF],
            ),
        ] {
            buf.clear();
            addsd_freg64_freg64(&mut buf, *dst, *src);
            assert_eq!(&expected[..], &buf[..]);
        }
    }

    #[test]
    fn test_andpd_freg64_freg64() {
        let arena = bumpalo::Bump::new();
        let mut buf = bumpalo::vec![in &arena];

        for ((dst, src), expected) in &[
            (
                (X86_64FloatReg::XMM0, X86_64FloatReg::XMM0),
                vec![0x66, 0x0F, 0x54, 0xC0],
            ),
            (
                (X86_64FloatReg::XMM0, X86_64FloatReg::XMM15),
                vec![0x66, 0x41, 0x0F, 0x54, 0xC7],
            ),
            (
                (X86_64FloatReg::XMM15, X86_64FloatReg::XMM0),
                vec![0x66, 0x44, 0x0F, 0x54, 0xF8],
            ),
            (
                (X86_64FloatReg::XMM15, X86_64FloatReg::XMM15),
                vec![0x66, 0x45, 0x0F, 0x54, 0xFF],
            ),
        ] {
            buf.clear();
            andpd_freg64_freg64(&mut buf, *dst, *src);
            assert_eq!(&expected[..], &buf[..]);
        }
    }

    #[test]
    fn test_xor_reg64_reg64() {
        let arena = bumpalo::Bump::new();
        let mut buf = bumpalo::vec![in &arena];
        for ((dst, src), expected) in &[
            (
                (X86_64GeneralReg::RAX, X86_64GeneralReg::RAX),
                [0x48, 0x31, 0xC0],
            ),
            (
                (X86_64GeneralReg::RAX, X86_64GeneralReg::R15),
                [0x4C, 0x31, 0xF8],
            ),
            (
                (X86_64GeneralReg::R15, X86_64GeneralReg::RAX),
                [0x49, 0x31, 0xC7],
            ),
            (
                (X86_64GeneralReg::R15, X86_64GeneralReg::R15),
                [0x4D, 0x31, 0xFF],
            ),
        ] {
            buf.clear();
            xor_reg64_reg64(&mut buf, *dst, *src);
            assert_eq!(expected, &buf[..]);
        }
    }

    #[test]
    fn test_cmovl_reg64_reg64() {
        let arena = bumpalo::Bump::new();
        let mut buf = bumpalo::vec![in &arena];
        for ((dst, src), expected) in &[
            (
                (X86_64GeneralReg::RAX, X86_64GeneralReg::RAX),
                [0x48, 0x0F, 0x4C, 0xC0],
            ),
            (
                (X86_64GeneralReg::RAX, X86_64GeneralReg::R15),
                [0x49, 0x0F, 0x4C, 0xC7],
            ),
            (
                (X86_64GeneralReg::R15, X86_64GeneralReg::RAX),
                [0x4C, 0x0F, 0x4C, 0xF8],
            ),
            (
                (X86_64GeneralReg::R15, X86_64GeneralReg::R15),
                [0x4D, 0x0F, 0x4C, 0xFF],
            ),
        ] {
            buf.clear();
            cmovl_reg64_reg64(&mut buf, *dst, *src);
            assert_eq!(expected, &buf[..]);
        }
    }

    #[test]
    fn test_cmp_reg64_imm32() {
        let arena = bumpalo::Bump::new();
        let mut buf = bumpalo::vec![in &arena];
        for (dst, expected) in &[
            (X86_64GeneralReg::RAX, [0x48, 0x81, 0xF8]),
            (X86_64GeneralReg::R15, [0x49, 0x81, 0xFF]),
        ] {
            buf.clear();
            cmp_reg64_imm32(&mut buf, *dst, TEST_I32);
            assert_eq!(expected, &buf[..3]);
            assert_eq!(TEST_I32.to_le_bytes(), &buf[3..]);
        }
    }

    #[test]
    fn test_imul_reg64_reg64() {
        let arena = bumpalo::Bump::new();
        let mut buf = bumpalo::vec![in &arena];
        for ((dst, src), expected) in &[
            (
                (X86_64GeneralReg::RAX, X86_64GeneralReg::RAX),
                [0x48, 0x0F, 0xAF, 0xC0],
            ),
            (
                (X86_64GeneralReg::RAX, X86_64GeneralReg::R15),
                [0x49, 0x0F, 0xAF, 0xC7],
            ),
            (
                (X86_64GeneralReg::R15, X86_64GeneralReg::RAX),
                [0x4C, 0x0F, 0xAF, 0xF8],
            ),
            (
                (X86_64GeneralReg::R15, X86_64GeneralReg::R15),
                [0x4D, 0x0F, 0xAF, 0xFF],
            ),
        ] {
            buf.clear();
            imul_reg64_reg64(&mut buf, *dst, *src);
            assert_eq!(expected, &buf[..]);
        }
    }

    #[test]
    fn test_jmp_imm32() {
        let arena = bumpalo::Bump::new();
        let mut buf = bumpalo::vec![in &arena];
        jmp_imm32(&mut buf, TEST_I32);
        assert_eq!(0xE9, buf[0]);
        assert_eq!(TEST_I32.to_le_bytes(), &buf[1..]);
    }

    #[test]
    fn test_jne_imm32() {
        let arena = bumpalo::Bump::new();
        let mut buf = bumpalo::vec![in &arena];
        jne_imm32(&mut buf, TEST_I32);
        assert_eq!([0x0F, 0x85], &buf[..2]);
        assert_eq!(TEST_I32.to_le_bytes(), &buf[2..]);
    }

    #[test]
    fn test_mov_reg64_imm32() {
        let arena = bumpalo::Bump::new();
        let mut buf = bumpalo::vec![in &arena];
        for (dst, expected) in &[
            (X86_64GeneralReg::RAX, [0x48, 0xC7, 0xC0]),
            (X86_64GeneralReg::R15, [0x49, 0xC7, 0xC7]),
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
            (X86_64GeneralReg::RAX, [0x48, 0xB8]),
            (X86_64GeneralReg::R15, [0x49, 0xBF]),
        ] {
            buf.clear();
            mov_reg64_imm64(&mut buf, *dst, TEST_I64);
            assert_eq!(expected, &buf[..2]);
            assert_eq!(TEST_I64.to_le_bytes(), &buf[2..]);
        }
        for (dst, expected) in &[
            (X86_64GeneralReg::RAX, [0x48, 0xC7, 0xC0]),
            (X86_64GeneralReg::R15, [0x49, 0xC7, 0xC7]),
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
            ((X86_64GeneralReg::RAX, X86_64GeneralReg::RAX), vec![]),
            (
                (X86_64GeneralReg::RAX, X86_64GeneralReg::RCX),
                vec![0x48, 0x89, 0xC8],
            ),
            (
                (X86_64GeneralReg::RAX, X86_64GeneralReg::R15),
                vec![0x4C, 0x89, 0xF8],
            ),
            (
                (X86_64GeneralReg::R15, X86_64GeneralReg::RAX),
                vec![0x49, 0x89, 0xC7],
            ),
            (
                (X86_64GeneralReg::R15, X86_64GeneralReg::R14),
                vec![0x4D, 0x89, 0xF7],
            ),
        ] {
            buf.clear();
            mov_reg64_reg64(&mut buf, *dst, *src);
            assert_eq!(&expected[..], &buf[..]);
        }
    }

    #[test]
    fn test_movsd_freg64_base32() {
        let arena = bumpalo::Bump::new();
        let mut buf = bumpalo::vec![in &arena];
        for ((dst, offset), expected) in &[
            (
                (X86_64FloatReg::XMM0, TEST_I32),
                vec![0xF2, 0x0F, 0x10, 0x85],
            ),
            (
                (X86_64FloatReg::XMM15, TEST_I32),
                vec![0xF2, 0x44, 0x0F, 0x10, 0xBD],
            ),
        ] {
            buf.clear();
            movsd_freg64_base64_offset32(&mut buf, *dst, X86_64GeneralReg::RBP, *offset);
            assert_eq!(expected, &buf[..buf.len() - 4]);
            assert_eq!(TEST_I32.to_le_bytes(), &buf[buf.len() - 4..]);
        }
    }

    #[test]
    fn test_movsd_base32_freg64() {
        let arena = bumpalo::Bump::new();
        let mut buf = bumpalo::vec![in &arena];
        for ((offset, src), expected) in &[
            (
                (TEST_I32, X86_64FloatReg::XMM0),
                vec![0xF2, 0x0F, 0x11, 0x85],
            ),
            (
                (TEST_I32, X86_64FloatReg::XMM15),
                vec![0xF2, 0x44, 0x0F, 0x11, 0xBD],
            ),
        ] {
            buf.clear();
            movsd_base64_offset32_freg64(&mut buf, X86_64GeneralReg::RBP, *offset, *src);
            assert_eq!(expected, &buf[..buf.len() - 4]);
            assert_eq!(TEST_I32.to_le_bytes(), &buf[buf.len() - 4..]);
        }
    }

    #[test]
    fn test_movsd_freg64_stack32() {
        let arena = bumpalo::Bump::new();
        let mut buf = bumpalo::vec![in &arena];
        for ((dst, offset), expected) in &[
            (
                (X86_64FloatReg::XMM0, TEST_I32),
                vec![0xF2, 0x0F, 0x10, 0x84, 0x24],
            ),
            (
                (X86_64FloatReg::XMM15, TEST_I32),
                vec![0xF2, 0x44, 0x0F, 0x10, 0xBC, 0x24],
            ),
        ] {
            buf.clear();
            movsd_freg64_base64_offset32(&mut buf, *dst, X86_64GeneralReg::RSP, *offset);
            assert_eq!(expected, &buf[..buf.len() - 4]);
            assert_eq!(TEST_I32.to_le_bytes(), &buf[buf.len() - 4..]);
        }
    }

    #[test]
    fn test_movsd_stack32_freg64() {
        let arena = bumpalo::Bump::new();
        let mut buf = bumpalo::vec![in &arena];
        for ((offset, src), expected) in &[
            (
                (TEST_I32, X86_64FloatReg::XMM0),
                vec![0xF2, 0x0F, 0x11, 0x84, 0x24],
            ),
            (
                (TEST_I32, X86_64FloatReg::XMM15),
                vec![0xF2, 0x44, 0x0F, 0x11, 0xBC, 0x24],
            ),
        ] {
            buf.clear();
            movsd_base64_offset32_freg64(&mut buf, X86_64GeneralReg::RSP, *offset, *src);
            assert_eq!(expected, &buf[..buf.len() - 4]);
            assert_eq!(TEST_I32.to_le_bytes(), &buf[buf.len() - 4..]);
        }
    }

    #[test]
    fn test_mov_reg64_base32() {
        let arena = bumpalo::Bump::new();
        let mut buf = bumpalo::vec![in &arena];
        for ((dst, offset), expected) in &[
            ((X86_64GeneralReg::RAX, TEST_I32), [0x48, 0x8B, 0x85]),
            ((X86_64GeneralReg::R15, TEST_I32), [0x4C, 0x8B, 0xBD]),
        ] {
            buf.clear();
            mov_reg64_base64_offset32(&mut buf, *dst, X86_64GeneralReg::RBP, *offset);
            assert_eq!(expected, &buf[..3]);
            assert_eq!(TEST_I32.to_le_bytes(), &buf[3..]);
        }
    }

    #[test]
    fn test_mov_base32_reg64() {
        let arena = bumpalo::Bump::new();
        let mut buf = bumpalo::vec![in &arena];
        for ((offset, src), expected) in &[
            ((TEST_I32, X86_64GeneralReg::RAX), [0x48, 0x89, 0x85]),
            ((TEST_I32, X86_64GeneralReg::R15), [0x4C, 0x89, 0xBD]),
        ] {
            buf.clear();
            mov_base64_offset32_reg64(&mut buf, X86_64GeneralReg::RBP, *offset, *src);
            assert_eq!(expected, &buf[..3]);
            assert_eq!(TEST_I32.to_le_bytes(), &buf[3..]);
        }
    }

    #[test]
    fn test_mov_reg64_stack32() {
        let arena = bumpalo::Bump::new();
        let mut buf = bumpalo::vec![in &arena];
        for ((dst, offset), expected) in &[
            ((X86_64GeneralReg::RAX, TEST_I32), [0x48, 0x8B, 0x84, 0x24]),
            ((X86_64GeneralReg::R15, TEST_I32), [0x4C, 0x8B, 0xBC, 0x24]),
        ] {
            buf.clear();
            mov_reg64_base64_offset32(&mut buf, *dst, X86_64GeneralReg::RSP, *offset);
            assert_eq!(expected, &buf[..4]);
            assert_eq!(TEST_I32.to_le_bytes(), &buf[4..]);
        }
    }

    #[test]
    fn test_mov_stack32_reg64() {
        let arena = bumpalo::Bump::new();
        let mut buf = bumpalo::vec![in &arena];
        for ((offset, src), expected) in &[
            ((TEST_I32, X86_64GeneralReg::RAX), [0x48, 0x89, 0x84, 0x24]),
            ((TEST_I32, X86_64GeneralReg::R15), [0x4C, 0x89, 0xBC, 0x24]),
        ] {
            buf.clear();
            mov_base64_offset32_reg64(&mut buf, X86_64GeneralReg::RSP, *offset, *src);
            assert_eq!(expected, &buf[..4]);
            assert_eq!(TEST_I32.to_le_bytes(), &buf[4..]);
        }
    }

    #[test]
    fn test_movsd_freg64_freg64() {
        let arena = bumpalo::Bump::new();
        let mut buf = bumpalo::vec![in &arena];
        for ((dst, src), expected) in &[
            ((X86_64FloatReg::XMM0, X86_64FloatReg::XMM0), vec![]),
            (
                (X86_64FloatReg::XMM0, X86_64FloatReg::XMM15),
                vec![0xF2, 0x41, 0x0F, 0x10, 0xC7],
            ),
            (
                (X86_64FloatReg::XMM15, X86_64FloatReg::XMM0),
                vec![0xF2, 0x44, 0x0F, 0x10, 0xF8],
            ),
            ((X86_64FloatReg::XMM15, X86_64FloatReg::XMM15), vec![]),
        ] {
            buf.clear();
            movsd_freg64_freg64(&mut buf, *dst, *src);
            assert_eq!(&expected[..], &buf[..]);
        }
    }

    #[test]
    fn test_movss_freg32_rip_offset32() {
        let arena = bumpalo::Bump::new();
        let mut buf = bumpalo::vec![in &arena];
        for ((dst, offset), expected) in &[
            (
                (X86_64FloatReg::XMM0, TEST_I32),
                vec![0xF3, 0x0F, 0x10, 0x05],
            ),
            (
                (X86_64FloatReg::XMM15, TEST_I32),
                vec![0xF3, 0x44, 0x0F, 0x10, 0x3D],
            ),
        ] {
            buf.clear();
            movss_freg32_rip_offset32(&mut buf, *dst, *offset as u32);
            assert_eq!(&expected[..], &buf[..(buf.len() - 4)]);
            assert_eq!(TEST_I32.to_le_bytes(), &buf[(buf.len() - 4)..]);
        }
    }

    #[test]
    fn test_movsd_freg64_rip_offset32() {
        let arena = bumpalo::Bump::new();
        let mut buf = bumpalo::vec![in &arena];
        for ((dst, offset), expected) in &[
            (
                (X86_64FloatReg::XMM0, TEST_I32),
                vec![0xF2, 0x0F, 0x10, 0x05],
            ),
            (
                (X86_64FloatReg::XMM15, TEST_I32),
                vec![0xF2, 0x44, 0x0F, 0x10, 0x3D],
            ),
        ] {
            buf.clear();
            movsd_freg64_rip_offset32(&mut buf, *dst, *offset as u32);
            assert_eq!(&expected[..], &buf[..(buf.len() - 4)]);
            assert_eq!(TEST_I32.to_le_bytes(), &buf[(buf.len() - 4)..]);
        }
    }

    #[test]
    fn test_neg_reg64() {
        let arena = bumpalo::Bump::new();
        let mut buf = bumpalo::vec![in &arena];
        for (reg, expected) in &[
            (X86_64GeneralReg::RAX, [0x48, 0xF7, 0xD8]),
            (X86_64GeneralReg::R15, [0x49, 0xF7, 0xDF]),
        ] {
            buf.clear();
            neg_reg64(&mut buf, *reg);
            assert_eq!(expected, &buf[..]);
        }
    }

    #[test]
    fn test_cvtsi2_help() {
        let arena = bumpalo::Bump::new();
        let mut buf = bumpalo::vec![in &arena];
        let cvtsi2ss_code: u8 = 0x2A;
        let cvttss2si_code: u8 = 0x2C;

        for (op_code, reg1, reg2, expected) in &[
            (
                cvtsi2ss_code,
                X86_64FloatReg::XMM0,
                X86_64GeneralReg::RDI,
                [0xF3, 0x48, 0x0F, 0x2A, 0xC7],
            ),
            (
                cvtsi2ss_code,
                X86_64FloatReg::XMM15,
                X86_64GeneralReg::RDI,
                [0xF3, 0x4C, 0x0F, 0x2A, 0xFF],
            ),
            (
                cvtsi2ss_code,
                X86_64FloatReg::XMM0,
                X86_64GeneralReg::RAX,
                [0xF3, 0x48, 0x0F, 0x2A, 0xC0],
            ),
            (
                cvtsi2ss_code,
                X86_64FloatReg::XMM0,
                X86_64GeneralReg::R15,
                [0xF3, 0x49, 0x0F, 0x2A, 0xC7],
            ),
        ] {
            buf.clear();
            cvtsi2_help(&mut buf, 0xF3, *op_code, *reg1, *reg2);
            assert_eq!(expected, &buf[..]);
        }

        for (op_code, reg1, reg2, expected) in &[
            (
                cvttss2si_code,
                X86_64GeneralReg::RAX,
                X86_64FloatReg::XMM0,
                [0xF3, 0x48, 0x0F, 0x2C, 0xC0],
            ),
            (
                cvttss2si_code,
                X86_64GeneralReg::RAX,
                X86_64FloatReg::XMM15,
                [0xF3, 0x49, 0x0F, 0x2C, 0xC7],
            ),
            (
                cvttss2si_code,
                X86_64GeneralReg::RAX,
                X86_64FloatReg::XMM0,
                [0xF3, 0x48, 0x0F, 0x2C, 0xC0],
            ),
            (
                cvttss2si_code,
                X86_64GeneralReg::R15,
                X86_64FloatReg::XMM0,
                [0xF3, 0x4C, 0x0F, 0x2C, 0xF8],
            ),
        ] {
            buf.clear();
            cvtsi2_help(&mut buf, 0xF3, *op_code, *reg1, *reg2);
            assert_eq!(expected, &buf[..]);
        }
    }

    #[test]
    fn test_cvtsx2_help() {
        let arena = bumpalo::Bump::new();
        let mut buf = bumpalo::vec![in &arena];
        let cvtss2sd_code: u8 = 0x5A;

        for (op_code, reg1, reg2, expected) in &[(
            cvtss2sd_code,
            X86_64FloatReg::XMM1,
            X86_64FloatReg::XMM0,
            [0xF3, 0x0F, 0x5A, 0xC8],
        )] {
            buf.clear();
            cvtsx2_help(&mut buf, 0xF3, *op_code, *reg1, *reg2);
            assert_eq!(expected, &buf[..]);
        }
    }

    #[test]
    fn test_set_reg64_help() {
        let arena = bumpalo::Bump::new();
        let mut buf = bumpalo::vec![in &arena];

        // tests for 7 bytes in the output buffer
        let (reg, expected) = (
            X86_64GeneralReg::RAX,
            [
                0x0F, 0x94, 0xC0, // SETE al ; al are the 8 lower weight bits of rax
                0x48, 0x83, 0xE0, 0x01, // AND rax, 1
            ],
        );
        buf.clear();
        set_reg64_help(0x94, &mut buf, reg); // sete_reg64
        assert_eq!(expected, &buf[..]);

        // tests for 8 bytes in the output buffer
        for (reg, expected) in &[
            (
                X86_64GeneralReg::RSP,
                [
                    // SETE spl ; spl are the 8 lower weight bits of rsp
                    0x40, 0x0F, 0x94, 0xC4, //
                    // AND rsp, 1
                    0x48, 0x83, 0xE4, 0x01,
                ],
            ),
            (
                X86_64GeneralReg::R15,
                [
                    // SETE r15b ; r15b are the 8 lower weight bits of r15
                    0x41, 0x0F, 0x94, 0xC7, //
                    // AND rsp, 1
                    0x49, 0x83, 0xE7, 0x01,
                ],
            ),
        ] {
            buf.clear();
            set_reg64_help(0x94, &mut buf, *reg); // sete_reg64
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
            (X86_64GeneralReg::RAX, [0x48, 0x81, 0xE8]),
            (X86_64GeneralReg::R15, [0x49, 0x81, 0xEF]),
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
            (X86_64GeneralReg::RAX, vec![0x58]),
            (X86_64GeneralReg::R15, vec![0x41, 0x5F]),
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
            (X86_64GeneralReg::RAX, vec![0x50]),
            (X86_64GeneralReg::R15, vec![0x41, 0x57]),
        ] {
            buf.clear();
            push_reg64(&mut buf, *src);
            assert_eq!(&expected[..], &buf[..]);
        }
    }
}
