#![allow(clippy::redundant_closure_call)]
//|> clippy false positive: https://github.com/rust-lang/rust-clippy/issues/1553

use crate::generic64::{storage::StorageManager, Assembler, CallConv, RegTrait};
use crate::{
    pointer_layouts, single_register_floats, single_register_int_builtins,
    single_register_integers, single_register_layouts, Relocation,
};
use bumpalo::collections::Vec;
use roc_builtins::bitcode::{FloatWidth, IntWidth};
use roc_error_macros::internal_error;
use roc_module::symbol::Symbol;
use roc_mono::layout::{
    Builtin, InLayout, Layout, LayoutInterner, LayoutRepr, STLayoutInterner, UnionLayout,
};

use super::{CompareOperation, RegisterWidth};

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

    // These are registers that a called function must save and restore if it wants to use them.
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
    fn setup_stack(
        buf: &mut Vec<'_, u8>,
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
    fn cleanup_stack(
        buf: &mut Vec<'_, u8>,
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
            '_,
            X86_64GeneralReg,
            X86_64FloatReg,
            X86_64Assembler,
            X86_64SystemV,
        >,
        layout_interner: &mut STLayoutInterner<'a>,
        args: &'a [(InLayout<'a>, Symbol)],
        ret_layout: &InLayout<'a>,
    ) {
        let returns_via_pointer =
            X86_64SystemV::returns_via_arg_pointer(layout_interner, ret_layout);

        let mut state = X64_64SystemVLoadArgs {
            general_i: usize::from(returns_via_pointer),
            float_i: 0,
            // 16 is the size of the pushed return address and base pointer.
            argument_offset: X86_64SystemV::SHADOW_SPACE_SIZE as i32 + 16,
        };

        if returns_via_pointer {
            storage_manager.ret_pointer_arg(X86_64SystemV::GENERAL_PARAM_REGS[0]);
        }

        for (in_layout, sym) in args.iter() {
            state.load_arg(buf, storage_manager, layout_interner, *sym, *in_layout);
        }
    }

    #[inline(always)]
    fn store_args<'a>(
        buf: &mut Vec<'a, u8>,
        storage_manager: &mut StorageManager<
            'a,
            '_,
            X86_64GeneralReg,
            X86_64FloatReg,
            X86_64Assembler,
            X86_64SystemV,
        >,
        layout_interner: &mut STLayoutInterner<'a>,
        dst: &Symbol,
        args: &[Symbol],
        arg_layouts: &[InLayout<'a>],
        ret_layout: &InLayout<'a>,
    ) {
        let mut general_i = 0;

        if Self::returns_via_arg_pointer(layout_interner, ret_layout) {
            // Save space on the stack for the result we will be return.
            let base_offset =
                storage_manager.claim_stack_area_layout(layout_interner, *dst, *ret_layout);
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

        let mut state = X64_64SystemVStoreArgs {
            general_i,
            float_i: 0,
            tmp_stack_offset: Self::SHADOW_SPACE_SIZE as i32,
        };

        for (sym, in_layout) in args.iter().zip(arg_layouts.iter()) {
            state.store_arg(buf, storage_manager, layout_interner, *sym, *in_layout);
        }

        storage_manager.update_fn_call_stack_size(state.tmp_stack_offset as u32);
    }

    fn return_complex_symbol<'a>(
        buf: &mut Vec<'a, u8>,
        storage_manager: &mut StorageManager<
            'a,
            '_,
            X86_64GeneralReg,
            X86_64FloatReg,
            X86_64Assembler,
            X86_64SystemV,
        >,
        layout_interner: &mut STLayoutInterner<'a>,
        sym: &Symbol,
        layout: &InLayout<'a>,
    ) {
        match layout_interner.get_repr(*layout) {
            single_register_layouts!() => {
                internal_error!("single register layouts are not complex symbols");
            }
            _ if layout_interner.stack_size(*layout) == 0 => {}
            _ if !Self::returns_via_arg_pointer(layout_interner, layout) => {
                let (base_offset, size) = storage_manager.stack_offset_and_size(sym);

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
            '_,
            X86_64GeneralReg,
            X86_64FloatReg,
            X86_64Assembler,
            X86_64SystemV,
        >,
        layout_interner: &mut STLayoutInterner<'a>,
        sym: &Symbol,
        layout: &InLayout<'a>,
    ) {
        match layout_interner.get_repr(*layout) {
            single_register_layouts!() => {
                internal_error!("single register layouts are not complex symbols");
            }
            _ if layout_interner.stack_size(*layout) == 0 => {
                storage_manager.no_data(sym);
            }
            _ if !Self::returns_via_arg_pointer(layout_interner, layout) => {
                let size = layout_interner.stack_size(*layout);
                let offset =
                    storage_manager.claim_stack_area_layout(layout_interner, *sym, *layout);
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
                // This should have been received via an arg pointer.
                // That means the value is already loaded onto the stack area we allocated before the call.
                // Nothing to do.
            }
        }
    }

    fn setjmp(buf: &mut Vec<'_, u8>) {
        use X86_64GeneralReg::*;
        type ASM = X86_64Assembler;

        // based on the musl libc setjmp implementation
        //
        // 000000000020237c <__setjmp>:
        //   20237c:    48 89 1f                mov    QWORD PTR [rdi],rbx
        //   20237f:    48 89 6f 08             mov    QWORD PTR [rdi+0x8],rbp
        //   202383:    4c 89 67 10             mov    QWORD PTR [rdi+0x10],r12
        //   202387:    4c 89 6f 18             mov    QWORD PTR [rdi+0x18],r13
        //   20238b:    4c 89 77 20             mov    QWORD PTR [rdi+0x20],r14
        //   20238f:    4c 89 7f 28             mov    QWORD PTR [rdi+0x28],r15
        //   202393:    48 8d 54 24 08          lea    rdx,[rsp+0x8]
        //   202398:    48 89 57 30             mov    QWORD PTR [rdi+0x30],rdx
        //   20239c:    48 8b 14 24             mov    rdx,QWORD PTR [rsp]
        //   2023a0:    48 89 57 38             mov    QWORD PTR [rdi+0x38],rdx
        //   2023a4:    31 c0                   xor    eax,eax
        //   2023a6:    c3                      ret

        let env = RDI;

        // store caller-saved (i.e. non-volatile) registers
        ASM::mov_mem64_offset32_reg64(buf, env, 0x00, RBX);
        ASM::mov_mem64_offset32_reg64(buf, env, 0x08, RBP);
        ASM::mov_mem64_offset32_reg64(buf, env, 0x10, R12);
        ASM::mov_mem64_offset32_reg64(buf, env, 0x18, R13);
        ASM::mov_mem64_offset32_reg64(buf, env, 0x20, R14);
        ASM::mov_mem64_offset32_reg64(buf, env, 0x28, R15);

        // go one value up (as if setjmp wasn't called)
        lea_reg64_offset8(buf, RDX, RSP, 0x8);

        // store the new stack pointer
        ASM::mov_mem64_offset32_reg64(buf, env, 0x30, RDX);

        // store the address we'll resume at
        ASM::mov_reg64_mem64_offset32(buf, RDX, RSP, 0);
        ASM::mov_mem64_offset32_reg64(buf, env, 0x38, RDX);

        // zero out eax, so we return 0 (we do a 64-bit xor for convenience)
        ASM::xor_reg64_reg64_reg64(buf, RAX, RAX, RAX);

        ASM::ret(buf)
    }

    fn longjmp(buf: &mut Vec<'_, u8>) {
        use X86_64GeneralReg::*;
        type ASM = X86_64Assembler;

        //  202358:   31 c0                   xor    eax,eax
        //  20235a:   83 fe 01                cmp    esi,0x1
        //  20235d:   11 f0                   adc    eax,esi
        //  20235f:   48 8b 1f                mov    rbx,QWORD PTR [rdi]
        //  202362:   48 8b 6f 08             mov    rbp,QWORD PTR [rdi+0x8]
        //  202366:   4c 8b 67 10             mov    r12,QWORD PTR [rdi+0x10]
        //  20236a:   4c 8b 6f 18             mov    r13,QWORD PTR [rdi+0x18]
        //  20236e:   4c 8b 77 20             mov    r14,QWORD PTR [rdi+0x20]
        //  202372:   4c 8b 7f 28             mov    r15,QWORD PTR [rdi+0x28]
        //  202376:   48 8b 67 30             mov    rsp,QWORD PTR [rdi+0x30]
        //  20237a:   ff 67 38                jmp    QWORD PTR [rdi+0x38]

        // make sure something nonzero is returned ?!
        ASM::mov_reg64_reg64(buf, RAX, RSI);

        // load the caller-saved registers
        let env = RDI;
        ASM::mov_reg64_mem64_offset32(buf, RBX, env, 0x00);
        ASM::mov_reg64_mem64_offset32(buf, RBP, env, 0x08);
        ASM::mov_reg64_mem64_offset32(buf, R12, env, 0x10);
        ASM::mov_reg64_mem64_offset32(buf, R13, env, 0x18);
        ASM::mov_reg64_mem64_offset32(buf, R14, env, 0x20);
        ASM::mov_reg64_mem64_offset32(buf, R15, env, 0x28);

        // value of rsp before the setjmp call
        ASM::mov_reg64_mem64_offset32(buf, RSP, env, 0x30);

        jmp_reg64_offset8(buf, env, 0x38)
    }

    fn roc_panic(buf: &mut Vec<'_, u8>, relocs: &mut Vec<'_, Relocation>) {
        use X86_64GeneralReg::*;
        type ASM = X86_64Assembler;

        // move the first argument to roc_panic (a *const RocStr) into r8
        ASM::mov_reg64_reg64(buf, R8, RDI);

        // move the crash tag into the second return register. We add 1 to it because the 0 value
        // is already used for "no crash occurred"
        ASM::add_reg64_reg64_imm32(buf, RDX, RSI, 1);

        // the setlongjmp_buffer
        ASM::data_pointer(buf, relocs, String::from("setlongjmp_buffer"), RDI);

        // the value to return from the longjmp. It is a pointer to the last 3 words of the setlongjmp_buffer
        // they represent the errore message.
        ASM::mov_reg64_imm64(buf, RSI, 0x40);
        ASM::add_reg64_reg64_reg64(buf, RSI, RSI, RDI);

        for offset in [0, 8, 16] {
            ASM::mov_reg64_mem64_offset32(buf, R9, R8, offset);
            ASM::mov_mem64_offset32_reg64(buf, RSI, offset, R9);
        }

        Self::longjmp(buf)
    }
}

fn copy_symbol_to_stack_offset<'a, CC>(
    buf: &mut Vec<'a, u8>,
    storage_manager: &mut X86_64StorageManager<'a, '_, CC>,
    sym: Symbol,
    tmp_reg: X86_64GeneralReg,
    stack_offset: i32,
) -> u32
where
    CC: CallConv<X86_64GeneralReg, X86_64FloatReg, X86_64Assembler>,
{
    type ASM = X86_64Assembler;

    let mut copied = 0;
    let (base_offset, size) = storage_manager.stack_offset_and_size(&sym);

    if size - copied >= 8 {
        for _ in 0..(size - copied) / 8 {
            ASM::mov_reg64_base32(buf, tmp_reg, base_offset + copied as i32);
            ASM::mov_stack32_reg64(buf, stack_offset + copied as i32, tmp_reg);

            copied += 8;
        }
    }

    if size - copied >= 4 {
        for _ in 0..(size - copied) / 4 {
            ASM::mov_reg32_base32(buf, tmp_reg, base_offset + copied as i32);
            ASM::mov_stack32_reg32(buf, stack_offset + copied as i32, tmp_reg);

            copied += 4;
        }
    }

    if size - copied >= 2 {
        for _ in 0..(size - copied) / 2 {
            ASM::mov_reg16_base32(buf, tmp_reg, base_offset + copied as i32);
            ASM::mov_stack32_reg16(buf, stack_offset + copied as i32, tmp_reg);

            copied += 2;
        }
    }

    if size - copied >= 1 {
        for _ in 0..(size - copied) {
            ASM::mov_reg8_base32(buf, tmp_reg, base_offset + copied as i32);
            ASM::mov_stack32_reg8(buf, stack_offset + copied as i32, tmp_reg);

            copied += 1;
        }
    }

    size
}

pub(crate) fn copy_to_base_offset<GeneralReg, FloatReg, ASM>(
    buf: &mut Vec<'_, u8>,
    dst_base_offset: i32,
    stack_size: u32,
    ptr_reg: GeneralReg,
    tmp_reg: GeneralReg,
    read_offset: i32,
) where
    FloatReg: RegTrait,
    GeneralReg: RegTrait,
    ASM: Assembler<GeneralReg, FloatReg>,
{
    let mut copied = 0;
    let size = stack_size as i32;
    let base_offset = dst_base_offset;

    if size - copied >= 8 {
        for _ in (0..(size - copied)).step_by(8) {
            ASM::mov_reg64_mem64_offset32(buf, tmp_reg, ptr_reg, read_offset + copied);
            ASM::mov_base32_reg64(buf, base_offset + copied, tmp_reg);

            copied += 8;
        }
    }

    if size - copied >= 4 {
        for _ in (0..(size - copied)).step_by(4) {
            ASM::mov_reg32_mem32_offset32(buf, tmp_reg, ptr_reg, read_offset + copied);
            ASM::mov_base32_reg32(buf, base_offset + copied, tmp_reg);

            copied += 4;
        }
    }

    if size - copied >= 2 {
        for _ in (0..(size - copied)).step_by(2) {
            ASM::mov_reg16_mem16_offset32(buf, tmp_reg, ptr_reg, read_offset + copied);
            ASM::mov_base32_reg16(buf, base_offset + copied, tmp_reg);

            copied += 2;
        }
    }

    if size - copied >= 1 {
        for _ in (0..(size - copied)).step_by(1) {
            ASM::mov_reg8_mem8_offset32(buf, tmp_reg, ptr_reg, read_offset + copied);
            ASM::mov_base32_reg8(buf, base_offset + copied, tmp_reg);

            copied += 1;
        }
    }
}

struct X64_64SystemVStoreArgs {
    general_i: usize,
    float_i: usize,
    tmp_stack_offset: i32,
}

impl X64_64SystemVStoreArgs {
    const GENERAL_PARAM_REGS: &'static [X86_64GeneralReg] = X86_64SystemV::GENERAL_PARAM_REGS;
    const GENERAL_RETURN_REGS: &'static [X86_64GeneralReg] = X86_64SystemV::GENERAL_RETURN_REGS;

    const FLOAT_PARAM_REGS: &'static [X86_64FloatReg] = X86_64SystemV::FLOAT_PARAM_REGS;
    const FLOAT_RETURN_REGS: &'static [X86_64FloatReg] = X86_64SystemV::FLOAT_RETURN_REGS;

    fn store_arg<'a>(
        &mut self,
        buf: &mut Vec<'a, u8>,
        storage_manager: &mut X86_64StorageManager<'a, '_, X86_64SystemV>,
        layout_interner: &mut STLayoutInterner<'a>,
        sym: Symbol,
        in_layout: InLayout<'a>,
    ) {
        type ASM = X86_64Assembler;

        // we use the return register as a temporary register; it will be overwritten anyway
        let tmp_reg = Self::GENERAL_RETURN_REGS[0];

        match layout_interner.get_repr(in_layout) {
            single_register_integers!() => self.store_arg_general(buf, storage_manager, sym),
            pointer_layouts!() => self.store_arg_general(buf, storage_manager, sym),
            single_register_floats!() => self.store_arg_float(buf, storage_manager, sym),
            LayoutRepr::I128 | LayoutRepr::U128 | LayoutRepr::DEC => {
                let (offset, _) = storage_manager.stack_offset_and_size(&sym);

                if self.general_i + 1 < Self::GENERAL_PARAM_REGS.len() {
                    let reg1 = Self::GENERAL_PARAM_REGS[self.general_i];
                    let reg2 = Self::GENERAL_PARAM_REGS[self.general_i + 1];

                    ASM::mov_reg64_base32(buf, reg1, offset);
                    ASM::mov_reg64_base32(buf, reg2, offset + 8);

                    self.general_i += 2;
                } else {
                    // Copy to stack using return reg as buffer.
                    let reg = Self::GENERAL_RETURN_REGS[0];

                    ASM::mov_reg64_base32(buf, reg, offset);
                    ASM::mov_stack32_reg64(buf, self.tmp_stack_offset, reg);

                    ASM::mov_reg64_base32(buf, reg, offset + 8);
                    ASM::mov_stack32_reg64(buf, self.tmp_stack_offset + 8, reg);

                    self.tmp_stack_offset += 16;
                }
            }
            _ if layout_interner.stack_size(in_layout) == 0 => {}
            _ if layout_interner.stack_size(in_layout) > 16 => {
                // TODO: Double check this.
                // Just copy onto the stack.
                let stack_offset = self.tmp_stack_offset;

                let size =
                    copy_symbol_to_stack_offset(buf, storage_manager, sym, tmp_reg, stack_offset);

                self.tmp_stack_offset += size as i32;
            }
            LayoutRepr::LambdaSet(lambda_set) => self.store_arg(
                buf,
                storage_manager,
                layout_interner,
                sym,
                lambda_set.runtime_representation(),
            ),
            LayoutRepr::Struct { .. } => {
                let stack_size = layout_interner.stack_size(in_layout);
                if stack_size <= 8 {
                    self.store_arg_64bit(buf, storage_manager, sym);
                } else if stack_size <= 16 {
                    self.store_arg_128bit(buf, storage_manager, sym);
                } else {
                    unreachable!("covered by earlier branch");
                }
            }
            LayoutRepr::Union(UnionLayout::NonRecursive(_)) => {
                let stack_offset = self.tmp_stack_offset;

                let size =
                    copy_symbol_to_stack_offset(buf, storage_manager, sym, tmp_reg, stack_offset);

                self.tmp_stack_offset += size as i32;
            }
            _ => {
                todo!(
                    "calling with arg type, {:?}",
                    layout_interner.dbg(in_layout)
                );
            }
        }
    }

    fn store_arg_general<'a>(
        &mut self,
        buf: &mut Vec<'a, u8>,
        storage_manager: &mut X86_64StorageManager<'a, '_, X86_64SystemV>,
        sym: Symbol,
    ) {
        match Self::GENERAL_PARAM_REGS.get(self.general_i) {
            Some(reg) => {
                storage_manager.load_to_specified_general_reg(buf, &sym, *reg);
                self.general_i += 1;
            }
            None => {
                // Copy to stack using return reg as buffer.
                let tmp = Self::GENERAL_RETURN_REGS[0];

                storage_manager.load_to_specified_general_reg(buf, &sym, tmp);
                X86_64Assembler::mov_stack32_reg64(buf, self.tmp_stack_offset, tmp);

                self.tmp_stack_offset += 8;
            }
        }
    }

    fn store_arg_float<'a>(
        &mut self,
        buf: &mut Vec<'a, u8>,
        storage_manager: &mut X86_64StorageManager<'a, '_, X86_64SystemV>,
        sym: Symbol,
    ) {
        match Self::FLOAT_PARAM_REGS.get(self.float_i) {
            Some(reg) => {
                storage_manager.load_to_specified_float_reg(buf, &sym, *reg);
                self.float_i += 1;
            }
            None => {
                // Copy to stack using return reg as buffer.
                let tmp = Self::FLOAT_RETURN_REGS[0];

                storage_manager.load_to_specified_float_reg(buf, &sym, tmp);
                X86_64Assembler::mov_stack32_freg64(buf, self.tmp_stack_offset, tmp);

                self.tmp_stack_offset += 8;
            }
        }
    }

    fn store_arg_64bit<'a>(
        &mut self,
        buf: &mut Vec<'a, u8>,
        storage_manager: &mut X86_64StorageManager<'a, '_, X86_64SystemV>,
        sym: Symbol,
    ) {
        type ASM = X86_64Assembler;

        let (offset, _) = storage_manager.stack_offset_and_size(&sym);

        match Self::GENERAL_PARAM_REGS.get(self.general_i) {
            Some(reg) => {
                ASM::mov_reg64_base32(buf, *reg, offset);

                self.general_i += 1;
            }
            None => {
                // Copy to stack using return reg as buffer.
                let reg = X86_64GeneralReg::RAX;

                ASM::mov_reg64_base32(buf, reg, offset);
                ASM::mov_stack32_reg64(buf, self.tmp_stack_offset, reg);

                self.tmp_stack_offset += 8;
            }
        }
    }

    fn store_arg_128bit<'a>(
        &mut self,
        buf: &mut Vec<'a, u8>,
        storage_manager: &mut X86_64StorageManager<'a, '_, X86_64SystemV>,
        sym: Symbol,
    ) {
        type ASM = X86_64Assembler;

        let (offset, _) = storage_manager.stack_offset_and_size(&sym);

        if self.general_i + 1 < Self::GENERAL_PARAM_REGS.len() {
            let reg1 = Self::GENERAL_PARAM_REGS[self.general_i];
            let reg2 = Self::GENERAL_PARAM_REGS[self.general_i + 1];

            ASM::mov_reg64_base32(buf, reg1, offset);
            ASM::mov_reg64_base32(buf, reg2, offset + 8);

            self.general_i += 2;
        } else {
            // Copy to stack using return reg as buffer.
            let reg = X86_64GeneralReg::RAX;

            ASM::mov_reg64_base32(buf, reg, offset);
            ASM::mov_stack32_reg64(buf, self.tmp_stack_offset, reg);

            ASM::mov_reg64_base32(buf, reg, offset + 8);
            ASM::mov_stack32_reg64(buf, self.tmp_stack_offset + 8, reg);

            self.tmp_stack_offset += 16;
        }
    }
}

struct X64_64WindowsFastCallStoreArgs {
    general_i: usize,
    float_i: usize,
    tmp_stack_offset: i32,
}

impl X64_64WindowsFastCallStoreArgs {
    const GENERAL_PARAM_REGS: &'static [X86_64GeneralReg] =
        X86_64WindowsFastcall::GENERAL_PARAM_REGS;
    const GENERAL_RETURN_REGS: &'static [X86_64GeneralReg] =
        X86_64WindowsFastcall::GENERAL_RETURN_REGS;

    const FLOAT_PARAM_REGS: &'static [X86_64FloatReg] = X86_64WindowsFastcall::FLOAT_PARAM_REGS;
    const FLOAT_RETURN_REGS: &'static [X86_64FloatReg] = X86_64WindowsFastcall::FLOAT_RETURN_REGS;

    fn store_arg<'a>(
        &mut self,
        buf: &mut Vec<'a, u8>,
        storage_manager: &mut X86_64StorageManager<'a, '_, X86_64WindowsFastcall>,
        layout_interner: &mut STLayoutInterner<'a>,
        sym: Symbol,
        in_layout: InLayout<'a>,
    ) {
        type ASM = X86_64Assembler;

        // we use the return register as a temporary register; it will be overwritten anyway
        let tmp_reg = Self::GENERAL_RETURN_REGS[0];

        match layout_interner.get_repr(in_layout) {
            single_register_integers!() => self.store_arg_general(buf, storage_manager, sym),
            pointer_layouts!() => self.store_arg_general(buf, storage_manager, sym),
            single_register_floats!() => self.store_arg_float(buf, storage_manager, sym),
            LayoutRepr::I128 | LayoutRepr::U128 => {
                let (offset, _) = storage_manager.stack_offset_and_size(&sym);

                if self.general_i + 1 < Self::GENERAL_PARAM_REGS.len() {
                    let reg1 = Self::GENERAL_PARAM_REGS[self.general_i];
                    let reg2 = Self::GENERAL_PARAM_REGS[self.general_i + 1];

                    ASM::mov_reg64_base32(buf, reg1, offset);
                    ASM::mov_reg64_base32(buf, reg2, offset + 8);

                    self.general_i += 2;
                } else {
                    // Copy to stack using return reg as buffer.
                    let reg = Self::GENERAL_RETURN_REGS[0];

                    ASM::mov_reg64_base32(buf, reg, offset);
                    ASM::mov_stack32_reg64(buf, self.tmp_stack_offset, reg);

                    ASM::mov_reg64_base32(buf, reg, offset + 8);
                    ASM::mov_stack32_reg64(buf, self.tmp_stack_offset + 8, reg);

                    self.tmp_stack_offset += 16;
                }
            }
            _ if layout_interner.stack_size(in_layout) == 0 => {}
            _ if layout_interner.stack_size(in_layout) > 16 => {
                // Reference: https://learn.microsoft.com/en-us/cpp/build/x64-calling-convention?view=msvc-170#parameter-passing
                match Self::GENERAL_PARAM_REGS.get(self.general_i) {
                    Some(reg) => {
                        // if there is a general purpose register available, use it to store a pointer to the value
                        let (base_offset, _size) = storage_manager.stack_offset_and_size(&sym);

                        ASM::add_reg64_reg64_imm32(buf, *reg, X86_64GeneralReg::RBP, base_offset);

                        self.general_i += 1;
                    }
                    None => {
                        // else, pass the value implicitly by copying to the stack (of the new frame)
                        let stack_offset = self.tmp_stack_offset;

                        let size = copy_symbol_to_stack_offset(
                            buf,
                            storage_manager,
                            sym,
                            tmp_reg,
                            stack_offset,
                        );

                        self.tmp_stack_offset += size as i32;
                    }
                }
            }
            LayoutRepr::LambdaSet(lambda_set) => self.store_arg(
                buf,
                storage_manager,
                layout_interner,
                sym,
                lambda_set.runtime_representation(),
            ),
            LayoutRepr::Struct { .. } => {
                // for now, just also store this on the stack
                let stack_offset = self.tmp_stack_offset;

                let size =
                    copy_symbol_to_stack_offset(buf, storage_manager, sym, tmp_reg, stack_offset);

                self.tmp_stack_offset += size as i32;
            }
            LayoutRepr::Union(UnionLayout::NonRecursive(_)) => {
                let stack_offset = self.tmp_stack_offset;

                let size =
                    copy_symbol_to_stack_offset(buf, storage_manager, sym, tmp_reg, stack_offset);

                self.tmp_stack_offset += size as i32;
            }
            _ => {
                todo!(
                    "calling with arg type, {:?}",
                    layout_interner.dbg(in_layout)
                );
            }
        }
    }

    fn store_arg_general<'a>(
        &mut self,
        buf: &mut Vec<'a, u8>,
        storage_manager: &mut X86_64StorageManager<'a, '_, X86_64WindowsFastcall>,
        sym: Symbol,
    ) {
        match Self::GENERAL_PARAM_REGS.get(self.general_i) {
            Some(reg) => {
                storage_manager.load_to_specified_general_reg(buf, &sym, *reg);
                self.general_i += 1;
            }
            None => {
                // Copy to stack using return reg as buffer.
                let tmp = Self::GENERAL_RETURN_REGS[0];

                storage_manager.load_to_specified_general_reg(buf, &sym, tmp);
                X86_64Assembler::mov_stack32_reg64(buf, self.tmp_stack_offset, tmp);

                self.tmp_stack_offset += 8;
            }
        }
    }

    fn store_arg_float<'a>(
        &mut self,
        buf: &mut Vec<'a, u8>,
        storage_manager: &mut X86_64StorageManager<'a, '_, X86_64WindowsFastcall>,
        sym: Symbol,
    ) {
        match Self::FLOAT_PARAM_REGS.get(self.float_i) {
            Some(reg) => {
                storage_manager.load_to_specified_float_reg(buf, &sym, *reg);
                self.float_i += 1;
            }
            None => {
                // Copy to stack using return reg as buffer.
                let tmp = Self::FLOAT_RETURN_REGS[0];

                storage_manager.load_to_specified_float_reg(buf, &sym, tmp);
                X86_64Assembler::mov_stack32_freg64(buf, self.tmp_stack_offset, tmp);

                self.tmp_stack_offset += 8;
            }
        }
    }
}

type X86_64StorageManager<'a, 'r, CallConv> =
    StorageManager<'a, 'r, X86_64GeneralReg, X86_64FloatReg, X86_64Assembler, CallConv>;

struct X64_64SystemVLoadArgs {
    general_i: usize,
    float_i: usize,
    argument_offset: i32,
}

impl X64_64SystemVLoadArgs {
    fn load_arg<'a>(
        &mut self,
        buf: &mut Vec<u8>,
        storage_manager: &mut X86_64StorageManager<'a, '_, X86_64SystemV>,
        layout_interner: &mut STLayoutInterner<'a>,
        sym: Symbol,
        in_layout: InLayout<'a>,
    ) {
        let stack_size = layout_interner.stack_size(in_layout);
        match layout_interner.get_repr(in_layout) {
            single_register_integers!() => self.load_arg_general(storage_manager, sym),
            pointer_layouts!() => self.load_arg_general(storage_manager, sym),
            single_register_floats!() => self.load_arg_float(storage_manager, sym),
            _ if stack_size == 0 => {
                storage_manager.no_data(&sym);
            }
            _ if stack_size > 16 => {
                // TODO: Double check this.
                storage_manager.complex_stack_arg(&sym, self.argument_offset, stack_size);
                self.argument_offset += stack_size as i32;
            }
            LayoutRepr::LambdaSet(lambda_set) => self.load_arg(
                buf,
                storage_manager,
                layout_interner,
                sym,
                lambda_set.runtime_representation(),
            ),
            LayoutRepr::Struct { .. } => {
                if stack_size <= 8 {
                    self.load_arg_general_64bit(
                        buf,
                        storage_manager,
                        layout_interner,
                        sym,
                        in_layout,
                    );
                } else if stack_size <= 16 {
                    self.load_arg_general_128bit(
                        buf,
                        storage_manager,
                        layout_interner,
                        sym,
                        in_layout,
                    );
                } else {
                    unreachable!("covered by an earlier branch")
                }
            }
            LayoutRepr::Builtin(Builtin::Int(IntWidth::U128 | IntWidth::I128)) => {
                self.load_arg_general_128bit(buf, storage_manager, layout_interner, sym, in_layout);
            }
            LayoutRepr::Builtin(Builtin::Decimal) => {
                self.load_arg_general_128bit(buf, storage_manager, layout_interner, sym, in_layout);
            }
            LayoutRepr::Union(UnionLayout::NonRecursive(_)) => {
                // for now, just also store this on the stack
                storage_manager.complex_stack_arg(&sym, self.argument_offset, stack_size);
                self.argument_offset += stack_size as i32;
            }
            _ => {
                todo!(
                    "Loading args with layout {:?}",
                    layout_interner.dbg(in_layout)
                );
            }
        }
    }

    fn load_arg_general(
        &mut self,
        storage_manager: &mut X86_64StorageManager<'_, '_, X86_64SystemV>,
        sym: Symbol,
    ) {
        if let Some(reg) = X86_64SystemV::GENERAL_PARAM_REGS.get(self.general_i) {
            storage_manager.general_reg_arg(&sym, *reg);
            self.general_i += 1;
        } else {
            storage_manager.primitive_stack_arg(&sym, self.argument_offset);
            self.argument_offset += 8;
        }
    }

    fn load_arg_general_64bit(
        &mut self,
        buf: &mut Vec<u8>,
        storage_manager: &mut X86_64StorageManager<'_, '_, X86_64SystemV>,
        layout_interner: &mut STLayoutInterner<'_>,
        sym: Symbol,
        in_layout: InLayout<'_>,
    ) {
        type ASM = X86_64Assembler;

        let reg1 = X86_64SystemV::GENERAL_PARAM_REGS.get(self.general_i);

        match reg1 {
            Some(reg1) => {
                let offset =
                    storage_manager.claim_stack_area_layout(layout_interner, sym, in_layout);

                ASM::mov_base32_reg64(buf, offset, *reg1);

                self.general_i += 1;
            }
            None => {
                storage_manager.complex_stack_arg(&sym, self.argument_offset, 8);
                self.argument_offset += 8;
            }
        }
    }

    fn load_arg_general_128bit(
        &mut self,
        buf: &mut Vec<u8>,
        storage_manager: &mut X86_64StorageManager<'_, '_, X86_64SystemV>,
        layout_interner: &mut STLayoutInterner<'_>,
        sym: Symbol,
        in_layout: InLayout<'_>,
    ) {
        type ASM = X86_64Assembler;

        let reg1 = X86_64SystemV::GENERAL_PARAM_REGS.get(self.general_i);
        let reg2 = X86_64SystemV::GENERAL_PARAM_REGS.get(self.general_i + 1);

        match (reg1, reg2) {
            (Some(reg1), Some(reg2)) => {
                let offset =
                    storage_manager.claim_stack_area_layout(layout_interner, sym, in_layout);

                ASM::mov_base32_reg64(buf, offset, *reg1);
                ASM::mov_base32_reg64(buf, offset + 8, *reg2);

                self.general_i += 2;
            }
            _ => {
                storage_manager.complex_stack_arg(&sym, self.argument_offset, 16);
                self.argument_offset += 16;
            }
        }
    }

    fn load_arg_float(
        &mut self,
        storage_manager: &mut X86_64StorageManager<'_, '_, X86_64SystemV>,
        sym: Symbol,
    ) {
        if let Some(reg) = X86_64SystemV::FLOAT_PARAM_REGS.get(self.float_i) {
            storage_manager.float_reg_arg(&sym, *reg);
            self.float_i += 1;
        } else {
            storage_manager.primitive_stack_arg(&sym, self.argument_offset);
            self.argument_offset += 8;
        }
    }
}

struct X64_64WindowsFastCallLoadArgs {
    general_i: usize,
    float_i: usize,
    argument_offset: i32,
}

impl X64_64WindowsFastCallLoadArgs {
    fn load_arg<'a>(
        &mut self,
        buf: &mut Vec<'a, u8>,
        storage_manager: &mut X86_64StorageManager<'a, '_, X86_64WindowsFastcall>,
        layout_interner: &mut STLayoutInterner<'a>,
        sym: Symbol,
        in_layout: InLayout<'a>,
    ) {
        type ASM = X86_64Assembler;

        let stack_size = layout_interner.stack_size(in_layout);
        match layout_interner.get_repr(in_layout) {
            single_register_integers!() => self.load_arg_general(storage_manager, sym),
            pointer_layouts!() => self.load_arg_general(storage_manager, sym),
            single_register_floats!() => self.load_arg_float(storage_manager, sym),
            _ if stack_size == 0 => {
                storage_manager.no_data(&sym);
            }
            _ if stack_size > 16 => {
                // Reference: https://learn.microsoft.com/en-us/cpp/build/x64-calling-convention?view=msvc-170#parameter-passing
                match X86_64WindowsFastcall::GENERAL_PARAM_REGS.get(self.general_i) {
                    Some(ptr_reg) => {
                        // if there is a general purpose register available, use it to store a pointer to the value
                        let base_offset = storage_manager.claim_stack_area_layout(
                            layout_interner,
                            sym,
                            in_layout,
                        );
                        let tmp_reg = X86_64WindowsFastcall::GENERAL_RETURN_REGS[0];

                        copy_to_base_offset::<_, _, ASM>(
                            buf,
                            base_offset,
                            stack_size,
                            *ptr_reg,
                            tmp_reg,
                            0,
                        );

                        self.general_i += 1;
                    }
                    None => {
                        // else, pass the value implicitly by copying to the stack (of the new frame)
                        storage_manager.complex_stack_arg(&sym, self.argument_offset, stack_size);
                        self.argument_offset += stack_size as i32;
                    }
                }
            }
            LayoutRepr::LambdaSet(lambda_set) => self.load_arg(
                buf,
                storage_manager,
                layout_interner,
                sym,
                lambda_set.runtime_representation(),
            ),
            LayoutRepr::Struct { .. } => {
                // for now, just also store this on the stack
                storage_manager.complex_stack_arg(&sym, self.argument_offset, stack_size);
                self.argument_offset += stack_size as i32;
            }
            LayoutRepr::Builtin(Builtin::Int(IntWidth::U128 | IntWidth::I128)) => {
                self.load_arg_general_128bit(buf, storage_manager, sym);
            }
            LayoutRepr::Builtin(Builtin::Decimal) => {
                self.load_arg_general_128bit(buf, storage_manager, sym);
            }
            LayoutRepr::Union(UnionLayout::NonRecursive(_)) => {
                // for now, just also store this on the stack
                storage_manager.complex_stack_arg(&sym, self.argument_offset, stack_size);
                self.argument_offset += stack_size as i32;
            }
            _ => {
                todo!(
                    "Loading args with layout {:?}",
                    layout_interner.dbg(in_layout)
                );
            }
        }
    }

    fn load_arg_general(
        &mut self,
        storage_manager: &mut X86_64StorageManager<'_, '_, X86_64WindowsFastcall>,
        sym: Symbol,
    ) {
        if let Some(reg) = X86_64WindowsFastcall::GENERAL_PARAM_REGS.get(self.general_i) {
            storage_manager.general_reg_arg(&sym, *reg);
            self.general_i += 1;
        } else {
            storage_manager.primitive_stack_arg(&sym, self.argument_offset);
            self.argument_offset += 8;
        }
    }

    fn load_arg_general_128bit(
        &mut self,
        buf: &mut Vec<u8>,
        storage_manager: &mut X86_64StorageManager<'_, '_, X86_64WindowsFastcall>,
        sym: Symbol,
    ) {
        type ASM = X86_64Assembler;

        let reg1 = X86_64WindowsFastcall::GENERAL_PARAM_REGS.get(self.general_i);
        let reg2 = X86_64WindowsFastcall::GENERAL_PARAM_REGS.get(self.general_i + 1);

        match (reg1, reg2) {
            (Some(reg1), Some(reg2)) => {
                let offset = storage_manager.claim_stack_area_with_alignment(sym, 16, 16);

                ASM::mov_base32_reg64(buf, offset, *reg1);
                ASM::mov_base32_reg64(buf, offset + 8, *reg2);

                self.general_i += 2;
            }
            _ => {
                storage_manager.complex_stack_arg(&sym, self.argument_offset, 16);
                self.argument_offset += 16;
            }
        }
    }

    fn load_arg_float(
        &mut self,
        storage_manager: &mut X86_64StorageManager<'_, '_, X86_64WindowsFastcall>,
        sym: Symbol,
    ) {
        if let Some(reg) = X86_64WindowsFastcall::FLOAT_PARAM_REGS.get(self.float_i) {
            storage_manager.float_reg_arg(&sym, *reg);
            self.float_i += 1;
        } else {
            storage_manager.primitive_stack_arg(&sym, self.argument_offset);
            self.argument_offset += 8;
        }
    }
}

impl X86_64SystemV {
    fn returns_via_arg_pointer<'a>(
        interner: &STLayoutInterner<'a>,
        ret_layout: &InLayout<'a>,
    ) -> bool {
        // TODO: This will need to be more complex/extended to fully support the calling convention.
        // details here: https://github.com/hjl-tools/x86-psABI/wiki/x86-64-psABI-1.0.pdf
        interner.stack_size(*ret_layout) > 16
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

    // These are registers that a called function must save and restore if it wants to use them.
    //
    // Refer https://learn.microsoft.com/en-us/cpp/build/x64-calling-convention?view=msvc-170#callercallee-saved-registers
    // > The x64 ABI considers registers RBX, RBP, RDI, RSI, RSP, R12, R13, R14, R15, and XMM6-XMM15 nonvolatile.
    // > They must be saved and restored by a function that uses them.
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

    // Refer https://learn.microsoft.com/en-us/cpp/build/x64-calling-convention?view=msvc-170#callercallee-saved-registers
    // > The x64 ABI considers registers RBX, RBP, RDI, RSI, RSP, R12, R13, R14, R15, and XMM6-XMM15 nonvolatile.
    // > They must be saved and restored by a function that uses them.
    #[inline(always)]
    fn float_callee_saved(reg: &X86_64FloatReg) -> bool {
        matches!(
            reg,
            X86_64FloatReg::XMM6
                | X86_64FloatReg::XMM7
                | X86_64FloatReg::XMM8
                | X86_64FloatReg::XMM9
                | X86_64FloatReg::XMM10
                | X86_64FloatReg::XMM11
                | X86_64FloatReg::XMM12
                | X86_64FloatReg::XMM13
                | X86_64FloatReg::XMM14
                | X86_64FloatReg::XMM15
        )
    }

    #[inline(always)]
    fn setup_stack(
        buf: &mut Vec<'_, u8>,
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
    fn cleanup_stack(
        buf: &mut Vec<'_, u8>,
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
        storage_manager: &mut X86_64StorageManager<'a, '_, X86_64WindowsFastcall>,
        layout_interner: &mut STLayoutInterner<'a>,
        args: &'a [(InLayout<'a>, Symbol)],
        ret_layout: &InLayout<'a>,
    ) {
        let returns_via_pointer =
            X86_64WindowsFastcall::returns_via_arg_pointer(layout_interner, ret_layout);

        let mut state = X64_64WindowsFastCallLoadArgs {
            general_i: usize::from(returns_via_pointer),
            float_i: 0,
            // 16 is the size of the pushed return address and base pointer.
            argument_offset: X86_64WindowsFastcall::SHADOW_SPACE_SIZE as i32 + 16,
        };

        if returns_via_pointer {
            storage_manager.ret_pointer_arg(X86_64WindowsFastcall::GENERAL_PARAM_REGS[0]);
        }

        for (in_layout, sym) in args.iter() {
            state.load_arg(buf, storage_manager, layout_interner, *sym, *in_layout);
        }
    }

    #[inline(always)]
    fn store_args<'a>(
        buf: &mut Vec<'a, u8>,
        storage_manager: &mut StorageManager<
            'a,
            '_,
            X86_64GeneralReg,
            X86_64FloatReg,
            X86_64Assembler,
            X86_64WindowsFastcall,
        >,
        layout_interner: &mut STLayoutInterner<'a>,
        dst: &Symbol,
        args: &[Symbol],
        arg_layouts: &[InLayout<'a>],
        ret_layout: &InLayout<'a>,
    ) {
        let mut general_i = 0;

        if Self::returns_via_arg_pointer(layout_interner, ret_layout) {
            // Save space on the stack for the result we will be return.
            let base_offset =
                storage_manager.claim_stack_area_layout(layout_interner, *dst, *ret_layout);

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

        let mut state = X64_64WindowsFastCallStoreArgs {
            general_i,
            float_i: 0,
            tmp_stack_offset: Self::SHADOW_SPACE_SIZE as i32,
        };

        for (sym, in_layout) in args.iter().zip(arg_layouts.iter()) {
            state.store_arg(buf, storage_manager, layout_interner, *sym, *in_layout);
        }

        storage_manager.update_fn_call_stack_size(state.tmp_stack_offset as u32);
    }

    fn return_complex_symbol<'a>(
        buf: &mut Vec<'a, u8>,
        storage_manager: &mut StorageManager<
            'a,
            '_,
            X86_64GeneralReg,
            X86_64FloatReg,
            X86_64Assembler,
            X86_64WindowsFastcall,
        >,
        layout_interner: &mut STLayoutInterner<'a>,
        sym: &Symbol,
        layout: &InLayout<'a>,
    ) {
        match layout_interner.get_repr(*layout) {
            single_register_layouts!() => {
                internal_error!("single register layouts are not complex symbols");
            }
            // For windows (and zig 0.9 changes in zig 0.10) we need to match what zig does,
            // in this case uses RAX & RDX to return the value
            LayoutRepr::I128 | LayoutRepr::U128 => {
                let (base_offset, size) = storage_manager.stack_offset_and_size(sym);
                debug_assert_eq!(base_offset % 8, 0);
                debug_assert_eq!(size, 16);

                X86_64Assembler::mov_reg64_base32(buf, X86_64GeneralReg::RAX, base_offset);
                X86_64Assembler::mov_reg64_base32(buf, X86_64GeneralReg::RDX, base_offset + 0x08);
            }
            _ if layout_interner.stack_size(*layout) == 0 => {}
            _ if !Self::returns_via_arg_pointer(layout_interner, layout) => {
                let (base_offset, size) = storage_manager.stack_offset_and_size(sym);
                debug_assert_eq!(base_offset % 8, 0);
                if size <= 8 {
                    X86_64Assembler::mov_reg64_base32(
                        buf,
                        Self::GENERAL_RETURN_REGS[0],
                        base_offset,
                    );
                } else {
                    internal_error!(
                        "types that don't return via arg pointer must be less than 8 bytes"
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
            '_,
            X86_64GeneralReg,
            X86_64FloatReg,
            X86_64Assembler,
            X86_64WindowsFastcall,
        >,
        layout_interner: &mut STLayoutInterner<'a>,
        sym: &Symbol,
        layout: &InLayout<'a>,
    ) {
        match layout_interner.get_repr(*layout) {
            single_register_layouts!() => {
                internal_error!("single register layouts are not complex symbols");
            }
            // For windows (and zig 0.9 changes in zig 0.10) we need to match what zig does,
            // in this case uses RAX & RDX to return the value
            LayoutRepr::I128 | LayoutRepr::U128 => {
                let offset =
                    storage_manager.claim_stack_area_layout(layout_interner, *sym, *layout);
                X86_64Assembler::mov_base32_reg64(buf, offset, X86_64GeneralReg::RAX);
                X86_64Assembler::mov_base32_reg64(buf, offset + 0x08, X86_64GeneralReg::RDX);
            }
            _ if layout_interner.stack_size(*layout) == 0 => {
                storage_manager.no_data(sym);
            }
            _ if !Self::returns_via_arg_pointer(layout_interner, layout) => {
                let size = layout_interner.stack_size(*layout);
                let offset =
                    storage_manager.claim_stack_area_layout(layout_interner, *sym, *layout);
                if size <= 8 {
                    X86_64Assembler::mov_base32_reg64(buf, offset, Self::GENERAL_RETURN_REGS[0]);
                } else {
                    internal_error!(
                        "types that don't return via arg pointer must be less than 8 bytes"
                    );
                }
            }
            _ => {
                // This should have been received via an arg pointer.
                // That means the value is already loaded onto the stack area we allocated before the call.
                // Nothing to do.
            }
        }
    }

    fn setjmp(buf: &mut Vec<'_, u8>) {
        use X86_64GeneralReg::*;
        type ASM = X86_64Assembler;

        // input:
        //
        // rcx: pointer to the jmp_buf
        // rdx: stack pointer

        // mingw_getsp:
        //     lea rax [ rsp + 8 ]
        //     ret
        //
        // _setjmp:
        //     mov [rcx + 0x00] rdx
        //     mov [rcx + 0x08] rbx
        //     mov [rcx + 0x18] rbp # note 0x10 is not used yet!
        //     mov [rcx + 0x20] rsi
        //     mov [rcx + 0x28] rdi
        //     mov [rcx + 0x30] r12
        //     mov [rcx + 0x38] r13
        //     mov [rcx + 0x40] r14
        //     mov [rcx + 0x48] r15
        //     lea r8 [rsp + 0x08]
        //     mov [rcx + 0x10] r8
        //     mov r8 [rsp]
        //     mov [rcx + 0x50] r8
        //
        //     stmxcsr [rcx + 0x58]
        //     fnstcw word ptr [rcx + 0x5C]
        //
        //     mobdxq xmmword ptr [rcx + 0x60], xmm6
        //     mobdxq xmmword ptr [rcx + 0x70], xmm7
        //     mobdxq xmmword ptr [rcx + 0x80], xmm8
        //     mobdxq xmmword ptr [rcx + 0x90], xmm9
        //     mobdxq xmmword ptr [rcx + 0xa0], xmm10
        //     mobdxq xmmword ptr [rcx + 0xb0], xmm11
        //     mobdxq xmmword ptr [rcx + 0xc0], xmm12
        //     mobdxq xmmword ptr [rcx + 0xd0], xmm13
        //     mobdxq xmmword ptr [rcx + 0xe0], xmm14
        //     mobdxq xmmword ptr [rcx + 0xf0], xmm15
        //
        //     xor eax, eax
        //     ret

        let result_pointer = RCX;
        let env = RDX;
        debug_assert_eq!(env, Self::GENERAL_PARAM_REGS[1]);

        ASM::mov_mem64_offset32_reg64(buf, env, 0x00, RDX);
        ASM::mov_mem64_offset32_reg64(buf, env, 0x08, RBX);
        // NOTE: 0x10 is unused here!
        ASM::mov_mem64_offset32_reg64(buf, env, 0x18, RBP);
        ASM::mov_mem64_offset32_reg64(buf, env, 0x20, RSI);
        ASM::mov_mem64_offset32_reg64(buf, env, 0x28, RDI);
        ASM::mov_mem64_offset32_reg64(buf, env, 0x30, R12);
        ASM::mov_mem64_offset32_reg64(buf, env, 0x38, R13);
        ASM::mov_mem64_offset32_reg64(buf, env, 0x40, R14);
        ASM::mov_mem64_offset32_reg64(buf, env, 0x48, R15);

        // go one value up (as if setjmp wasn't called)
        lea_reg64_offset8(buf, R8, RSP, 0x8);
        ASM::mov_mem64_offset32_reg64(buf, env, 0x10, R8);

        // store the current stack pointer
        ASM::mov_reg64_mem64_offset32(buf, R8, RSP, 0);
        ASM::mov_mem64_offset32_reg64(buf, env, 0x50, R8);

        // zero out the fields of the result pointer
        ASM::mov_reg64_imm64(buf, R8, 0x00);
        ASM::mov_mem64_offset32_reg64(buf, result_pointer, 0x00, R8);
        ASM::mov_mem64_offset32_reg64(buf, result_pointer, 0x08, R8);

        // now the windows implementation goes on to store xmm registers and sse2 stuff.
        // we skip that for now

        // store the result pointer into the env so that longjmp can retrieve it
        ASM::mov_mem64_offset32_reg64(buf, env, 0x58, result_pointer);

        ASM::ret(buf)
    }

    fn longjmp(_buf: &mut Vec<'_, u8>) {
        // do nothing, longjmp is part of roc_panic
    }

    fn roc_panic(buf: &mut Vec<'_, u8>, relocs: &mut Vec<'_, Relocation>) {
        use X86_64GeneralReg::*;
        type ASM = X86_64Assembler;

        // a *const RocStr
        let roc_str_ptr = RCX;
        debug_assert_eq!(roc_str_ptr, Self::GENERAL_PARAM_REGS[0]);

        // a 32-bit integer
        let panic_tag = RDX;
        debug_assert_eq!(panic_tag, Self::GENERAL_PARAM_REGS[1]);

        // move the crash tag into a temporary register. We add 1 to it because the 0 value
        // is already used for "no crash occurred"
        ASM::add_reg64_reg64_imm32(buf, R10, panic_tag, 0x01);

        // the setlongjmp_buffer
        let env = R8;
        ASM::data_pointer(buf, relocs, String::from("setlongjmp_buffer"), env);

        // move the roc_str bytes into the setlongjmp_buffer
        for offset in [0, 8, 16] {
            ASM::mov_reg64_mem64_offset32(buf, R9, roc_str_ptr, offset);
            ASM::mov_mem64_offset32_reg64(buf, env, 0x60 + offset, R9);
        }

        // now, time to move all the registers back to how they were
        ASM::mov_reg64_mem64_offset32(buf, RDX, env, 0x00);
        ASM::mov_reg64_mem64_offset32(buf, RBX, env, 0x08);
        // again 0x10 is skipped here
        ASM::mov_reg64_mem64_offset32(buf, RBP, env, 0x18);
        ASM::mov_reg64_mem64_offset32(buf, RSI, env, 0x20);
        ASM::mov_reg64_mem64_offset32(buf, RDI, env, 0x28);
        ASM::mov_reg64_mem64_offset32(buf, R12, env, 0x30);
        ASM::mov_reg64_mem64_offset32(buf, R13, env, 0x38);
        ASM::mov_reg64_mem64_offset32(buf, R14, env, 0x40);
        ASM::mov_reg64_mem64_offset32(buf, R15, env, 0x48);

        // value of rsp before setjmp call
        ASM::mov_reg64_mem64_offset32(buf, RSP, env, 0x10);

        // set up the return values. The windows fastcall calling convention has only one return
        // register, and we need to return two values, so we use some space in the setlongjmp_buffer
        let result_pointer = R9;
        ASM::mov_reg64_mem64_offset32(buf, result_pointer, env, 0x58);

        // a pointer to the error message
        ASM::add_reg64_reg64_imm32(buf, R11, env, 0x60);

        // write a pointer to the error message into result_pointer
        ASM::mov_mem64_offset32_reg64(buf, result_pointer, 0x00, R11);

        // write the panic tag (now in R10) into the result_pointer
        ASM::mov_mem64_offset32_reg64(buf, result_pointer, 0x08, R10);

        jmp_reg64_offset8(buf, env, 0x50)
    }
}

impl X86_64WindowsFastcall {
    fn returns_via_arg_pointer<'a>(
        interner: &STLayoutInterner<'a>,
        ret_layout: &InLayout<'a>,
    ) -> bool {
        // TODO: This is not fully correct there are some exceptions for "vector" types.
        // details here: https://docs.microsoft.com/en-us/cpp/build/x64-calling-convention?view=msvc-160#return-values
        match *ret_layout {
            Layout::I128 | Layout::U128 => false,
            _ => interner.stack_size(*ret_layout) > 8,
        }
    }
}

#[inline(always)]
fn x86_64_generic_setup_stack(
    buf: &mut Vec<'_, u8>,
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
fn x86_64_generic_cleanup_stack(
    buf: &mut Vec<'_, u8>,
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

type Reg64 = X86_64GeneralReg;

fn binop_move_src_to_dst_reg64<F>(buf: &mut Vec<'_, u8>, f: F, dst: Reg64, src1: Reg64, src2: Reg64)
where
    F: FnOnce(&mut Vec<'_, u8>, X86_64GeneralReg, X86_64GeneralReg),
{
    if dst == src1 {
        f(buf, dst, src2);
    } else if dst == src2 {
        f(buf, dst, src1);
    } else {
        mov_reg64_reg64(buf, dst, src1);
        f(buf, dst, src2);
    }
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
    fn abs_freg32_freg32(
        buf: &mut Vec<'_, u8>,
        relocs: &mut Vec<'_, Relocation>,
        dst: X86_64FloatReg,
        src: X86_64FloatReg,
    ) {
        movss_freg32_rip_offset32(buf, dst, 0);

        // TODO: make sure this constant only loads once instead of every call to abs
        relocs.push(Relocation::LocalData {
            offset: buf.len() as u64 - 4,
            data: 0x7fffffffu64.to_le_bytes().to_vec(),
        });

        andps_freg32_freg32(buf, dst, src);
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
    fn add_reg64_reg64_reg64(buf: &mut Vec<'_, u8>, dst: Reg64, src1: Reg64, src2: Reg64) {
        binop_move_src_to_dst_reg64(buf, add_reg64_reg64, dst, src1, src2)
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
    fn sub_freg32_freg32_freg32(
        buf: &mut Vec<'_, u8>,
        dst: X86_64FloatReg,
        src1: X86_64FloatReg,
        src2: X86_64FloatReg,
    ) {
        if dst == src1 {
            subss_freg32_freg32(buf, dst, src2);
        } else if dst == src2 {
            subss_freg32_freg32(buf, dst, src1);
        } else {
            movss_freg32_freg32(buf, dst, src1);
            subss_freg32_freg32(buf, dst, src2);
        }
    }
    #[inline(always)]
    fn sub_freg64_freg64_freg64(
        buf: &mut Vec<'_, u8>,
        dst: X86_64FloatReg,
        src1: X86_64FloatReg,
        src2: X86_64FloatReg,
    ) {
        if dst == src1 {
            subsd_freg64_freg64(buf, dst, src2);
        } else if dst == src2 {
            subsd_freg64_freg64(buf, dst, src1);
        } else {
            movsd_freg64_freg64(buf, dst, src1);
            subsd_freg64_freg64(buf, dst, src2);
        }
    }

    #[inline(always)]
    fn call(buf: &mut Vec<'_, u8>, relocs: &mut Vec<'_, Relocation>, fn_name: String) {
        buf.extend([0xE8, 0x00, 0x00, 0x00, 0x00]);
        relocs.push(Relocation::LinkedFunction {
            offset: buf.len() as u64 - 4,
            name: fn_name,
        });
    }

    #[inline(always)]
    fn function_pointer(
        buf: &mut Vec<'_, u8>,
        relocs: &mut Vec<'_, Relocation>,
        fn_name: String,
        dst: X86_64GeneralReg,
    ) {
        lea_reg64(buf, dst);

        relocs.push(Relocation::LinkedFunction {
            offset: buf.len() as u64 - 4,
            name: fn_name,
        });
    }

    #[inline(always)]
    fn data_pointer(
        buf: &mut Vec<'_, u8>,
        relocs: &mut Vec<'_, Relocation>,
        fn_name: String,
        dst: X86_64GeneralReg,
    ) {
        lea_reg64(buf, dst);

        relocs.push(Relocation::LinkedData {
            offset: buf.len() as u64 - 4,
            name: fn_name,
        });

        // on X86_64, we actually get a pointer to a pointer
        // so we just dereference to get just a pointer to the data
        X86_64Assembler::mov_reg64_mem64_offset32(buf, dst, dst, 0);
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
        storage_manager: &mut StorageManager<'a, '_, X86_64GeneralReg, X86_64FloatReg, ASM, CC>,
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
        storage_manager: &mut StorageManager<'a, '_, X86_64GeneralReg, X86_64FloatReg, ASM, CC>,
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
        storage_manager: &mut StorageManager<'a, '_, X86_64GeneralReg, X86_64FloatReg, ASM, CC>,
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

    fn irem_reg64_reg64_reg64<'a, ASM, CC>(
        buf: &mut Vec<'a, u8>,
        storage_manager: &mut StorageManager<'a, '_, X86_64GeneralReg, X86_64FloatReg, ASM, CC>,
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
        mov_reg64_reg64(buf, dst, X86_64GeneralReg::RDX);
    }

    fn urem_reg64_reg64_reg64<'a, ASM, CC>(
        buf: &mut Vec<'a, u8>,
        storage_manager: &mut StorageManager<'a, '_, X86_64GeneralReg, X86_64FloatReg, ASM, CC>,
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
        mov_reg64_reg64(buf, dst, X86_64GeneralReg::RDX);
    }

    #[inline(always)]
    fn jmp_imm32(buf: &mut Vec<'_, u8>, offset: i32) -> usize {
        jmp_imm32(buf, offset);

        // on x86_64, jumps are calculated from the end of the jmp instruction
        buf.len()
    }

    #[inline(always)]
    fn tail_call(buf: &mut Vec<'_, u8>) -> u64 {
        Self::jmp_imm32(buf, 0);
        buf.len() as u64 - 4
    }

    #[inline(always)]
    fn jne_reg64_imm64_imm32<'a, ASM, CC>(
        buf: &mut Vec<'a, u8>,
        storage_manager: &mut StorageManager<'a, '_, X86_64GeneralReg, X86_64FloatReg, ASM, CC>,
        reg: X86_64GeneralReg,
        imm: u64,
        offset: i32,
    ) -> usize
    where
        ASM: Assembler<X86_64GeneralReg, X86_64FloatReg>,
        CC: CallConv<X86_64GeneralReg, X86_64FloatReg, ASM>,
    {
        buf.reserve(13);
        if imm > i32::MAX as u64 {
            storage_manager.with_tmp_general_reg(buf, |_, buf, tmp| {
                mov_reg64_imm64(buf, tmp, imm as _);
                cmp_reg64_reg64(buf, RegisterWidth::W64, reg, tmp);
            })
        } else {
            cmp_reg64_imm32(buf, reg, imm as i32);
        }

        jne_imm32(buf, offset);

        // on x86_64, jumps are calculated from the end of the jmp instruction
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
    fn mov_reg32_freg32(buf: &mut Vec<'_, u8>, dst: X86_64GeneralReg, src: X86_64FloatReg) {
        movd_reg32_freg32(buf, dst, src);
    }
    #[inline(always)]
    fn mov_reg64_freg64(buf: &mut Vec<'_, u8>, dst: X86_64GeneralReg, src: X86_64FloatReg) {
        movq_reg64_freg64(buf, dst, src);
    }

    #[inline(always)]
    fn mov_freg32_reg32(_buf: &mut Vec<'_, u8>, _dst: X86_64FloatReg, _src: X86_64GeneralReg) {
        unimplemented!("`mov_freg32_reg32` is not currently used by the x86 backend")
    }
    #[inline(always)]
    fn mov_freg64_reg64(_buf: &mut Vec<'_, u8>, _dst: X86_64FloatReg, _src: X86_64GeneralReg) {
        unimplemented!("`mov_freg64_reg64` is not currently used by the x86 backend")
    }

    #[inline(always)]
    fn mov_reg_reg(
        buf: &mut Vec<'_, u8>,
        register_width: RegisterWidth,
        dst: X86_64GeneralReg,
        src: X86_64GeneralReg,
    ) {
        mov_reg_reg(buf, register_width, dst, src);
    }

    #[inline(always)]
    fn movsx_reg_reg(
        buf: &mut Vec<'_, u8>,
        input_width: RegisterWidth,
        dst: X86_64GeneralReg,
        src: X86_64GeneralReg,
    ) {
        use RegisterWidth::*;

        match input_width {
            W8 | W16 | W32 => raw_movsx_reg_reg(buf, input_width, dst, src),
            W64 => mov_reg_reg(buf, input_width, dst, src),
        }
    }

    #[inline(always)]
    fn movzx_reg_reg(
        buf: &mut Vec<'_, u8>,
        input_width: RegisterWidth,
        dst: X86_64GeneralReg,
        src: X86_64GeneralReg,
    ) {
        use RegisterWidth::*;

        match input_width {
            W8 | W16 => raw_movzx_reg_reg(buf, input_width, dst, src),
            W32 | W64 => mov_reg_reg(buf, input_width, dst, src),
        }
    }

    #[inline(always)]
    fn mov_freg64_mem64_offset32(
        buf: &mut Vec<'_, u8>,
        dst: X86_64FloatReg,
        src: X86_64GeneralReg,
        offset: i32,
    ) {
        movsd_freg64_base64_offset32(buf, dst, src, offset)
    }

    #[inline(always)]
    fn mov_freg32_mem32_offset32(
        buf: &mut Vec<'_, u8>,
        dst: X86_64FloatReg,
        src: X86_64GeneralReg,
        offset: i32,
    ) {
        movss_freg32_base32_offset32(buf, dst, src, offset)
    }

    #[inline(always)]
    fn mov_freg64_base32(buf: &mut Vec<'_, u8>, dst: X86_64FloatReg, offset: i32) {
        movsd_freg64_base64_offset32(buf, dst, X86_64GeneralReg::RBP, offset)
    }

    #[inline(always)]
    fn mov_freg32_base32(buf: &mut Vec<'_, u8>, dst: X86_64FloatReg, offset: i32) {
        movss_freg32_base32_offset32(buf, dst, X86_64GeneralReg::RBP, offset)
    }

    #[inline(always)]
    fn mov_reg_base32(
        buf: &mut Vec<'_, u8>,
        register_width: RegisterWidth,
        dst: X86_64GeneralReg,
        offset: i32,
    ) {
        use RegisterWidth::*;

        match register_width {
            W8 => mov_reg8_base8_offset32(buf, dst, X86_64GeneralReg::RBP, offset),
            W16 => mov_reg16_base16_offset32(buf, dst, X86_64GeneralReg::RBP, offset),
            W32 => mov_reg32_base32_offset32(buf, dst, X86_64GeneralReg::RBP, offset),
            W64 => mov_reg64_base64_offset32(buf, dst, X86_64GeneralReg::RBP, offset),
        }
    }

    #[inline(always)]
    fn mov_base32_freg64(buf: &mut Vec<'_, u8>, offset: i32, src: X86_64FloatReg) {
        movsd_base64_offset32_freg64(buf, X86_64GeneralReg::RBP, offset, src)
    }

    #[inline(always)]
    fn mov_base32_freg32(buf: &mut Vec<'_, u8>, offset: i32, src: X86_64FloatReg) {
        movss_base32_offset32_freg32(buf, X86_64GeneralReg::RBP, offset, src)
    }

    #[inline(always)]
    fn movesd_mem64_offset32_freg64(
        buf: &mut Vec<'_, u8>,
        ptr: X86_64GeneralReg,
        offset: i32,
        src: X86_64FloatReg,
    ) {
        movsd_base64_offset32_freg64(buf, ptr, offset, src)
    }

    #[inline(always)]
    fn mov_base32_reg(
        buf: &mut Vec<'_, u8>,
        register_width: RegisterWidth,
        offset: i32,
        src: X86_64GeneralReg,
    ) {
        use RegisterWidth::*;

        match register_width {
            W8 => mov_base8_offset32_reg8(buf, X86_64GeneralReg::RBP, offset, src),
            W16 => mov_base16_offset32_reg16(buf, X86_64GeneralReg::RBP, offset, src),
            W32 => mov_base32_offset32_reg32(buf, X86_64GeneralReg::RBP, offset, src),
            W64 => mov_base64_offset32_reg64(buf, X86_64GeneralReg::RBP, offset, src),
        }
    }

    #[inline(always)]
    fn mov_reg_mem_offset32(
        buf: &mut Vec<'_, u8>,
        register_width: RegisterWidth,
        dst: X86_64GeneralReg,
        src: X86_64GeneralReg,
        offset: i32,
    ) {
        match register_width {
            RegisterWidth::W8 => mov_reg8_base8_offset32(buf, dst, src, offset),
            RegisterWidth::W16 => mov_reg16_base16_offset32(buf, dst, src, offset),
            RegisterWidth::W32 => mov_reg32_base32_offset32(buf, dst, src, offset),
            RegisterWidth::W64 => mov_reg64_base64_offset32(buf, dst, src, offset),
        }
    }

    #[inline(always)]
    fn mov_mem_offset32_reg(
        buf: &mut Vec<'_, u8>,
        register_width: RegisterWidth,
        dst: X86_64GeneralReg,
        offset: i32,
        src: X86_64GeneralReg,
    ) {
        match register_width {
            RegisterWidth::W8 => mov_base8_offset32_reg8(buf, dst, offset, src),
            RegisterWidth::W16 => mov_base16_offset32_reg16(buf, dst, offset, src),
            RegisterWidth::W32 => mov_base32_offset32_reg32(buf, dst, offset, src),
            RegisterWidth::W64 => mov_base64_offset32_reg64(buf, dst, offset, src),
        }
    }

    #[inline(always)]
    fn movsx_reg_base32(
        buf: &mut Vec<'_, u8>,
        register_width: RegisterWidth,
        dst: X86_64GeneralReg,
        offset: i32,
    ) {
        use RegisterWidth::*;

        match register_width {
            W64 => Self::mov_reg64_base32(buf, dst, offset),
            W32 => movsx_reg64_base32_offset32(buf, dst, X86_64GeneralReg::RBP, offset),
            W16 => movsx_reg64_base16_offset32(buf, dst, X86_64GeneralReg::RBP, offset),
            W8 => movsx_reg64_base8_offset32(buf, dst, X86_64GeneralReg::RBP, offset),
        }
    }
    #[inline(always)]
    fn movzx_reg_base32(
        buf: &mut Vec<'_, u8>,
        register_width: RegisterWidth,
        dst: X86_64GeneralReg,
        offset: i32,
    ) {
        use RegisterWidth::*;

        match register_width {
            W64 => Self::mov_reg64_base32(buf, dst, offset),
            W32 => {
                // The Intel documentation (3.4.1.1 General-Purpose Registers in 64-Bit Mode in manual Basic Architecture))
                // 32-bit operands generate a 32-bit result, zero-extended to a 64-bit result in the destination general-purpose register.
                Self::mov_reg64_base32(buf, dst, offset)
            }
            W16 => movzx_reg64_base16_offset32(buf, dst, X86_64GeneralReg::RBP, offset),
            W8 => movzx_reg64_base8_offset32(buf, dst, X86_64GeneralReg::RBP, offset),
        }
    }

    #[inline(always)]
    fn mov_mem64_offset32_freg64(
        buf: &mut Vec<'_, u8>,
        dst: X86_64GeneralReg,
        offset: i32,
        src: X86_64FloatReg,
    ) {
        movsd_base64_offset32_freg64(buf, dst, offset, src)
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
    fn mov_stack32_reg(
        buf: &mut Vec<'_, u8>,
        register_width: RegisterWidth,
        offset: i32,
        src: X86_64GeneralReg,
    ) {
        mov_base_offset32_reg(buf, register_width, X86_64GeneralReg::RSP, offset, src)
    }

    #[inline(always)]
    fn neg_reg64_reg64(buf: &mut Vec<'_, u8>, dst: X86_64GeneralReg, src: X86_64GeneralReg) {
        mov_reg64_reg64(buf, dst, src);
        neg_reg64(buf, dst);
    }

    #[inline(always)]
    fn neg_freg64_freg64(
        buf: &mut Vec<'_, u8>,
        relocs: &mut Vec<'_, Relocation>,
        dst: X86_64FloatReg,
        src: X86_64FloatReg,
    ) {
        Self::mov_freg64_imm64(buf, relocs, dst, f64::from_bits(0x8000_0000_0000_0000));
        xorpd_freg64_freg64(buf, dst, src);
    }

    #[inline(always)]
    fn neg_freg32_freg32(
        buf: &mut Vec<'_, u8>,
        relocs: &mut Vec<'_, Relocation>,
        dst: X86_64FloatReg,
        src: X86_64FloatReg,
    ) {
        Self::mov_freg32_imm32(buf, relocs, dst, f32::from_bits(0x8000_0000));
        xorps_freg32_freg32(buf, dst, src);
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
    fn eq_reg_reg_reg(
        buf: &mut Vec<'_, u8>,
        register_width: RegisterWidth,
        dst: X86_64GeneralReg,
        src1: X86_64GeneralReg,
        src2: X86_64GeneralReg,
    ) {
        cmp_reg64_reg64(buf, register_width, src1, src2);
        sete_reg64(buf, dst);
    }

    #[inline(always)]
    fn neq_reg_reg_reg(
        buf: &mut Vec<'_, u8>,
        register_width: RegisterWidth,
        dst: X86_64GeneralReg,
        src1: X86_64GeneralReg,
        src2: X86_64GeneralReg,
    ) {
        cmp_reg64_reg64(buf, register_width, src1, src2);
        setne_reg64(buf, dst);
    }

    #[inline(always)]
    fn signed_compare_reg64(
        buf: &mut Vec<'_, u8>,
        register_width: RegisterWidth,
        operation: CompareOperation,
        dst: X86_64GeneralReg,
        src1: X86_64GeneralReg,
        src2: X86_64GeneralReg,
    ) {
        cmp_reg64_reg64(buf, register_width, src1, src2);

        match operation {
            CompareOperation::LessThan => setl_reg64(buf, dst),
            CompareOperation::LessThanOrEqual => setle_reg64(buf, dst),
            CompareOperation::GreaterThan => setg_reg64(buf, dst),
            CompareOperation::GreaterThanOrEqual => setge_reg64(buf, dst),
        }
    }

    fn unsigned_compare_reg64(
        buf: &mut Vec<'_, u8>,
        register_width: RegisterWidth,
        operation: CompareOperation,
        dst: X86_64GeneralReg,
        src1: X86_64GeneralReg,
        src2: X86_64GeneralReg,
    ) {
        cmp_reg64_reg64(buf, register_width, src1, src2);

        match operation {
            CompareOperation::LessThan => setb_reg64(buf, dst),
            CompareOperation::LessThanOrEqual => setbe_reg64(buf, dst),
            CompareOperation::GreaterThan => seta_reg64(buf, dst),
            CompareOperation::GreaterThanOrEqual => setae_reg64(buf, dst),
        }
    }

    fn eq_freg_freg_reg64(
        buf: &mut Vec<'_, u8>,
        dst: X86_64GeneralReg,
        src1: X86_64FloatReg,
        src2: X86_64FloatReg,
        width: FloatWidth,
    ) {
        match width {
            FloatWidth::F32 => cmp_freg32_freg32(buf, src1, src2),
            FloatWidth::F64 => cmp_freg64_freg64(buf, src1, src2),
        }

        sete_reg64(buf, dst);
    }

    fn neq_freg_freg_reg64(
        buf: &mut Vec<'_, u8>,
        dst: X86_64GeneralReg,
        src1: X86_64FloatReg,
        src2: X86_64FloatReg,
        width: FloatWidth,
    ) {
        match width {
            FloatWidth::F32 => cmp_freg32_freg32(buf, src1, src2),
            FloatWidth::F64 => cmp_freg64_freg64(buf, src1, src2),
        }

        setne_reg64(buf, dst);
    }

    #[inline(always)]
    fn cmp_freg_freg_reg64(
        buf: &mut Vec<'_, u8>,
        dst: X86_64GeneralReg,
        src1: X86_64FloatReg,
        src2: X86_64FloatReg,
        width: FloatWidth,
        operation: CompareOperation,
    ) {
        use CompareOperation::*;

        let (arg1, arg2) = match operation {
            LessThan | LessThanOrEqual => (src1, src2),
            GreaterThan | GreaterThanOrEqual => (src2, src1),
        };

        match width {
            FloatWidth::F32 => cmp_freg32_freg32(buf, arg2, arg1),
            FloatWidth::F64 => cmp_freg64_freg64(buf, arg2, arg1),
        }

        match operation {
            LessThan | GreaterThan => seta_reg64(buf, dst),
            LessThanOrEqual | GreaterThanOrEqual => setae_reg64(buf, dst),
        };
    }

    #[inline(always)]
    fn is_nan_freg_reg64(
        buf: &mut Vec<'_, u8>,
        dst: X86_64GeneralReg,
        src: X86_64FloatReg,
        width: FloatWidth,
    ) {
        match width {
            FloatWidth::F32 => cmp_freg32_freg32(buf, src, src),
            FloatWidth::F64 => cmp_freg64_freg64(buf, src, src),
        }

        setp_reg64(buf, dst)
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
    fn ret(buf: &mut Vec<'_, u8>) {
        ret(buf);
    }

    fn set_if_overflow(buf: &mut Vec<'_, u8>, dst: X86_64GeneralReg) {
        seto_reg64(buf, dst);
    }

    fn and_reg64_reg64_reg64(buf: &mut Vec<'_, u8>, dst: Reg64, src1: Reg64, src2: Reg64) {
        binop_move_src_to_dst_reg64(buf, and_reg64_reg64, dst, src1, src2)
    }

    fn or_reg64_reg64_reg64(buf: &mut Vec<'_, u8>, dst: Reg64, src1: Reg64, src2: Reg64) {
        binop_move_src_to_dst_reg64(buf, or_reg64_reg64, dst, src1, src2)
    }

    fn xor_reg64_reg64_reg64(buf: &mut Vec<'_, u8>, dst: Reg64, src1: Reg64, src2: Reg64) {
        binop_move_src_to_dst_reg64(buf, xor_reg64_reg64, dst, src1, src2)
    }

    fn shl_reg64_reg64_reg64<'a, ASM, CC>(
        buf: &mut Vec<'a, u8>,
        storage_manager: &mut StorageManager<'a, '_, X86_64GeneralReg, X86_64FloatReg, ASM, CC>,
        dst: X86_64GeneralReg,
        src1: X86_64GeneralReg,
        src2: X86_64GeneralReg,
    ) where
        ASM: Assembler<X86_64GeneralReg, X86_64FloatReg>,
        CC: CallConv<X86_64GeneralReg, X86_64FloatReg, ASM>,
    {
        shift_reg64_reg64_reg64(buf, storage_manager, shl_reg64_reg64, dst, src1, src2)
    }

    fn shr_reg64_reg64_reg64<'a, ASM, CC>(
        buf: &mut Vec<'a, u8>,
        storage_manager: &mut StorageManager<'a, '_, X86_64GeneralReg, X86_64FloatReg, ASM, CC>,
        dst: X86_64GeneralReg,
        src1: X86_64GeneralReg,
        src2: X86_64GeneralReg,
    ) where
        ASM: Assembler<X86_64GeneralReg, X86_64FloatReg>,
        CC: CallConv<X86_64GeneralReg, X86_64FloatReg, ASM>,
    {
        shift_reg64_reg64_reg64(buf, storage_manager, shr_reg64_reg64, dst, src1, src2)
    }

    fn sar_reg64_reg64_reg64<'a, ASM, CC>(
        buf: &mut Vec<'a, u8>,
        storage_manager: &mut StorageManager<'a, '_, X86_64GeneralReg, X86_64FloatReg, ASM, CC>,
        dst: X86_64GeneralReg,
        src1: X86_64GeneralReg,
        src2: X86_64GeneralReg,
    ) where
        ASM: Assembler<X86_64GeneralReg, X86_64FloatReg>,
        CC: CallConv<X86_64GeneralReg, X86_64FloatReg, ASM>,
    {
        shift_reg64_reg64_reg64(buf, storage_manager, sar_reg64_reg64, dst, src1, src2)
    }

    fn sqrt_freg64_freg64(buf: &mut Vec<'_, u8>, dst: X86_64FloatReg, src: X86_64FloatReg) {
        sqrtsd_freg64_freg64(buf, dst, src)
    }

    fn sqrt_freg32_freg32(buf: &mut Vec<'_, u8>, dst: X86_64FloatReg, src: X86_64FloatReg) {
        sqrtss_freg32_freg32(buf, dst, src)
    }
}

fn shift_reg64_reg64_reg64<'a, ASM, CC>(
    buf: &mut Vec<'a, u8>,
    storage_manager: &mut StorageManager<'a, '_, X86_64GeneralReg, X86_64FloatReg, ASM, CC>,
    shift_function: fn(buf: &mut Vec<'_, u8>, X86_64GeneralReg),
    dst: X86_64GeneralReg,
    src1: X86_64GeneralReg,
    src2: X86_64GeneralReg,
) where
    ASM: Assembler<X86_64GeneralReg, X86_64FloatReg>,
    CC: CallConv<X86_64GeneralReg, X86_64FloatReg, ASM>,
{
    macro_rules! helper {
        ($buf:expr, $dst:expr, $src1:expr, $src2:expr) => {{
            mov_reg64_reg64($buf, $dst, $src1);
            mov_reg64_reg64($buf, X86_64GeneralReg::RCX, $src2);

            shift_function($buf, $dst)
        }};
    }

    // if RCX is one of our input registers, we need to move some stuff around
    if let X86_64GeneralReg::RCX = dst {
        storage_manager.with_tmp_general_reg(buf, |_, buf, tmp| {
            helper!(buf, tmp, src1, src2);

            mov_reg64_reg64(buf, dst, tmp);
        })
    } else if let X86_64GeneralReg::RCX = src2 {
        storage_manager.with_tmp_general_reg(buf, |_, buf, tmp| {
            mov_reg64_reg64(buf, tmp, src2);

            helper!(buf, dst, src1, tmp);
        })
    } else {
        helper!(buf, dst, src1, src2)
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

const GRP_4: u8 = 0x66;

const REX: u8 = 0x40;

// see https://wiki.osdev.org/X86-64_Instruction_Encoding#Encoding
/// If set, 64-bit operand size is used
const REX_PREFIX_W: u8 = 0b1000;
/// Extension to the MODRM.reg
/// Permits access to additional registers
const REX_PREFIX_R: u8 = 0b0100;
#[allow(unused)]
/// Extension to the SIB.index field
const REX_PREFIX_X: u8 = 0b0010;
/// Extension to the MODRM.rm
const REX_PREFIX_B: u8 = 0b0001;

/// Wide REX (64-bit)
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
fn binop_reg8_reg8(op_code: u8, buf: &mut Vec<u8>, dst: X86_64GeneralReg, src: X86_64GeneralReg) {
    let dst_high = dst as u8 > 7;
    let dst_mod = dst as u8 % 8;
    let src_high = src as u8 > 7;
    let src_mod = src as u8 % 8;

    if dst_high || src_high {
        let rex = add_rm_extension(dst, REX);
        let rex = add_reg_extension(src, rex);

        buf.extend([rex, op_code, 0xC0 | dst_mod | (src_mod << 3)])
    } else {
        let rex_prefix = [
            X86_64GeneralReg::RBP,
            X86_64GeneralReg::RSP,
            X86_64GeneralReg::RSI,
            X86_64GeneralReg::RDI,
        ];

        if rex_prefix.contains(&src) || rex_prefix.contains(&dst) {
            buf.push(0x40);
        }

        buf.extend([op_code, 0xC0 | dst_mod | (src_mod << 3)]);
    }
}

#[inline(always)]
fn binop_reg16_reg16(
    op_code: u8,
    buf: &mut Vec<'_, u8>,
    dst: X86_64GeneralReg,
    src: X86_64GeneralReg,
) {
    let dst_high = dst as u8 > 7;
    let dst_mod = dst as u8 % 8;
    let src_high = src as u8 > 7;
    let src_mod = (src as u8 % 8) << 3;

    if dst_high || src_high {
        let rex = add_rm_extension(dst, REX);
        let rex = add_reg_extension(src, rex);

        buf.extend([0x66, rex, op_code, 0xC0 | dst_mod | src_mod])
    } else {
        buf.extend([0x66, op_code, 0xC0 | dst_mod | src_mod]);
    }
}

#[inline(always)]
fn binop_reg32_reg32(
    op_code: u8,
    buf: &mut Vec<'_, u8>,
    dst: X86_64GeneralReg,
    src: X86_64GeneralReg,
) {
    let dst_high = dst as u8 > 7;
    let dst_mod = dst as u8 % 8;
    let src_high = src as u8 > 7;
    let src_mod = (src as u8 % 8) << 3;

    if dst_high || src_high {
        let rex = add_rm_extension(dst, REX);
        let rex = add_reg_extension(src, rex);

        buf.extend([rex, op_code, 0xC0 | dst_mod | src_mod])
    } else {
        buf.extend([op_code, 0xC0 | dst_mod | src_mod]);
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
    buf.extend([rex, op_code, 0xC0 | dst_mod | src_mod]);
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
    buf.extend([rex, op_code1, op_code2, 0xC0 | dst_mod | src_mod]);
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
    buf.extend([rex, 0x81, 0xC0 | dst_mod]);
    buf.extend(imm.to_le_bytes());
}

/// `ADD r/m64,r64` -> Add r64 to r/m64.
#[inline(always)]
fn add_reg64_reg64(buf: &mut Vec<'_, u8>, dst: X86_64GeneralReg, src: X86_64GeneralReg) {
    binop_reg64_reg64(0x01, buf, dst, src);
}

/// `AND r/m64,r64` -> Bitwise logical and r64 to r/m64.
#[inline(always)]
fn and_reg64_reg64(buf: &mut Vec<'_, u8>, dst: X86_64GeneralReg, src: X86_64GeneralReg) {
    // NOTE: src and dst are flipped by design
    binop_reg64_reg64(0x23, buf, src, dst);
}

/// `OR r/m64,r64` -> Bitwise logical or r64 to r/m64.
#[inline(always)]
fn or_reg64_reg64(buf: &mut Vec<'_, u8>, dst: X86_64GeneralReg, src: X86_64GeneralReg) {
    // NOTE: src and dst are flipped by design
    binop_reg64_reg64(0x0B, buf, src, dst);
}

/// `XOR r/m64,r64` -> Bitwise logical exclusive or r64 to r/m64.
#[inline(always)]
fn xor_reg64_reg64(buf: &mut Vec<'_, u8>, dst: X86_64GeneralReg, src: X86_64GeneralReg) {
    // NOTE: src and dst are flipped by design
    binop_reg64_reg64(0x33, buf, src, dst);
}

/// `SHL r/m64, CL` -> Multiply r/m64 by 2, CL times.
#[inline(always)]
fn shl_reg64_reg64(buf: &mut Vec<'_, u8>, dst: X86_64GeneralReg) {
    let rex = add_rm_extension(dst, REX_W);
    let rex = add_reg_extension(dst, rex);

    let dst_mod = dst as u8 % 8;
    buf.extend([rex, 0xD3, 0xC0 | (4 << 3) | dst_mod]);
}

/// `SHR r/m64, CL` -> Unsigned divide r/m64 by 2, CL times.
#[inline(always)]
fn shr_reg64_reg64(buf: &mut Vec<'_, u8>, dst: X86_64GeneralReg) {
    let rex = add_rm_extension(dst, REX_W);
    let rex = add_reg_extension(dst, rex);

    let dst_mod = dst as u8 % 8;
    buf.extend([rex, 0xD3, 0xC0 | (5 << 3) | dst_mod]);
}

/// `SAR r/m64, CL` -> Signed divide r/m64 by 2, CL times.
#[inline(always)]
fn sar_reg64_reg64(buf: &mut Vec<'_, u8>, dst: X86_64GeneralReg) {
    let rex = add_rm_extension(dst, REX_W);
    let rex = add_reg_extension(dst, rex);

    let dst_mod = dst as u8 % 8;
    buf.extend([rex, 0xD3, 0xC0 | (7 << 3) | dst_mod]);
}

fn double_binary_operation(
    buf: &mut Vec<'_, u8>,
    dst: X86_64FloatReg,
    src: X86_64FloatReg,
    float_width: FloatWidth,
    op_code2: u8,
) {
    let op_code1 = match float_width {
        FloatWidth::F32 => 0xF3,
        FloatWidth::F64 => 0xF2,
    };
    let dst_high = dst as u8 > 7;
    let dst_mod = dst as u8 % 8;
    let src_high = src as u8 > 7;
    let src_mod = src as u8 % 8;
    if dst_high || src_high {
        buf.extend([
            op_code1,
            0x40 | ((dst_high as u8) << 2) | (src_high as u8),
            0x0F,
            op_code2,
            0xC0 | (dst_mod << 3) | (src_mod),
        ])
    } else {
        buf.extend([op_code1, 0x0F, op_code2, 0xC0 | (dst_mod << 3) | (src_mod)])
    }
}

/// `ADDSD xmm1,xmm2/m64` -> Add the low double-precision floating-point value from xmm2/mem to xmm1 and store the result in xmm1.
#[inline(always)]
fn addsd_freg64_freg64(buf: &mut Vec<'_, u8>, dst: X86_64FloatReg, src: X86_64FloatReg) {
    double_binary_operation(buf, dst, src, FloatWidth::F64, 0x58)
}

/// `ADDSS xmm1,xmm2/m64` -> Add the low single-precision floating-point value from xmm2/mem to xmm1 and store the result in xmm1.
#[inline(always)]
fn addss_freg32_freg32(buf: &mut Vec<'_, u8>, dst: X86_64FloatReg, src: X86_64FloatReg) {
    double_binary_operation(buf, dst, src, FloatWidth::F32, 0x58)
}

/// `SUBSD xmm1,xmm2/m64` -> Sub the low double-precision floating-point value from xmm2/mem to xmm1 and store the result in xmm1.
#[inline(always)]
fn subsd_freg64_freg64(buf: &mut Vec<'_, u8>, dst: X86_64FloatReg, src: X86_64FloatReg) {
    double_binary_operation(buf, dst, src, FloatWidth::F64, 0x5C)
}

/// `SUBSS xmm1,xmm2/m64` -> Sub the low single-precision floating-point value from xmm2/mem to xmm1 and store the result in xmm1.
#[inline(always)]
fn subss_freg32_freg32(buf: &mut Vec<'_, u8>, dst: X86_64FloatReg, src: X86_64FloatReg) {
    double_binary_operation(buf, dst, src, FloatWidth::F32, 0x5C)
}

#[inline(always)]
fn mulsd_freg64_freg64(buf: &mut Vec<'_, u8>, dst: X86_64FloatReg, src: X86_64FloatReg) {
    double_binary_operation(buf, dst, src, FloatWidth::F64, 0x59)
}

#[inline(always)]
fn mulss_freg32_freg32(buf: &mut Vec<'_, u8>, dst: X86_64FloatReg, src: X86_64FloatReg) {
    double_binary_operation(buf, dst, src, FloatWidth::F32, 0x59)
}

/// `DIVSS xmm1,xmm2/m64` -> Divide the low single-precision floating-point value from xmm2/mem to xmm1 and store the result in xmm1.
#[inline(always)]
fn divss_freg32_freg32(buf: &mut Vec<'_, u8>, dst: X86_64FloatReg, src: X86_64FloatReg) {
    double_binary_operation(buf, dst, src, FloatWidth::F32, 0x5E)
}

/// `DIVSD xmm1,xmm2/m64` -> Divide the low double-precision floating-point value from xmm2/mem to xmm1 and store the result in xmm1.
#[inline(always)]
fn divsd_freg64_freg64(buf: &mut Vec<'_, u8>, dst: X86_64FloatReg, src: X86_64FloatReg) {
    double_binary_operation(buf, dst, src, FloatWidth::F64, 0x5E)
}

#[inline(always)]
fn andpd_freg64_freg64(buf: &mut Vec<'_, u8>, dst: X86_64FloatReg, src: X86_64FloatReg) {
    let dst_high = dst as u8 > 7;
    let dst_mod = dst as u8 % 8;
    let src_high = src as u8 > 7;
    let src_mod = src as u8 % 8;

    if dst_high || src_high {
        buf.extend([
            0x66,
            0x40 | ((dst_high as u8) << 2) | (src_high as u8),
            0x0F,
            0x54,
            0xC0 | (dst_mod << 3) | (src_mod),
        ])
    } else {
        buf.extend([0x66, 0x0F, 0x54, 0xC0 | (dst_mod << 3) | (src_mod)])
    }
}

#[inline(always)]
fn andps_freg32_freg32(buf: &mut Vec<'_, u8>, dst: X86_64FloatReg, src: X86_64FloatReg) {
    let dst_high = dst as u8 > 7;
    let dst_mod = dst as u8 % 8;
    let src_high = src as u8 > 7;
    let src_mod = src as u8 % 8;

    if dst_high || src_high {
        buf.extend([
            0x40 | ((dst_high as u8) << 2) | (src_high as u8),
            0x0F,
            0x54,
            0xC0 | (dst_mod << 3) | (src_mod),
        ])
    } else {
        buf.extend([0x0F, 0x54, 0xC0 | (dst_mod << 3) | (src_mod)])
    }
}

/// r/m64 AND imm8 (sign-extended).
#[inline(always)]
fn and_reg64_imm8(buf: &mut Vec<'_, u8>, dst: X86_64GeneralReg, imm: i8) {
    let rex = add_rm_extension(dst, REX_W);
    let dst_mod = dst as u8 % 8;
    buf.extend([rex, 0x83, 0xE0 | dst_mod, imm as u8]);
}

/// `CMOVL r64,r/m64` -> Move if less (SF≠ OF).
#[inline(always)]
fn cmovl_reg64_reg64(buf: &mut Vec<'_, u8>, dst: X86_64GeneralReg, src: X86_64GeneralReg) {
    let rex = add_reg_extension(dst, REX_W);
    let rex = add_rm_extension(src, rex);
    let dst_mod = (dst as u8 % 8) << 3;
    let src_mod = src as u8 % 8;
    buf.extend([rex, 0x0F, 0x4C, 0xC0 | dst_mod | src_mod]);
}

/// `CMP r/m64,i32` -> Compare i32 to r/m64.
#[inline(always)]
fn cmp_reg64_imm32(buf: &mut Vec<'_, u8>, dst: X86_64GeneralReg, imm: i32) {
    let rex = add_rm_extension(dst, REX_W);
    let dst_mod = dst as u8 % 8;
    buf.reserve(7);
    buf.extend([rex, 0x81, 0xF8 | dst_mod]);
    buf.extend(imm.to_le_bytes());
}

/// `CMP r/m64,r64` -> Compare r64 to r/m64.
#[inline(always)]
fn cmp_reg64_reg64(
    buf: &mut Vec<'_, u8>,
    register_width: RegisterWidth,
    dst: X86_64GeneralReg,
    src: X86_64GeneralReg,
) {
    match register_width {
        RegisterWidth::W8 => binop_reg64_reg64(0x38, buf, dst, src),
        RegisterWidth::W16 => binop_reg16_reg16(0x39, buf, dst, src),
        RegisterWidth::W32 => binop_reg32_reg32(0x39, buf, dst, src),
        RegisterWidth::W64 => binop_reg64_reg64(0x39, buf, dst, src),
    }
}

#[inline(always)]
fn cmp_freg64_freg64(buf: &mut Vec<'_, u8>, src1: X86_64FloatReg, src2: X86_64FloatReg) {
    let src1_high = src1 as u8 > 7;
    let src1_mod = src1 as u8 % 8;

    let src2_high = src2 as u8 > 7;
    let src2_mod = src2 as u8 % 8;

    if src1_high || src2_high {
        buf.extend([
            0x66,
            0x40 | ((src1_high as u8) << 2) | (src2_high as u8),
            0x0F,
            0x2E,
            0xC0 | (src1_mod << 3) | (src2_mod),
        ])
    } else {
        buf.extend([0x66, 0x0F, 0x2E, 0xC0 | (src1_mod << 3) | (src2_mod)])
    }
}

#[inline(always)]
fn cmp_freg32_freg32(buf: &mut Vec<'_, u8>, src1: X86_64FloatReg, src2: X86_64FloatReg) {
    let src1_high = src1 as u8 > 7;
    let src1_mod = src1 as u8 % 8;

    let src2_high = src2 as u8 > 7;
    let src2_mod = src2 as u8 % 8;

    if src1_high || src2_high {
        buf.extend([
            0x65,
            0x40 | ((src1_high as u8) << 2) | (src2_high as u8),
            0x0F,
            0x2E,
            0xC0 | (src1_mod << 3) | (src2_mod),
        ])
    } else {
        buf.extend([0x65, 0x0F, 0x2E, 0xC0 | (src1_mod << 3) | (src2_mod)])
    }
}

#[inline(always)]
fn sqrtsd_freg64_freg64(buf: &mut Vec<'_, u8>, dst: X86_64FloatReg, src: X86_64FloatReg) {
    let dst_high = dst as u8 > 7;
    let dst_mod = dst as u8 % 8;

    let src_high = src as u8 > 7;
    let src_mod = src as u8 % 8;

    if dst_high || src_high {
        buf.extend([
            0xF2,
            0x40 | ((dst_high as u8) << 2) | (src_high as u8),
            0x0F,
            0x51,
            0xC0 | (dst_mod << 3) | (src_mod),
        ])
    } else {
        buf.extend([0xF2, 0x0F, 0x51, 0xC0 | (dst_mod << 3) | (src_mod)])
    }
}

#[inline(always)]
fn sqrtss_freg32_freg32(buf: &mut Vec<'_, u8>, dst: X86_64FloatReg, src: X86_64FloatReg) {
    let dst_high = dst as u8 > 7;
    let dst_mod = dst as u8 % 8;

    let src_high = src as u8 > 7;
    let src_mod = src as u8 % 8;

    if dst_high || src_high {
        buf.extend([
            0xF3,
            0x40 | ((dst_high as u8) << 2) | (src_high as u8),
            0x0F,
            0x51,
            0xC0 | (dst_mod << 3) | (src_mod),
        ])
    } else {
        buf.extend([0xF3, 0x0F, 0x51, 0xC0 | (dst_mod << 3) | (src_mod)])
    }
}

/// `XORPD xmm1, xmm2/m128` -> Bitwise exclusive-OR of xmm2/m128 and xmm1.
#[inline(always)]
fn xorpd_freg64_freg64(buf: &mut Vec<'_, u8>, dst: X86_64FloatReg, src: X86_64FloatReg) {
    let dst_high = dst as u8 > 7;
    let dst_mod = dst as u8 % 8;

    let src_high = src as u8 > 7;
    let src_mod = src as u8 % 8;

    if dst_high || src_high {
        buf.extend([
            0x66,
            0x40 | ((dst_high as u8) << 2) | (src_high as u8),
            0x0F,
            0x57,
            0xC0 | (dst_mod << 3) | src_mod,
        ])
    } else {
        buf.extend([0x66, 0x0F, 0x57, 0xC0 | (dst_mod << 3) | src_mod]);
    }
}

/// `XORPS xmm1,xmm2/m128` -> Bitwise exclusive-OR of xmm2/m128 and xmm1.
#[inline(always)]
fn xorps_freg32_freg32(buf: &mut Vec<'_, u8>, dst: X86_64FloatReg, src: X86_64FloatReg) {
    let dst_high = dst as u8 > 7;
    let dst_mod = dst as u8 % 8;

    let src_high = src as u8 > 7;
    let src_mod = src as u8 % 8;

    if dst_high || src_high {
        buf.extend([
            0x40 | ((dst_high as u8) << 2) | (src_high as u8),
            0x0F,
            0x57,
            0xC0 | (dst_mod << 3) | src_mod,
        ]);
    } else {
        buf.extend([0x0F, 0x57, 0xC0 | (dst_mod << 3) | src_mod]);
    }
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

    buf.extend([rex, 0xF7, 0b1110_0000 | (src as u8 % 8)]);
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
    buf.extend([0x48, 0x99]);

    buf.extend([rex, 0xF7, 0b1111_1000 | (src as u8 % 8)]);
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
    buf.extend([0x48, 0x99]);

    // adds a cqo (convert doubleword to quadword)
    buf.extend([rex, 0xF7, 0b1111_0000 | (src as u8 % 8)]);
}

/// Jump near, relative, RIP = RIP + 32-bit displacement sign extended to 64-bits.
#[inline(always)]
fn jmp_imm32(buf: &mut Vec<'_, u8>, imm: i32) {
    buf.reserve(5);
    buf.push(0xE9);
    buf.extend(imm.to_le_bytes());
}

#[inline(always)]
fn jmp_reg64_offset8(buf: &mut Vec<'_, u8>, base: X86_64GeneralReg, offset: i8) {
    let rex = add_rm_extension(base, REX_W);

    #[allow(clippy::unusual_byte_groupings)]
    buf.extend([rex, 0xff, 0b01_100_000 | (base as u8 % 8)]);

    // Using RSP or R12 requires a secondary index byte.
    if base == X86_64GeneralReg::RSP || base == X86_64GeneralReg::R12 {
        buf.push(0x24);
    }

    buf.extend(offset.to_le_bytes())
}

/// Jump near if not equal (ZF=0).
#[inline(always)]
fn jne_imm32(buf: &mut Vec<'_, u8>, imm: i32) {
    buf.reserve(6);
    buf.push(0x0F);
    buf.push(0x85);
    buf.extend(imm.to_le_bytes());
}

/// `MOV r/m64, imm32` -> Move imm32 sign extended to 64-bits to r/m64.
#[inline(always)]
fn mov_reg64_imm32(buf: &mut Vec<'_, u8>, dst: X86_64GeneralReg, imm: i32) {
    let rex = add_rm_extension(dst, REX_W);
    let dst_mod = dst as u8 % 8;
    buf.reserve(7);
    buf.extend([rex, 0xC7, 0xC0 | dst_mod]);
    buf.extend(imm.to_le_bytes());
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
        buf.extend([rex, 0xB8 | dst_mod]);
        buf.extend(imm.to_le_bytes());
    }
}

/// `LEA r64, m` -> Store effective address for m in register r64.
#[inline(always)]
fn lea_reg64(buf: &mut Vec<'_, u8>, dst: X86_64GeneralReg) {
    let rex = add_opcode_extension(dst, REX_W);
    let rex = add_reg_extension(dst, rex);
    let dst_mod = dst as u8 % 8;

    #[allow(clippy::unusual_byte_groupings)]
    buf.extend([
        rex,
        0x8d,
        0b00_000_101 | (dst_mod << 3),
        0x00,
        0x00,
        0x00,
        0x00,
    ])
}

/// `LEA r64, m` -> Store effective address for m in register r64.
#[inline(always)]
fn lea_reg64_offset8(
    buf: &mut Vec<'_, u8>,
    dst: X86_64GeneralReg,
    src: X86_64GeneralReg,
    offset: i8,
) {
    let rex = add_rm_extension(src, REX_W);
    let rex = add_reg_extension(dst, rex);

    let dst_mod = dst as u8 % 8;
    let src_mod = src as u8 % 8;

    #[allow(clippy::unusual_byte_groupings)]
    // the upper bits 0b01 of the mod_rm byte indicate 8-bit displacement
    buf.extend([rex, 0x8d, 0b01_000_000 | (dst_mod << 3) | src_mod]);

    // Using RSP or R12 requires a secondary index byte.
    if src == X86_64GeneralReg::RSP || src == X86_64GeneralReg::R12 {
        buf.push(0x24);
    }

    buf.push(offset as u8);
}

fn raw_mov_reg_reg(
    buf: &mut Vec<'_, u8>,
    register_width: RegisterWidth,
    dst: X86_64GeneralReg,
    src: X86_64GeneralReg,
) {
    match register_width {
        RegisterWidth::W8 => binop_reg8_reg8(0x88, buf, dst, src),
        RegisterWidth::W16 => binop_reg16_reg16(0x89, buf, dst, src),
        RegisterWidth::W32 => binop_reg32_reg32(0x89, buf, dst, src),
        RegisterWidth::W64 => binop_reg64_reg64(0x89, buf, dst, src),
    }
}

#[allow(unused)]
fn raw_movsx_reg_reg(
    buf: &mut Vec<u8>,
    input_width: RegisterWidth,
    dst: X86_64GeneralReg,
    src: X86_64GeneralReg,
) {
    let dst_high = dst as u8 > 7;
    let dst_mod = dst as u8 % 8;
    let src_high = src as u8 > 7;
    let src_mod = src as u8 % 8;

    // NOTE src and dst seem to be flipped here. It works this way though
    let mod_rm = 0xC0 | (dst_mod << 3) | src_mod;

    let rex = add_rm_extension(src, REX_W);
    let rex = add_reg_extension(dst, rex);

    match input_width {
        RegisterWidth::W8 => {
            buf.extend([rex, 0x0f, 0xbe, mod_rm]);
        }
        RegisterWidth::W16 => {
            buf.extend([rex, 0x0f, 0xbf, mod_rm]);
        }
        RegisterWidth::W32 => {
            buf.extend([rex, 0x63, mod_rm]);
        }
        RegisterWidth::W64 => { /* do nothing */ }
    }
}

#[allow(unused)]
fn raw_movzx_reg_reg(
    buf: &mut Vec<u8>,
    input_width: RegisterWidth,
    dst: X86_64GeneralReg,
    src: X86_64GeneralReg,
) {
    let dst_high = dst as u8 > 7;
    let dst_mod = dst as u8 % 8;
    let src_high = src as u8 > 7;
    let src_mod = src as u8 % 8;

    // NOTE src and dst seem to be flipped here. It works this way though
    let mod_rm = 0xC0 | (dst_mod << 3) | src_mod;

    let rex = add_rm_extension(src, REX_W);
    let rex = add_reg_extension(dst, rex);

    match input_width {
        RegisterWidth::W8 => {
            buf.extend([rex, 0x0f, 0xb6, mod_rm]);
        }
        RegisterWidth::W16 => {
            buf.extend([rex, 0x0f, 0xb7, mod_rm]);
        }
        RegisterWidth::W32 | RegisterWidth::W64 => { /* do nothing */ }
    }
}

/// `MOV r/m64,r64` -> Move r64 to r/m64.
/// This will not generate anything if dst and src are the same.
#[inline(always)]
fn mov_reg64_reg64(buf: &mut Vec<'_, u8>, dst: X86_64GeneralReg, src: X86_64GeneralReg) {
    mov_reg_reg(buf, RegisterWidth::W64, dst, src)
}

#[inline(always)]
fn mov_reg_reg(
    buf: &mut Vec<'_, u8>,
    register_width: RegisterWidth,
    dst: X86_64GeneralReg,
    src: X86_64GeneralReg,
) {
    if dst != src {
        raw_mov_reg_reg(buf, register_width, dst, src);
    }
}

// The following base and stack based operations could be optimized based on how many bytes the offset actually is.

#[inline(always)]
fn mov_base_offset32_reg(
    buf: &mut Vec<'_, u8>,
    register_width: RegisterWidth,
    base: X86_64GeneralReg,
    offset: i32,
    src: X86_64GeneralReg,
) {
    match register_width {
        RegisterWidth::W8 => mov_base16_offset32_reg16(buf, base, offset, src),
        RegisterWidth::W16 => mov_base16_offset32_reg16(buf, base, offset, src),
        RegisterWidth::W32 => mov_base32_offset32_reg32(buf, base, offset, src),
        RegisterWidth::W64 => mov_base64_offset32_reg64(buf, base, offset, src),
    }
}

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
    buf.extend([rex, 0x89, 0x80 | src_mod | base_mod]);
    // Using RSP or R12 requires a secondary index byte.
    if base == X86_64GeneralReg::RSP || base == X86_64GeneralReg::R12 {
        buf.push(0x24);
    }
    buf.extend(offset.to_le_bytes());
}

/// `MOV r/m32,r32` -> Move r32 to r/m32, where m32 references a base + offset.
#[inline(always)]
fn mov_base32_offset32_reg32(
    buf: &mut Vec<'_, u8>,
    base: X86_64GeneralReg,
    offset: i32,
    src: X86_64GeneralReg,
) {
    let rex = add_rm_extension(base, REX);
    let rex = add_reg_extension(src, rex);
    let src_mod = (src as u8 % 8) << 3;
    let base_mod = base as u8 % 8;
    buf.reserve(8);
    buf.extend([rex, 0x89, 0x80 | src_mod | base_mod]);
    // Using RSP or R12 requires a secondary index byte.
    if base == X86_64GeneralReg::RSP || base == X86_64GeneralReg::R12 {
        buf.push(0x24);
    }
    buf.extend(offset.to_le_bytes());
}

/// `MOV r/m16,r16` -> Move r16 to r/m16, where m16 references a base + offset.
#[inline(always)]
fn mov_base16_offset32_reg16(
    buf: &mut Vec<'_, u8>,
    base: X86_64GeneralReg,
    offset: i32,
    src: X86_64GeneralReg,
) {
    let rex = add_rm_extension(base, REX);
    let rex = add_reg_extension(src, rex);
    let src_mod = (src as u8 % 8) << 3;
    let base_mod = base as u8 % 8;
    buf.reserve(8);
    buf.extend([GRP_4, rex, 0x89, 0x80 | src_mod | base_mod]);
    // Using RSP or R12 requires a secondary index byte.
    if base == X86_64GeneralReg::RSP || base == X86_64GeneralReg::R12 {
        buf.push(0x24);
    }
    buf.extend(offset.to_le_bytes());
}

/// `MOV r/m8,r8` -> Move r8 to r/m8, where m8 references a base + offset.
#[inline(always)]
fn mov_base8_offset32_reg8(
    buf: &mut Vec<'_, u8>,
    base: X86_64GeneralReg,
    offset: i32,
    src: X86_64GeneralReg,
) {
    let rex = add_rm_extension(base, REX);
    let rex = add_reg_extension(src, rex);
    let src_mod = (src as u8 % 8) << 3;
    let base_mod = base as u8 % 8;
    buf.reserve(8);
    buf.extend([rex, 0x88, 0x80 | src_mod | base_mod]);
    // Using RSP or R12 requires a secondary index byte.
    if base == X86_64GeneralReg::RSP || base == X86_64GeneralReg::R12 {
        buf.push(0x24);
    }
    buf.extend(offset.to_le_bytes());
}

#[inline(always)]
fn mov_reg_base_offset32(
    buf: &mut Vec<'_, u8>,
    register_width: RegisterWidth,
    dst: X86_64GeneralReg,
    base: X86_64GeneralReg,
    offset: i32,
) {
    use RegisterWidth::*;

    let rex = match register_width {
        W64 => REX_W,
        _ => REX,
    };

    let rex = add_rm_extension(base, rex);
    let rex = add_reg_extension(dst, rex);

    let dst_mod = (dst as u8 % 8) << 3;
    let base_mod = base as u8 % 8;
    let operands = 0x80 | dst_mod | base_mod;

    buf.reserve(8);

    let instruction = match register_width {
        W8 => 0x8A,
        W16 | W32 | W64 => 0x8B,
    };

    match register_width {
        W16 => buf.extend([GRP_4, rex, instruction, operands]),
        _ => buf.extend([rex, instruction, operands]),
    };

    // Using RSP or R12 requires a secondary index byte.
    if base == X86_64GeneralReg::RSP || base == X86_64GeneralReg::R12 {
        buf.push(0x24);
    }
    buf.extend(offset.to_le_bytes());
}

/// `MOV r64,r/m64` -> Move r/m64 to r64, where m64 references a base + offset.
#[inline(always)]
fn mov_reg64_base64_offset32(
    buf: &mut Vec<'_, u8>,
    dst: X86_64GeneralReg,
    base: X86_64GeneralReg,
    offset: i32,
) {
    mov_reg_base_offset32(buf, RegisterWidth::W64, dst, base, offset)
}

/// `MOV r/m32,r32` -> Move r32 to r/m32.
#[inline(always)]
fn mov_reg32_base32_offset32(
    buf: &mut Vec<'_, u8>,
    dst: X86_64GeneralReg,
    base: X86_64GeneralReg,
    offset: i32,
) {
    mov_reg_base_offset32(buf, RegisterWidth::W32, dst, base, offset)
}

/// `MOV r/m16,r16` -> Move r16 to r/m16.
#[inline(always)]
fn mov_reg16_base16_offset32(
    buf: &mut Vec<'_, u8>,
    dst: X86_64GeneralReg,
    base: X86_64GeneralReg,
    offset: i32,
) {
    mov_reg_base_offset32(buf, RegisterWidth::W16, dst, base, offset)
}

/// `MOV r/m8,r8` -> Move r8 to r/m8.
#[inline(always)]
fn mov_reg8_base8_offset32(
    buf: &mut Vec<'_, u8>,
    dst: X86_64GeneralReg,
    base: X86_64GeneralReg,
    offset: i32,
) {
    mov_reg_base_offset32(buf, RegisterWidth::W8, dst, base, offset)
}

#[inline(always)]
fn movsx_reg64_base_offset32(
    buf: &mut Vec<'_, u8>,
    dst: X86_64GeneralReg,
    base: X86_64GeneralReg,
    offset: i32,
    opcode: &[u8],
) {
    let rex = add_rm_extension(base, REX_W);
    let rex = add_reg_extension(dst, rex);
    let dst_mod = (dst as u8 % 8) << 3;
    let base_mod = base as u8 % 8;
    buf.reserve(9);

    // our output is a 64-bit value, so rex is always needed
    buf.push(rex);
    buf.extend(opcode);
    buf.push(0x80 | dst_mod | base_mod);

    // Using RSP or R12 requires a secondary index byte.
    if base == X86_64GeneralReg::RSP || base == X86_64GeneralReg::R12 {
        buf.push(0x24);
    }
    buf.extend(offset.to_le_bytes());
}

/// `MOVSX r64,r/m32` -> Move r/m32 with sign extention to r64, where m32 references a base + offset.
#[inline(always)]
fn movsx_reg64_base32_offset32(
    buf: &mut Vec<'_, u8>,
    dst: X86_64GeneralReg,
    base: X86_64GeneralReg,
    offset: i32,
) {
    movsx_reg64_base_offset32(buf, dst, base, offset, &[0x63])
}

/// `MOVSX r64,r/m16` -> Move r/m16 with sign extention to r64, where m16 references a base + offset.
#[inline(always)]
fn movsx_reg64_base16_offset32(
    buf: &mut Vec<'_, u8>,
    dst: X86_64GeneralReg,
    base: X86_64GeneralReg,
    offset: i32,
) {
    movsx_reg64_base_offset32(buf, dst, base, offset, &[0x0F, 0xBF])
}

/// `MOVSX r64,r/m8` -> Move r/m8 with sign extention to r64, where m8 references a base + offset.
#[inline(always)]
fn movsx_reg64_base8_offset32(
    buf: &mut Vec<'_, u8>,
    dst: X86_64GeneralReg,
    base: X86_64GeneralReg,
    offset: i32,
) {
    movsx_reg64_base_offset32(buf, dst, base, offset, &[0x0F, 0xBE])
}

#[inline(always)]
fn movzx_reg64_base_offset32(
    buf: &mut Vec<'_, u8>,
    dst: X86_64GeneralReg,
    base: X86_64GeneralReg,
    offset: i32,
    opcode: u8,
) {
    let rex = add_rm_extension(base, REX_W);
    let rex = add_reg_extension(dst, rex);
    let dst_mod = (dst as u8 % 8) << 3;
    let base_mod = base as u8 % 8;
    buf.reserve(9);
    buf.extend([rex, 0x0F, opcode, 0x80 | dst_mod | base_mod]);
    // Using RSP or R12 requires a secondary index byte.
    if base == X86_64GeneralReg::RSP || base == X86_64GeneralReg::R12 {
        buf.push(0x24);
    }
    buf.extend(offset.to_le_bytes());
}

/// `MOVZX r64,r/m8` -> Move r/m8 with zero extention to r64, where m8 references a base + offset.
#[inline(always)]
fn movzx_reg64_base8_offset32(
    buf: &mut Vec<'_, u8>,
    dst: X86_64GeneralReg,
    base: X86_64GeneralReg,
    offset: i32,
) {
    movzx_reg64_base_offset32(buf, dst, base, offset, 0xB6)
}

/// `MOVZX r64,r/m16` -> Move r/m16 with zero extention to r64, where m16 references a base + offset.
#[inline(always)]
fn movzx_reg64_base16_offset32(
    buf: &mut Vec<'_, u8>,
    dst: X86_64GeneralReg,
    base: X86_64GeneralReg,
    offset: i32,
) {
    movzx_reg64_base_offset32(buf, dst, base, offset, 0xB7)
}

#[inline(always)]
fn movd_reg32_freg32(buf: &mut Vec<'_, u8>, dst: X86_64GeneralReg, src: X86_64FloatReg) {
    let dst_high = dst as u8 > 7;
    let dst_mod = dst as u8 % 8;
    let src_high = src as u8 > 7;
    let src_mod = src as u8 % 8;
    if dst_high || src_high {
        let rex = add_rm_extension(dst, REX);
        let rex = add_reg_extension(src, rex);

        buf.extend([0x66, rex, 0x0F, 0x7E, 0xC0 | (src_mod << 3) | (dst_mod)])
    } else {
        buf.extend([0x66, 0x0F, 0x7E, 0xC0 | (src_mod << 3) | (dst_mod)])
    }
}

#[inline(always)]
fn movq_reg64_freg64(buf: &mut Vec<'_, u8>, dst: X86_64GeneralReg, src: X86_64FloatReg) {
    let dst_mod = dst as u8 % 8;
    let src_mod = src as u8 % 8;

    let rex = add_rm_extension(dst, REX_W);
    let rex = add_reg_extension(src, rex);

    buf.extend([0x66, rex, 0x0F, 0x7E, 0xC0 | (src_mod << 3) | (dst_mod)]);
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
        buf.extend([
            0xF2,
            0x40 | ((dst_high as u8) << 2) | (src_high as u8),
            0x0F,
            0x10,
            0xC0 | (dst_mod << 3) | (src_mod),
        ])
    } else {
        buf.extend([0xF2, 0x0F, 0x10, 0xC0 | (dst_mod << 3) | (src_mod)])
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
        buf.extend([
            0xF3,
            0x40 | ((dst_high as u8) << 2) | (src_high as u8),
            0x0F,
            0x10,
            0xC0 | (dst_mod << 3) | (src_mod),
        ])
    } else {
        buf.extend([0xF3, 0x0F, 0x10, 0xC0 | (dst_mod << 3) | (src_mod)])
    }
}

// `MOVSS xmm, m32` -> Load scalar single-precision floating-point value from m32 to xmm register.
#[inline(always)]
fn movss_freg32_rip_offset32(buf: &mut Vec<'_, u8>, dst: X86_64FloatReg, offset: u32) {
    let dst_mod = dst as u8 % 8;
    if dst as u8 > 7 {
        buf.reserve(9);
        buf.extend([0xF3, 0x44, 0x0F, 0x10, 0x05 | (dst_mod << 3)]);
    } else {
        buf.reserve(8);
        buf.extend([0xF3, 0x0F, 0x10, 0x05 | (dst_mod << 3)]);
    }
    buf.extend(offset.to_le_bytes());
}

// `MOVSD xmm, m64` -> Load scalar double-precision floating-point value from m64 to xmm register.
#[inline(always)]
fn movsd_freg64_rip_offset32(buf: &mut Vec<'_, u8>, dst: X86_64FloatReg, offset: u32) {
    let dst_mod = dst as u8 % 8;
    if dst as u8 > 7 {
        buf.reserve(9);
        buf.extend([0xF2, 0x44, 0x0F, 0x10, 0x05 | (dst_mod << 3)]);
    } else {
        buf.reserve(8);
        buf.extend([0xF2, 0x0F, 0x10, 0x05 | (dst_mod << 3)]);
    }
    buf.extend(offset.to_le_bytes());
}

// `MOVSD r/m64,xmm1` -> Move xmm1 to r/m64. where m64 references the base pointer.
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
    buf.extend([0x0F, 0x11, 0x80 | src_mod | base_mod]);
    // Using RSP or R12 requires a secondary index byte.
    if base == X86_64GeneralReg::RSP || base == X86_64GeneralReg::R12 {
        buf.push(0x24);
    }
    buf.extend(offset.to_le_bytes());
}

// `MOVSS r/m64,xmm1` -> Move xmm1 to r/m64. where m64 references the base pointer.
#[inline(always)]
fn movss_base32_offset32_freg32(
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
    buf.push(0xF3);
    if src as u8 > 7 || base as u8 > 7 {
        buf.push(rex);
    }
    buf.extend([0x0F, 0x11, 0x80 | src_mod | base_mod]);
    // Using RSP or R12 requires a secondary index byte.
    if base == X86_64GeneralReg::RSP || base == X86_64GeneralReg::R12 {
        buf.push(0x24);
    }
    buf.extend(offset.to_le_bytes());
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
    buf.extend([0x0F, 0x10, 0x80 | dst_mod | base_mod]);
    // Using RSP or R12 requires a secondary index byte.
    if base == X86_64GeneralReg::RSP || base == X86_64GeneralReg::R12 {
        buf.push(0x24);
    }
    buf.extend(offset.to_le_bytes());
}

/// `MOVSS xmm1,r/m32` -> Move r/m32 to xmm1. where m64 references the base pointer.
#[inline(always)]
fn movss_freg32_base32_offset32(
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
    buf.push(0xF3);
    if dst as u8 > 7 || base as u8 > 7 {
        buf.push(rex);
    }
    buf.extend([0x0F, 0x10, 0x80 | dst_mod | base_mod]);
    // Using RSP or R12 requires a secondary index byte.
    if base == X86_64GeneralReg::RSP || base == X86_64GeneralReg::R12 {
        buf.push(0x24);
    }
    buf.extend(offset.to_le_bytes());
}

/// `NEG r/m64` -> Two's complement negate r/m64.
#[inline(always)]
fn neg_reg64(buf: &mut Vec<'_, u8>, reg: X86_64GeneralReg) {
    let rex = add_rm_extension(reg, REX_W);
    let reg_mod = reg as u8 % 8;
    buf.extend([rex, 0xF7, 0xD8 | reg_mod]);
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
        RAX | RCX | RDX | RBX => buf.extend([0x0F, op_code, 0xC0 | reg_mod]),
        RSP | RBP | RSI | RDI => buf.extend([REX, 0x0F, op_code, 0xC0 | reg_mod]),
        R8 | R9 | R10 | R11 | R12 | R13 | R14 | R15 => {
            buf.extend([REX | 1, 0x0F, op_code, 0xC0 | reg_mod])
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

    buf.extend([op_code1, rex, 0x0F, op_code2, 0xC0 | mod1 | mod2])
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

    buf.extend([op_code1, 0x0F, op_code2, 0xC0 | mod1 | mod2])
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

/// `SETB r/m64` -> Set byte if less (SF≠ OF).
#[inline(always)]
fn setb_reg64(buf: &mut Vec<'_, u8>, reg: X86_64GeneralReg) {
    set_reg64_help(0x92, buf, reg);
}

/// `SETG r/m64` -> Set byte if greater (ZF=0 and SF=OF).
#[inline(always)]
fn setg_reg64(buf: &mut Vec<'_, u8>, reg: X86_64GeneralReg) {
    set_reg64_help(0x9f, buf, reg);
}

/// `SETA r/m64` -> Set byte if above (CF=0 and ZF=0).
#[inline(always)]
fn seta_reg64(buf: &mut Vec<'_, u8>, reg: X86_64GeneralReg) {
    set_reg64_help(0x97, buf, reg);
}

/// `SETAE r/m64` -> Set byte if above or equal (CF=0).
#[inline(always)]
fn setae_reg64(buf: &mut Vec<'_, u8>, reg: X86_64GeneralReg) {
    set_reg64_help(0x93, buf, reg);
}

/// `SETBE r/m64` -> Set byte if below or equal (CF=1 or ZF=1).
#[inline(always)]
fn setbe_reg64(buf: &mut Vec<'_, u8>, reg: X86_64GeneralReg) {
    set_reg64_help(0x96, buf, reg);
}

/// `SETLE r/m64` -> Set byte if less or equal (ZF=1 or SF ≠ OF).
#[inline(always)]
fn setle_reg64(buf: &mut Vec<'_, u8>, reg: X86_64GeneralReg) {
    set_reg64_help(0x9e, buf, reg);
}

/// `SETGE r/m64` -> Set byte if greater or equal (SF=OF).
#[inline(always)]
fn setge_reg64(buf: &mut Vec<'_, u8>, reg: X86_64GeneralReg) {
    set_reg64_help(0x9d, buf, reg);
}

/// `SETO r/m64` -> Set byte if overflow flag is set.
#[inline(always)]
fn seto_reg64(buf: &mut Vec<'_, u8>, reg: X86_64GeneralReg) {
    set_reg64_help(0x90, buf, reg);
}

/// `SETP r/m64` -> Set byte if parity (PF=1).
#[inline(always)]
fn setp_reg64(buf: &mut Vec<'_, u8>, reg: X86_64GeneralReg) {
    set_reg64_help(0x9A, buf, reg);
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
    buf.extend([rex, 0x81, 0xE8 | dst_mod]);
    buf.extend(imm.to_le_bytes());
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
        buf.extend([rex, 0x58 | reg_mod]);
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
        buf.extend([rex, 0x50 | reg_mod]);
    } else {
        buf.push(0x50 | reg_mod);
    }
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
        fn low_32bits_string(&self) -> &str {
            match self {
                X86_64GeneralReg::RAX => "eax",
                X86_64GeneralReg::RBX => "ebx",
                X86_64GeneralReg::RCX => "ecx",
                X86_64GeneralReg::RDX => "edx",
                X86_64GeneralReg::RBP => "ebp",
                X86_64GeneralReg::RSP => "esp",
                X86_64GeneralReg::RSI => "esi",
                X86_64GeneralReg::RDI => "edi",
                X86_64GeneralReg::R8 => "r8d",
                X86_64GeneralReg::R9 => "r9d",
                X86_64GeneralReg::R10 => "r10d",
                X86_64GeneralReg::R11 => "r11d",
                X86_64GeneralReg::R12 => "r12d",
                X86_64GeneralReg::R13 => "r13d",
                X86_64GeneralReg::R14 => "r14d",
                X86_64GeneralReg::R15 => "r15d",
            }
        }

        #[allow(dead_code)]
        fn low_16bits_string(&self) -> &str {
            match self {
                X86_64GeneralReg::RAX => "ax",
                X86_64GeneralReg::RBX => "bx",
                X86_64GeneralReg::RCX => "cx",
                X86_64GeneralReg::RDX => "dx",
                X86_64GeneralReg::RBP => "bp",
                X86_64GeneralReg::RSP => "sp",
                X86_64GeneralReg::RSI => "si",
                X86_64GeneralReg::RDI => "di",
                X86_64GeneralReg::R8 => "r8w",
                X86_64GeneralReg::R9 => "r9w",
                X86_64GeneralReg::R10 => "r10w",
                X86_64GeneralReg::R11 => "r11w",
                X86_64GeneralReg::R12 => "r12w",
                X86_64GeneralReg::R13 => "r13w",
                X86_64GeneralReg::R14 => "r14w",
                X86_64GeneralReg::R15 => "r15w",
            }
        }

        #[allow(dead_code)]
        fn low_8bits_string(&self) -> &str {
            match self {
                X86_64GeneralReg::RAX => "al",
                X86_64GeneralReg::RBX => "bl",
                X86_64GeneralReg::RCX => "cl",
                X86_64GeneralReg::RDX => "dl",
                X86_64GeneralReg::RBP => "bpl",
                X86_64GeneralReg::RSP => "spl",
                X86_64GeneralReg::RSI => "sil",
                X86_64GeneralReg::RDI => "dil",

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

    const ALL_REGISTER_WIDTHS: &[RegisterWidth] = &[
        RegisterWidth::W8,
        RegisterWidth::W16,
        RegisterWidth::W32,
        RegisterWidth::W64,
    ];

    const ALL_GENERAL_REGS: &[X86_64GeneralReg] = &[
        X86_64GeneralReg::RAX,
        X86_64GeneralReg::RBX,
        X86_64GeneralReg::RCX,
        X86_64GeneralReg::RDX,
        X86_64GeneralReg::RBP,
        X86_64GeneralReg::RSP,
        X86_64GeneralReg::RSI,
        X86_64GeneralReg::RDI,
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
            |reg, imm| format!("add {reg}, 0x{imm:x}"),
            ALL_GENERAL_REGS,
            [TEST_I32]
        );
    }

    #[test]
    fn test_add_reg64_reg64() {
        disassembler_test!(
            add_reg64_reg64,
            |reg1, reg2| format!("add {reg1}, {reg2}"),
            ALL_GENERAL_REGS,
            ALL_GENERAL_REGS
        );
    }

    #[test]
    fn test_sub_reg64_reg64() {
        disassembler_test!(
            sub_reg64_reg64,
            |reg1, reg2| format!("sub {reg1}, {reg2}"),
            ALL_GENERAL_REGS,
            ALL_GENERAL_REGS
        );
    }

    #[test]
    fn test_addsd_freg64_freg64() {
        disassembler_test!(
            addsd_freg64_freg64,
            |reg1, reg2| format!("addsd {reg1}, {reg2}"),
            ALL_FLOAT_REGS,
            ALL_FLOAT_REGS
        );
    }

    #[test]
    fn test_addss_freg32_freg32() {
        disassembler_test!(
            addss_freg32_freg32,
            |reg1, reg2| format!("addss {reg1}, {reg2}"),
            ALL_FLOAT_REGS,
            ALL_FLOAT_REGS
        );
    }

    #[test]
    fn test_andpd_freg64_freg64() {
        disassembler_test!(
            andpd_freg64_freg64,
            |reg1, reg2| format!("andpd {reg1}, {reg2}"),
            ALL_FLOAT_REGS,
            ALL_FLOAT_REGS
        );
    }

    #[test]
    fn test_andps_freg32_freg32() {
        disassembler_test!(
            andps_freg32_freg32,
            |reg1, reg2| format!("andps {reg1}, {reg2}"),
            ALL_FLOAT_REGS,
            ALL_FLOAT_REGS
        );
    }

    #[test]
    fn test_and_reg64_reg64() {
        disassembler_test!(
            and_reg64_reg64,
            |reg1, reg2| format!("and {reg1}, {reg2}"),
            ALL_GENERAL_REGS,
            ALL_GENERAL_REGS
        );
    }

    #[test]
    fn test_or_reg64_reg64() {
        disassembler_test!(
            or_reg64_reg64,
            |reg1, reg2| format!("or {reg1}, {reg2}"),
            ALL_GENERAL_REGS,
            ALL_GENERAL_REGS
        );
    }

    #[test]
    fn test_xor_reg64_reg64() {
        disassembler_test!(
            xor_reg64_reg64,
            |reg1, reg2| format!("xor {reg1}, {reg2}"),
            ALL_GENERAL_REGS,
            ALL_GENERAL_REGS
        );
    }

    #[test]
    fn test_shl_reg64_reg64() {
        disassembler_test!(
            shl_reg64_reg64,
            |reg| format!("shl {reg}, cl"),
            ALL_GENERAL_REGS
        );
    }

    #[test]
    fn test_shr_reg64_reg64() {
        disassembler_test!(
            shr_reg64_reg64,
            |reg| format!("shr {reg}, cl"),
            ALL_GENERAL_REGS
        );
    }

    #[test]
    fn test_sar_reg64_reg64() {
        disassembler_test!(
            sar_reg64_reg64,
            |reg| format!("sar {reg}, cl"),
            ALL_GENERAL_REGS
        );
    }

    #[test]
    fn test_cmovl_reg64_reg64() {
        disassembler_test!(
            cmovl_reg64_reg64,
            |reg1, reg2| format!("cmovl {reg1}, {reg2}"),
            ALL_GENERAL_REGS,
            ALL_GENERAL_REGS
        );
    }

    #[test]
    fn test_cmp_reg64_imm32() {
        disassembler_test!(
            cmp_reg64_imm32,
            |reg, imm| format!("cmp {reg}, 0x{imm:x}"),
            ALL_GENERAL_REGS,
            [TEST_I32]
        );
    }

    #[test]
    fn test_imul_reg64_reg64() {
        disassembler_test!(
            imul_reg64_reg64,
            |reg1, reg2| format!("imul {reg1}, {reg2}"),
            ALL_GENERAL_REGS,
            ALL_GENERAL_REGS
        );
    }

    #[test]
    fn test_mul_reg64_reg64() {
        disassembler_test!(
            mul_reg64_reg64,
            |reg| format!("mul {reg}"),
            ALL_GENERAL_REGS
        );
    }

    #[test]
    fn test_mulsd_freg64_freg64() {
        disassembler_test!(
            mulsd_freg64_freg64,
            |reg1, reg2| format!("mulsd {reg1}, {reg2}"),
            ALL_FLOAT_REGS,
            ALL_FLOAT_REGS
        );
    }

    #[test]
    fn test_mulss_freg32_freg32() {
        disassembler_test!(
            mulss_freg32_freg32,
            |reg1, reg2| format!("mulss {reg1}, {reg2}"),
            ALL_FLOAT_REGS,
            ALL_FLOAT_REGS
        );
    }

    #[test]
    fn test_idiv_reg64_reg64() {
        disassembler_test!(
            idiv_reg64_reg64,
            |reg| format!("cqo\nidiv {reg}"),
            ALL_GENERAL_REGS
        );
    }

    #[test]
    fn test_div_reg64_reg64() {
        disassembler_test!(
            udiv_reg64_reg64,
            |reg| format!("cqo\ndiv {reg}"),
            ALL_GENERAL_REGS
        );
    }

    #[test]
    fn test_divsd_freg64_freg64() {
        disassembler_test!(
            divsd_freg64_freg64,
            |reg1, reg2| format!("divsd {reg1}, {reg2}"),
            ALL_FLOAT_REGS,
            ALL_FLOAT_REGS
        );
    }

    #[test]
    fn test_divss_freg32_freg32() {
        disassembler_test!(
            divss_freg32_freg32,
            |reg1, reg2| format!("divss {reg1}, {reg2}"),
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
    fn test_jmp_reg64_offset8() {
        disassembler_test!(
            jmp_reg64_offset8,
            |base, offset| if offset < 0x10 {
                format!("jmp qword ptr [{base} + {offset:x}]")
            } else {
                format!("jmp qword ptr [{base} + 0x{offset:x}]")
            },
            ALL_GENERAL_REGS,
            [0x8, 0x10]
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
            |reg, imm| format!("mov {reg}, 0x{imm:x}"),
            ALL_GENERAL_REGS,
            [TEST_I32]
        );
    }

    #[test]
    fn test_mov_reg64_imm64() {
        disassembler_test!(
            mov_reg64_imm64,
            |reg, imm| format!("movabs {reg}, 0x{imm:x}"),
            ALL_GENERAL_REGS,
            [TEST_I64]
        );
        disassembler_test!(
            mov_reg64_imm64,
            |reg, imm| format!("mov {reg}, 0x{imm:x}"),
            ALL_GENERAL_REGS,
            [TEST_I32 as i64]
        );
    }

    #[test]
    fn test_lea_reg64() {
        disassembler_test!(
            lea_reg64,
            |reg| format!("lea {reg}, [rip]"),
            ALL_GENERAL_REGS
        );
    }

    #[test]
    fn test_lea_reg64_offset32() {
        disassembler_test!(
            lea_reg64_offset8,
            |dst, src, offset| {
                if offset < 16 {
                    format!("lea {dst}, [{src} + {offset:x}]")
                } else {
                    format!("lea {dst}, [{src} + 0x{offset:x}]")
                }
            },
            ALL_GENERAL_REGS,
            ALL_GENERAL_REGS,
            [0x8i8, 0x10i8]
        );
    }

    #[test]
    fn test_mov_reg64_reg64() {
        disassembler_test!(
            raw_mov_reg_reg,
            |w, reg1, reg2| {
                match w {
                    RegisterWidth::W8 => format!(
                        "mov {}, {}",
                        X86_64GeneralReg::low_8bits_string(&reg1),
                        X86_64GeneralReg::low_8bits_string(&reg2)
                    ),
                    RegisterWidth::W16 => format!(
                        "mov {}, {}",
                        X86_64GeneralReg::low_16bits_string(&reg1),
                        X86_64GeneralReg::low_16bits_string(&reg2)
                    ),
                    RegisterWidth::W32 => format!(
                        "mov {}, {}",
                        X86_64GeneralReg::low_32bits_string(&reg1),
                        X86_64GeneralReg::low_32bits_string(&reg2)
                    ),
                    RegisterWidth::W64 => format!("mov {reg1}, {reg2}"),
                }
            },
            ALL_REGISTER_WIDTHS,
            ALL_GENERAL_REGS,
            ALL_GENERAL_REGS
        );
    }

    #[test]
    fn test_movsx_reg64_reg64() {
        disassembler_test!(
            raw_movsx_reg_reg,
            |w, reg1, reg2| {
                match w {
                    RegisterWidth::W8 => format!(
                        "movsx {}, {}",
                        reg1,
                        X86_64GeneralReg::low_8bits_string(&reg2)
                    ),
                    RegisterWidth::W16 => format!(
                        "movsx {}, {}",
                        reg1,
                        X86_64GeneralReg::low_16bits_string(&reg2)
                    ),
                    RegisterWidth::W32 => format!(
                        "movsxd {}, {}",
                        reg1,
                        X86_64GeneralReg::low_32bits_string(&reg2)
                    ),
                    RegisterWidth::W64 => String::new(),
                }
            },
            ALL_REGISTER_WIDTHS,
            ALL_GENERAL_REGS,
            ALL_GENERAL_REGS
        );
    }

    #[test]
    fn test_movzx_reg64_reg64() {
        disassembler_test!(
            raw_movzx_reg_reg,
            |w, reg1, reg2| {
                match w {
                    RegisterWidth::W8 => format!(
                        "movzx {}, {}",
                        reg1,
                        X86_64GeneralReg::low_8bits_string(&reg2)
                    ),
                    RegisterWidth::W16 => format!(
                        "movzx {}, {}",
                        reg1,
                        X86_64GeneralReg::low_16bits_string(&reg2)
                    ),
                    RegisterWidth::W32 => String::new(),
                    RegisterWidth::W64 => String::new(),
                }
            },
            ALL_REGISTER_WIDTHS,
            ALL_GENERAL_REGS,
            ALL_GENERAL_REGS
        );
    }

    #[test]
    fn test_movsd_freg64_base64_offset32() {
        disassembler_test!(
            movsd_freg64_base64_offset32,
            |reg1, reg2, imm| format!("movsd {reg1}, qword ptr [{reg2} + 0x{imm:x}]"),
            ALL_FLOAT_REGS,
            ALL_GENERAL_REGS,
            [TEST_I32]
        );
    }

    #[test]
    fn test_movss_freg32_base32_offset32() {
        disassembler_test!(
            movss_freg32_base32_offset32,
            |reg1, reg2, imm| format!("movss {reg1}, dword ptr [{reg2} + 0x{imm:x}]"),
            ALL_FLOAT_REGS,
            ALL_GENERAL_REGS,
            [TEST_I32]
        );
    }

    #[test]
    fn test_movsd_base64_offset32_freg64() {
        disassembler_test!(
            movsd_base64_offset32_freg64,
            |reg1, imm, reg2| format!("movsd qword ptr [{reg1} + 0x{imm:x}], {reg2}"),
            ALL_GENERAL_REGS,
            [TEST_I32],
            ALL_FLOAT_REGS
        );
    }

    #[test]
    fn test_movss_base64_offset32_freg64() {
        disassembler_test!(
            movss_base32_offset32_freg32,
            |reg1, imm, reg2| format!("movss dword ptr [{} + 0x{:x}], {}", reg1, imm, reg2),
            ALL_GENERAL_REGS,
            [TEST_I32],
            ALL_FLOAT_REGS
        );
    }

    #[test]
    fn test_mov_reg64_base64_offset32() {
        disassembler_test!(
            mov_reg64_base64_offset32,
            |reg1, reg2, imm| format!("mov {reg1}, qword ptr [{reg2} + 0x{imm:x}]"),
            ALL_GENERAL_REGS,
            ALL_GENERAL_REGS,
            [TEST_I32]
        );
    }

    #[test]
    fn test_mov_reg32_base32_offset32() {
        disassembler_test!(
            mov_reg32_base32_offset32,
            |reg1, reg2, imm| format!(
                "mov {}, dword ptr [{} + 0x{:x}]",
                X86_64GeneralReg::low_32bits_string(&reg1),
                reg2,
                imm
            ),
            ALL_GENERAL_REGS,
            ALL_GENERAL_REGS,
            [TEST_I32]
        );
    }

    #[test]
    fn test_mov_reg16_base16_offset32() {
        disassembler_test!(
            mov_reg16_base16_offset32,
            |reg1, reg2, imm| format!(
                "mov {}, word ptr [{} + 0x{:x}]",
                X86_64GeneralReg::low_16bits_string(&reg1),
                reg2,
                imm
            ),
            ALL_GENERAL_REGS,
            ALL_GENERAL_REGS,
            [TEST_I32]
        );
    }

    #[test]
    fn test_mov_reg8_base8_offset32() {
        disassembler_test!(
            mov_reg8_base8_offset32,
            |reg1, reg2, imm| format!(
                "mov {}, byte ptr [{} + 0x{:x}]",
                X86_64GeneralReg::low_8bits_string(&reg1),
                reg2,
                imm
            ),
            ALL_GENERAL_REGS,
            ALL_GENERAL_REGS,
            [TEST_I32]
        );
    }

    #[test]
    fn test_mov_base64_offset32_reg64() {
        disassembler_test!(
            mov_base64_offset32_reg64,
            |reg1, imm, reg2| format!("mov qword ptr [{reg1} + 0x{imm:x}], {reg2}"),
            ALL_GENERAL_REGS,
            [TEST_I32],
            ALL_GENERAL_REGS
        );
    }

    #[test]
    fn test_mov_base32_offset32_reg32() {
        disassembler_test!(
            mov_base32_offset32_reg32,
            |reg1, imm, reg2| format!(
                "mov dword ptr [{} + 0x{:x}], {}",
                reg1,
                imm,
                X86_64GeneralReg::low_32bits_string(&reg2),
            ),
            ALL_GENERAL_REGS,
            [TEST_I32],
            ALL_GENERAL_REGS
        );
    }

    #[test]
    fn test_mov_base16_offset32_reg16() {
        disassembler_test!(
            mov_base16_offset32_reg16,
            |reg1, imm, reg2| format!(
                "mov word ptr [{} + 0x{:x}], {}",
                reg1,
                imm,
                X86_64GeneralReg::low_16bits_string(&reg2),
            ),
            ALL_GENERAL_REGS,
            [TEST_I32],
            ALL_GENERAL_REGS
        );
    }

    #[test]
    fn test_mov_base8_offset32_reg8() {
        disassembler_test!(
            mov_base8_offset32_reg8,
            |reg1, imm, reg2| format!(
                "mov byte ptr [{} + 0x{:x}], {}",
                reg1,
                imm,
                X86_64GeneralReg::low_8bits_string(&reg2),
            ),
            ALL_GENERAL_REGS,
            [TEST_I32],
            ALL_GENERAL_REGS
        );
    }

    #[test]
    fn test_movsx_reg64_base32_offset32() {
        disassembler_test!(
            movsx_reg64_base32_offset32,
            |reg1, reg2, imm| format!("movsxd {reg1}, dword ptr [{reg2} + 0x{imm:x}]"),
            ALL_GENERAL_REGS,
            ALL_GENERAL_REGS,
            [TEST_I32]
        );
    }

    #[test]
    fn test_movsx_reg64_base16_offset32() {
        disassembler_test!(
            movsx_reg64_base16_offset32,
            |reg1, reg2, imm| format!("movsx {reg1}, word ptr [{reg2} + 0x{imm:x}]"),
            ALL_GENERAL_REGS,
            ALL_GENERAL_REGS,
            [TEST_I32]
        );
    }

    #[test]
    fn test_movsx_reg64_base8_offset32() {
        disassembler_test!(
            movsx_reg64_base8_offset32,
            |reg1, reg2, imm| format!("movsx {reg1}, byte ptr [{reg2} + 0x{imm:x}]"),
            ALL_GENERAL_REGS,
            ALL_GENERAL_REGS,
            [TEST_I32]
        );
    }

    #[test]
    fn test_movzx_reg64_base16_offset32() {
        disassembler_test!(
            movzx_reg64_base16_offset32,
            |reg1, reg2, imm| format!("movzx {reg1}, word ptr [{reg2} + 0x{imm:x}]"),
            ALL_GENERAL_REGS,
            ALL_GENERAL_REGS,
            [TEST_I32]
        );
    }

    #[test]
    fn test_movzx_reg64_base8_offset32() {
        disassembler_test!(
            movzx_reg64_base8_offset32,
            |reg1, reg2, imm| format!("movzx {reg1}, byte ptr [{reg2} + 0x{imm:x}]"),
            ALL_GENERAL_REGS,
            ALL_GENERAL_REGS,
            [TEST_I32]
        );
    }

    #[test]
    fn test_movd_reg32_freg32() {
        disassembler_test!(
            movd_reg32_freg32,
            |dst: X86_64GeneralReg, src| format!("movd {}, {}", dst.low_32bits_string(), src),
            ALL_GENERAL_REGS,
            ALL_FLOAT_REGS
        );
    }

    #[test]
    fn test_movq_reg64_freg64() {
        disassembler_test!(
            movq_reg64_freg64,
            |dst, src| format!("movq {dst}, {src}"),
            ALL_GENERAL_REGS,
            ALL_FLOAT_REGS
        );
    }

    #[test]
    fn test_movsd_freg64_freg64() {
        disassembler_test!(
            raw_movsd_freg64_freg64,
            |reg1, reg2| format!("movsd {reg1}, {reg2}"),
            ALL_FLOAT_REGS,
            ALL_FLOAT_REGS
        );
    }

    #[test]
    fn test_movss_freg32_freg32() {
        disassembler_test!(
            raw_movss_freg32_freg32,
            |reg1, reg2| format!("movss {reg1}, {reg2}"),
            ALL_FLOAT_REGS,
            ALL_FLOAT_REGS
        );
    }

    #[test]
    fn test_movss_freg32_rip_offset32() {
        disassembler_test!(
            movss_freg32_rip_offset32,
            |reg, imm| format!("movss {reg}, dword ptr [rip + 0x{imm:x}]"),
            ALL_FLOAT_REGS,
            [TEST_I32 as u32]
        );
    }

    #[test]
    fn test_movsd_freg64_rip_offset32() {
        disassembler_test!(
            movsd_freg64_rip_offset32,
            |reg, imm| format!("movsd {reg}, qword ptr [rip + 0x{imm:x}]"),
            ALL_FLOAT_REGS,
            [TEST_I32 as u32]
        );
    }

    #[test]
    fn test_neg_reg64() {
        disassembler_test!(neg_reg64, |reg| format!("neg {reg}"), ALL_GENERAL_REGS);
    }

    #[test]
    fn test_cvtsi2_help() {
        const CVTSI2SS_CODE: u8 = 0x2A;
        const CVTTSS2SI_CODE: u8 = 0x2C;
        disassembler_test!(
            |buf, r1, r2| cvtsi2_help(buf, 0xF3, CVTSI2SS_CODE, r1, r2),
            |reg1, reg2| format!("cvtsi2ss {reg1}, {reg2}"),
            ALL_FLOAT_REGS,
            ALL_GENERAL_REGS
        );
        disassembler_test!(
            |buf, r1, r2| cvtsi2_help(buf, 0xF3, CVTTSS2SI_CODE, r1, r2),
            |reg1, reg2| format!("cvttss2si {reg1}, {reg2}"),
            ALL_GENERAL_REGS,
            ALL_FLOAT_REGS
        );
    }

    #[test]
    fn test_cvtsx2_help() {
        const CVTSS2SD_CODE: u8 = 0x5A;
        disassembler_test!(
            |buf, r1, r2| cvtsi2_help(buf, 0xF3, CVTSS2SD_CODE, r1, r2),
            |reg1, reg2| format!("cvtss2sd {reg1}, {reg2}"),
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
            |reg, imm| format!("sub {reg}, 0x{imm:x}"),
            ALL_GENERAL_REGS,
            [TEST_I32]
        );
    }

    #[test]
    fn test_pop_reg64() {
        disassembler_test!(pop_reg64, |reg| format!("pop {reg}"), ALL_GENERAL_REGS);
    }

    #[test]
    fn test_push_reg64() {
        disassembler_test!(push_reg64, |reg| format!("push {reg}"), ALL_GENERAL_REGS);
    }

    #[test]
    fn test_sqrt_freg64_freg64() {
        disassembler_test!(
            sqrtsd_freg64_freg64,
            |dst, src| format!("sqrtsd {dst}, {src}"),
            ALL_FLOAT_REGS,
            ALL_FLOAT_REGS
        );
    }

    #[test]
    fn test_sqrt_freg32_freg32() {
        disassembler_test!(
            sqrtss_freg32_freg32,
            |dst, src| format!("sqrtss {dst}, {src}"),
            ALL_FLOAT_REGS,
            ALL_FLOAT_REGS
        );
    }

    #[test]
    fn test_int_cmp() {
        disassembler_test!(
            cmp_reg64_reg64,
            |_, dst: X86_64GeneralReg, src: X86_64GeneralReg| format!(
                "cmp {}, {}",
                dst.low_8bits_string(),
                src.low_8bits_string()
            ),
            [RegisterWidth::W8],
            ALL_GENERAL_REGS,
            ALL_GENERAL_REGS
        );

        disassembler_test!(
            cmp_reg64_reg64,
            |_, dst: X86_64GeneralReg, src: X86_64GeneralReg| format!(
                "cmp {}, {}",
                dst.low_16bits_string(),
                src.low_16bits_string()
            ),
            [RegisterWidth::W16],
            ALL_GENERAL_REGS,
            ALL_GENERAL_REGS
        );

        disassembler_test!(
            cmp_reg64_reg64,
            |_, dst: X86_64GeneralReg, src: X86_64GeneralReg| format!(
                "cmp {}, {}",
                dst.low_32bits_string(),
                src.low_32bits_string()
            ),
            [RegisterWidth::W32],
            ALL_GENERAL_REGS,
            ALL_GENERAL_REGS
        );

        disassembler_test!(
            cmp_reg64_reg64,
            |_, dst: X86_64GeneralReg, src: X86_64GeneralReg| format!("cmp {dst}, {src}",),
            [RegisterWidth::W64],
            ALL_GENERAL_REGS,
            ALL_GENERAL_REGS
        );
    }
}
