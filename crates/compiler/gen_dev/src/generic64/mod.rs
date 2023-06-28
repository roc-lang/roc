use crate::{
    pointer_layouts, single_register_floats, single_register_int_builtins,
    single_register_integers, Backend, Env, Relocation,
};
use bumpalo::collections::{CollectIn, Vec};
use roc_builtins::bitcode::{self, FloatWidth, IntWidth};
use roc_collections::all::MutMap;
use roc_error_macros::internal_error;
use roc_module::symbol::{Interns, ModuleId, Symbol};
use roc_mono::code_gen_help::{CallerProc, CodeGenHelp, HelperOp};
use roc_mono::ir::{
    BranchInfo, HigherOrderLowLevel, JoinPointId, ListLiteralElement, Literal, Param, ProcLayout,
    SelfRecursive, Stmt,
};
use roc_mono::layout::{
    Builtin, InLayout, LambdaName, Layout, LayoutIds, LayoutInterner, LayoutRepr, STLayoutInterner,
    TagIdIntType, UnionLayout,
};
use roc_mono::low_level::HigherOrder;
use roc_target::TargetInfo;
use std::marker::PhantomData;

pub(crate) mod aarch64;
#[cfg(test)]
mod disassembler_test_macro;
pub(crate) mod storage;
pub(crate) mod x86_64;

use storage::{RegStorage, StorageManager};

// TODO: on all number functions double check and deal with over/underflow.

#[derive(Debug, Clone, Copy)]
pub enum RegisterWidth {
    W8,
    W16,
    W32,
    W64,
}

impl RegisterWidth {
    fn try_from_layout(layout: LayoutRepr) -> Option<Self> {
        match layout {
            LayoutRepr::BOOL | LayoutRepr::I8 | LayoutRepr::U8 => Some(RegisterWidth::W8),
            LayoutRepr::I16 | LayoutRepr::U16 => Some(RegisterWidth::W16),
            LayoutRepr::U32 | LayoutRepr::I32 => Some(RegisterWidth::W32),
            LayoutRepr::I64 | LayoutRepr::U64 => Some(RegisterWidth::W64),
            _ => None,
        }
    }
}

pub trait CallConv<GeneralReg: RegTrait, FloatReg: RegTrait, ASM: Assembler<GeneralReg, FloatReg>>:
    Sized + Copy
{
    const BASE_PTR_REG: GeneralReg;
    const STACK_PTR_REG: GeneralReg;

    const GENERAL_PARAM_REGS: &'static [GeneralReg];
    const GENERAL_RETURN_REGS: &'static [GeneralReg];
    const GENERAL_DEFAULT_FREE_REGS: &'static [GeneralReg];

    const FLOAT_PARAM_REGS: &'static [FloatReg];
    const FLOAT_RETURN_REGS: &'static [FloatReg];
    const FLOAT_DEFAULT_FREE_REGS: &'static [FloatReg];

    const SHADOW_SPACE_SIZE: u8;

    fn general_callee_saved(reg: &GeneralReg) -> bool;
    #[inline(always)]
    fn general_caller_saved(reg: &GeneralReg) -> bool {
        !Self::general_callee_saved(reg)
    }
    fn float_callee_saved(reg: &FloatReg) -> bool;
    #[inline(always)]
    fn float_caller_saved(reg: &FloatReg) -> bool {
        !Self::float_callee_saved(reg)
    }

    fn setup_stack(
        buf: &mut Vec<'_, u8>,
        general_saved_regs: &[GeneralReg],
        float_saved_regs: &[FloatReg],
        requested_stack_size: i32,
        fn_call_stack_size: i32,
    ) -> i32;
    fn cleanup_stack(
        buf: &mut Vec<'_, u8>,
        general_saved_regs: &[GeneralReg],
        float_saved_regs: &[FloatReg],
        aligned_stack_size: i32,
        fn_call_stack_size: i32,
    );

    /// load_args updates the storage manager to know where every arg is stored.
    fn load_args<'a>(
        buf: &mut Vec<'a, u8>,
        storage_manager: &mut StorageManager<'a, '_, GeneralReg, FloatReg, ASM, Self>,
        layout_interner: &mut STLayoutInterner<'a>,
        args: &'a [(InLayout<'a>, Symbol)],
        // ret_layout is needed because if it is a complex type, we pass a pointer as the first arg.
        ret_layout: &InLayout<'a>,
    );

    /// store_args stores the args in registers and on the stack for function calling.
    /// It also updates the amount of temporary stack space needed in the storage manager.
    fn store_args<'a>(
        buf: &mut Vec<'a, u8>,
        storage_manager: &mut StorageManager<'a, '_, GeneralReg, FloatReg, ASM, Self>,
        layout_interner: &mut STLayoutInterner<'a>,
        dst: &Symbol,
        args: &[Symbol],
        arg_layouts: &[InLayout<'a>],
        // ret_layout is needed because if it is a complex type, we pass a pointer as the first arg.
        ret_layout: &InLayout<'a>,
    );

    /// return_complex_symbol returns the specified complex/non-primative symbol.
    /// It uses the layout to determine how the data should be returned.
    fn return_complex_symbol<'a>(
        buf: &mut Vec<'a, u8>,
        storage_manager: &mut StorageManager<'a, '_, GeneralReg, FloatReg, ASM, Self>,
        layout_interner: &mut STLayoutInterner<'a>,
        sym: &Symbol,
        layout: &InLayout<'a>,
    );

    /// load_returned_complex_symbol loads a complex symbol that was returned from a function call.
    /// It uses the layout to determine how the data should be loaded into the symbol.
    fn load_returned_complex_symbol<'a>(
        buf: &mut Vec<'a, u8>,
        storage_manager: &mut StorageManager<'a, '_, GeneralReg, FloatReg, ASM, Self>,
        layout_interner: &mut STLayoutInterner<'a>,
        sym: &Symbol,
        layout: &InLayout<'a>,
    );
}

pub enum CompareOperation {
    LessThan,
    LessThanOrEqual,
    GreaterThan,
    GreaterThanOrEqual,
}

/// Assembler contains calls to the backend assembly generator.
/// These calls do not necessarily map directly to a single assembly instruction.
/// They are higher level in cases where an instruction would not be common and shared between multiple architectures.
/// Thus, some backends will need to use mulitiple instructions to preform a single one of this calls.
/// Generally, I prefer explicit sources, as opposed to dst being one of the sources. Ex: `x = x + y` would be `add x, x, y` instead of `add x, y`.
/// dst should always come before sources.
pub trait Assembler<GeneralReg: RegTrait, FloatReg: RegTrait>: Sized + Copy {
    fn abs_reg64_reg64(buf: &mut Vec<'_, u8>, dst: GeneralReg, src: GeneralReg);
    fn abs_freg64_freg64(
        buf: &mut Vec<'_, u8>,
        relocs: &mut Vec<'_, Relocation>,
        dst: FloatReg,
        src: FloatReg,
    );

    fn add_reg64_reg64_imm32(buf: &mut Vec<'_, u8>, dst: GeneralReg, src1: GeneralReg, imm32: i32);
    fn add_freg32_freg32_freg32(
        buf: &mut Vec<'_, u8>,
        dst: FloatReg,
        src1: FloatReg,
        src2: FloatReg,
    );
    fn add_freg64_freg64_freg64(
        buf: &mut Vec<'_, u8>,
        dst: FloatReg,
        src1: FloatReg,
        src2: FloatReg,
    );
    fn add_reg64_reg64_reg64(
        buf: &mut Vec<'_, u8>,
        dst: GeneralReg,
        src1: GeneralReg,
        src2: GeneralReg,
    );

    fn and_reg64_reg64_reg64(
        buf: &mut Vec<'_, u8>,
        dst: GeneralReg,
        src1: GeneralReg,
        src2: GeneralReg,
    );

    fn or_reg64_reg64_reg64(
        buf: &mut Vec<'_, u8>,
        dst: GeneralReg,
        src1: GeneralReg,
        src2: GeneralReg,
    );

    fn xor_reg64_reg64_reg64(
        buf: &mut Vec<'_, u8>,
        dst: GeneralReg,
        src1: GeneralReg,
        src2: GeneralReg,
    );

    fn shl_reg64_reg64_reg64<'a, ASM, CC>(
        buf: &mut Vec<'a, u8>,
        storage_manager: &mut StorageManager<'a, '_, GeneralReg, FloatReg, ASM, CC>,
        dst: GeneralReg,
        src1: GeneralReg,
        src2: GeneralReg,
    ) where
        ASM: Assembler<GeneralReg, FloatReg>,
        CC: CallConv<GeneralReg, FloatReg, ASM>;

    fn shr_reg64_reg64_reg64<'a, ASM, CC>(
        buf: &mut Vec<'a, u8>,
        storage_manager: &mut StorageManager<'a, '_, GeneralReg, FloatReg, ASM, CC>,
        dst: GeneralReg,
        src1: GeneralReg,
        src2: GeneralReg,
    ) where
        ASM: Assembler<GeneralReg, FloatReg>,
        CC: CallConv<GeneralReg, FloatReg, ASM>;

    fn sar_reg64_reg64_reg64<'a, ASM, CC>(
        buf: &mut Vec<'a, u8>,
        storage_manager: &mut StorageManager<'a, '_, GeneralReg, FloatReg, ASM, CC>,
        dst: GeneralReg,
        src1: GeneralReg,
        src2: GeneralReg,
    ) where
        ASM: Assembler<GeneralReg, FloatReg>,
        CC: CallConv<GeneralReg, FloatReg, ASM>;

    fn call(buf: &mut Vec<'_, u8>, relocs: &mut Vec<'_, Relocation>, fn_name: String);

    fn function_pointer(
        buf: &mut Vec<'_, u8>,
        relocs: &mut Vec<'_, Relocation>,
        fn_name: String,
        dst: GeneralReg,
    );

    /// Jumps by an offset of offset bytes unconditionally.
    /// It should always generate the same number of bytes to enable replacement if offset changes.
    /// It returns the base offset to calculate the jump from (generally the instruction after the jump).
    fn jmp_imm32(buf: &mut Vec<'_, u8>, offset: i32) -> usize;

    fn tail_call(buf: &mut Vec<'_, u8>) -> u64;

    /// Jumps by an offset of offset bytes if reg is not equal to imm.
    /// It should always generate the same number of bytes to enable replacement if offset changes.
    /// It returns the base offset to calculate the jump from (generally the instruction after the jump).
    fn jne_reg64_imm64_imm32<'a, ASM, CC>(
        buf: &mut Vec<'a, u8>,
        storage_manager: &mut StorageManager<'a, '_, GeneralReg, FloatReg, ASM, CC>,
        reg: GeneralReg,
        imm: u64,
        offset: i32,
    ) -> usize
    where
        ASM: Assembler<GeneralReg, FloatReg>,
        CC: CallConv<GeneralReg, FloatReg, ASM>;

    fn mov_freg32_imm32(
        buf: &mut Vec<'_, u8>,
        relocs: &mut Vec<'_, Relocation>,
        dst: FloatReg,
        imm: f32,
    );
    fn mov_freg64_imm64(
        buf: &mut Vec<'_, u8>,
        relocs: &mut Vec<'_, Relocation>,
        dst: FloatReg,
        imm: f64,
    );
    fn mov_reg64_imm64(buf: &mut Vec<'_, u8>, dst: GeneralReg, imm: i64);
    fn mov_freg64_freg64(buf: &mut Vec<'_, u8>, dst: FloatReg, src: FloatReg);

    fn mov_reg32_freg32(buf: &mut Vec<'_, u8>, dst: GeneralReg, src: FloatReg);
    fn mov_reg64_freg64(buf: &mut Vec<'_, u8>, dst: GeneralReg, src: FloatReg);

    fn mov_reg_reg(
        buf: &mut Vec<'_, u8>,
        register_width: RegisterWidth,
        dst: GeneralReg,
        src: GeneralReg,
    );
    fn mov_reg64_reg64(buf: &mut Vec<'_, u8>, dst: GeneralReg, src: GeneralReg) {
        Self::mov_reg_reg(buf, RegisterWidth::W64, dst, src);
    }
    fn mov_reg32_reg32(buf: &mut Vec<'_, u8>, dst: GeneralReg, src: GeneralReg) {
        Self::mov_reg_reg(buf, RegisterWidth::W32, dst, src);
    }
    fn mov_reg16_reg16(buf: &mut Vec<'_, u8>, dst: GeneralReg, src: GeneralReg) {
        Self::mov_reg_reg(buf, RegisterWidth::W16, dst, src);
    }
    fn mov_reg8_reg8(buf: &mut Vec<'_, u8>, dst: GeneralReg, src: GeneralReg) {
        Self::mov_reg_reg(buf, RegisterWidth::W8, dst, src);
    }

    // move with sign extension
    fn movsx_reg_reg(
        buf: &mut Vec<'_, u8>,
        input_width: RegisterWidth,
        dst: GeneralReg,
        src: GeneralReg,
    );

    // move with zero extension
    fn movzx_reg_reg(
        buf: &mut Vec<'_, u8>,
        input_width: RegisterWidth,
        dst: GeneralReg,
        src: GeneralReg,
    );

    // base32 is similar to stack based instructions but they reference the base/frame pointer.
    fn mov_freg64_base32(buf: &mut Vec<'_, u8>, dst: FloatReg, offset: i32);

    fn mov_reg64_base32(buf: &mut Vec<'_, u8>, dst: GeneralReg, offset: i32);
    fn mov_reg32_base32(buf: &mut Vec<'_, u8>, dst: GeneralReg, offset: i32);
    fn mov_reg16_base32(buf: &mut Vec<'_, u8>, dst: GeneralReg, offset: i32);
    fn mov_reg8_base32(buf: &mut Vec<'_, u8>, dst: GeneralReg, offset: i32);

    fn mov_base32_freg64(buf: &mut Vec<'_, u8>, offset: i32, src: FloatReg);

    fn mov_base32_reg64(buf: &mut Vec<'_, u8>, offset: i32, src: GeneralReg);
    fn mov_base32_reg32(buf: &mut Vec<'_, u8>, offset: i32, src: GeneralReg);
    fn mov_base32_reg16(buf: &mut Vec<'_, u8>, offset: i32, src: GeneralReg);
    fn mov_base32_reg8(buf: &mut Vec<'_, u8>, offset: i32, src: GeneralReg);

    // move from memory (a pointer) to register
    fn mov_reg64_mem64_offset32(
        buf: &mut Vec<'_, u8>,
        dst: GeneralReg,
        src: GeneralReg,
        offset: i32,
    );
    fn mov_reg32_mem32_offset32(
        buf: &mut Vec<'_, u8>,
        dst: GeneralReg,
        src: GeneralReg,
        offset: i32,
    );
    fn mov_reg16_mem16_offset32(
        buf: &mut Vec<'_, u8>,
        dst: GeneralReg,
        src: GeneralReg,
        offset: i32,
    );
    fn mov_reg8_mem8_offset32(buf: &mut Vec<'_, u8>, dst: GeneralReg, src: GeneralReg, offset: i32);

    fn mov_freg64_mem64_offset32(
        buf: &mut Vec<'_, u8>,
        dst: FloatReg,
        src: GeneralReg,
        offset: i32,
    );
    fn mov_freg32_mem32_offset32(
        buf: &mut Vec<'_, u8>,
        dst: FloatReg,
        src: GeneralReg,
        offset: i32,
    );

    // move from register to memory
    fn mov_mem64_offset32_reg64(
        buf: &mut Vec<'_, u8>,
        dst: GeneralReg,
        offset: i32,
        src: GeneralReg,
    );
    fn mov_mem32_offset32_reg32(
        buf: &mut Vec<'_, u8>,
        dst: GeneralReg,
        offset: i32,
        src: GeneralReg,
    );
    fn mov_mem16_offset32_reg16(
        buf: &mut Vec<'_, u8>,
        dst: GeneralReg,
        offset: i32,
        src: GeneralReg,
    );
    fn mov_mem8_offset32_reg8(buf: &mut Vec<'_, u8>, dst: GeneralReg, offset: i32, src: GeneralReg);

    fn movesd_mem64_offset32_freg64(
        buf: &mut Vec<'_, u8>,
        ptr: GeneralReg,
        offset: i32,
        src: FloatReg,
    );

    /// Sign extends the data at `offset` with `size` as it copies it to `dst`
    /// size must be less than or equal to 8.
    fn movsx_reg_base32(
        buf: &mut Vec<'_, u8>,
        register_width: RegisterWidth,
        dst: GeneralReg,
        offset: i32,
    );

    fn movzx_reg_base32(
        buf: &mut Vec<'_, u8>,
        register_width: RegisterWidth,
        dst: GeneralReg,
        offset: i32,
    );

    fn mov_freg64_stack32(buf: &mut Vec<'_, u8>, dst: FloatReg, offset: i32);
    fn mov_reg64_stack32(buf: &mut Vec<'_, u8>, dst: GeneralReg, offset: i32);
    fn mov_stack32_freg64(buf: &mut Vec<'_, u8>, offset: i32, src: FloatReg);

    fn mov_stack32_reg(
        buf: &mut Vec<'_, u8>,
        register_width: RegisterWidth,
        offset: i32,
        src: GeneralReg,
    );

    fn mov_stack32_reg64(buf: &mut Vec<'_, u8>, offset: i32, src: GeneralReg) {
        Self::mov_stack32_reg(buf, RegisterWidth::W64, offset, src)
    }
    fn mov_stack32_reg32(buf: &mut Vec<'_, u8>, offset: i32, src: GeneralReg) {
        Self::mov_stack32_reg(buf, RegisterWidth::W32, offset, src)
    }
    fn mov_stack32_reg16(buf: &mut Vec<'_, u8>, offset: i32, src: GeneralReg) {
        Self::mov_stack32_reg(buf, RegisterWidth::W16, offset, src)
    }
    fn mov_stack32_reg8(buf: &mut Vec<'_, u8>, offset: i32, src: GeneralReg) {
        Self::mov_stack32_reg(buf, RegisterWidth::W8, offset, src)
    }

    fn sqrt_freg64_freg64(buf: &mut Vec<'_, u8>, dst: FloatReg, src: FloatReg);
    fn sqrt_freg32_freg32(buf: &mut Vec<'_, u8>, dst: FloatReg, src: FloatReg);

    fn neg_reg64_reg64(buf: &mut Vec<'_, u8>, dst: GeneralReg, src: GeneralReg);
    fn mul_freg32_freg32_freg32(
        buf: &mut Vec<'_, u8>,
        dst: FloatReg,
        src1: FloatReg,
        src2: FloatReg,
    );
    fn mul_freg64_freg64_freg64(
        buf: &mut Vec<'_, u8>,
        dst: FloatReg,
        src1: FloatReg,
        src2: FloatReg,
    );
    fn div_freg32_freg32_freg32(
        buf: &mut Vec<'_, u8>,
        dst: FloatReg,
        src1: FloatReg,
        src2: FloatReg,
    );
    fn div_freg64_freg64_freg64(
        buf: &mut Vec<'_, u8>,
        dst: FloatReg,
        src1: FloatReg,
        src2: FloatReg,
    );
    fn imul_reg64_reg64_reg64(
        buf: &mut Vec<'_, u8>,
        dst: GeneralReg,
        src1: GeneralReg,
        src2: GeneralReg,
    );
    fn umul_reg64_reg64_reg64<'a, ASM, CC>(
        buf: &mut Vec<'a, u8>,
        storage_manager: &mut StorageManager<'a, '_, GeneralReg, FloatReg, ASM, CC>,
        dst: GeneralReg,
        src1: GeneralReg,
        src2: GeneralReg,
    ) where
        ASM: Assembler<GeneralReg, FloatReg>,
        CC: CallConv<GeneralReg, FloatReg, ASM>;

    fn idiv_reg64_reg64_reg64<'a, ASM, CC>(
        buf: &mut Vec<'a, u8>,
        storage_manager: &mut StorageManager<'a, '_, GeneralReg, FloatReg, ASM, CC>,
        dst: GeneralReg,
        src1: GeneralReg,
        src2: GeneralReg,
    ) where
        ASM: Assembler<GeneralReg, FloatReg>,
        CC: CallConv<GeneralReg, FloatReg, ASM>;

    fn udiv_reg64_reg64_reg64<'a, ASM, CC>(
        buf: &mut Vec<'a, u8>,
        storage_manager: &mut StorageManager<'a, '_, GeneralReg, FloatReg, ASM, CC>,
        dst: GeneralReg,
        src1: GeneralReg,
        src2: GeneralReg,
    ) where
        ASM: Assembler<GeneralReg, FloatReg>,
        CC: CallConv<GeneralReg, FloatReg, ASM>;

    fn irem_reg64_reg64_reg64<'a, ASM, CC>(
        buf: &mut Vec<'a, u8>,
        storage_manager: &mut StorageManager<'a, '_, GeneralReg, FloatReg, ASM, CC>,
        dst: GeneralReg,
        src1: GeneralReg,
        src2: GeneralReg,
    ) where
        ASM: Assembler<GeneralReg, FloatReg>,
        CC: CallConv<GeneralReg, FloatReg, ASM>;

    fn urem_reg64_reg64_reg64<'a, ASM, CC>(
        buf: &mut Vec<'a, u8>,
        storage_manager: &mut StorageManager<'a, '_, GeneralReg, FloatReg, ASM, CC>,
        dst: GeneralReg,
        src1: GeneralReg,
        src2: GeneralReg,
    ) where
        ASM: Assembler<GeneralReg, FloatReg>,
        CC: CallConv<GeneralReg, FloatReg, ASM>;

    fn sub_reg64_reg64_imm32(buf: &mut Vec<'_, u8>, dst: GeneralReg, src1: GeneralReg, imm32: i32);
    fn sub_reg64_reg64_reg64(
        buf: &mut Vec<'_, u8>,
        dst: GeneralReg,
        src1: GeneralReg,
        src2: GeneralReg,
    );

    fn eq_reg_reg_reg(
        buf: &mut Vec<'_, u8>,
        register_width: RegisterWidth,
        dst: GeneralReg,
        src1: GeneralReg,
        src2: GeneralReg,
    );

    fn eq_reg64_reg64_reg64(
        buf: &mut Vec<'_, u8>,
        dst: GeneralReg,
        src1: GeneralReg,
        src2: GeneralReg,
    ) {
        Self::eq_reg_reg_reg(buf, RegisterWidth::W64, dst, src1, src2)
    }

    fn neq_reg_reg_reg(
        buf: &mut Vec<'_, u8>,
        register_width: RegisterWidth,
        dst: GeneralReg,
        src1: GeneralReg,
        src2: GeneralReg,
    );

    fn signed_compare_reg64(
        buf: &mut Vec<'_, u8>,
        register_width: RegisterWidth,
        operation: CompareOperation,
        dst: GeneralReg,
        src1: GeneralReg,
        src2: GeneralReg,
    );

    fn unsigned_compare_reg64(
        buf: &mut Vec<'_, u8>,
        register_width: RegisterWidth,
        operation: CompareOperation,
        dst: GeneralReg,
        src1: GeneralReg,
        src2: GeneralReg,
    );

    fn eq_freg_freg_reg64(
        buf: &mut Vec<'_, u8>,
        dst: GeneralReg,
        src1: FloatReg,
        src2: FloatReg,
        width: FloatWidth,
    );

    fn neq_freg_freg_reg64(
        buf: &mut Vec<'_, u8>,
        dst: GeneralReg,
        src1: FloatReg,
        src2: FloatReg,
        width: FloatWidth,
    );

    fn cmp_freg_freg_reg64(
        buf: &mut Vec<'_, u8>,
        dst: GeneralReg,
        src1: FloatReg,
        src2: FloatReg,
        width: FloatWidth,
        operation: CompareOperation,
    );

    fn is_nan_freg_reg64(buf: &mut Vec<'_, u8>, dst: GeneralReg, src: FloatReg, width: FloatWidth);

    fn to_float_freg32_reg64(buf: &mut Vec<'_, u8>, dst: FloatReg, src: GeneralReg);

    fn to_float_freg64_reg64(buf: &mut Vec<'_, u8>, dst: FloatReg, src: GeneralReg);

    fn to_float_freg32_freg64(buf: &mut Vec<'_, u8>, dst: FloatReg, src: FloatReg);

    fn to_float_freg64_freg32(buf: &mut Vec<'_, u8>, dst: FloatReg, src: FloatReg);

    fn set_if_overflow(buf: &mut Vec<'_, u8>, dst: GeneralReg);

    fn ret(buf: &mut Vec<'_, u8>);
}

pub trait RegTrait:
    Copy + PartialEq + Eq + std::hash::Hash + std::fmt::Debug + std::fmt::Display + 'static
{
    fn value(&self) -> u8;
}

pub struct Backend64Bit<
    'a,
    'r,
    GeneralReg: RegTrait,
    FloatReg: RegTrait,
    ASM: Assembler<GeneralReg, FloatReg>,
    CC: CallConv<GeneralReg, FloatReg, ASM>,
> {
    // TODO: A number of the uses of MutMap could probably be some form of linear mutmap
    // They are likely to be small enough that it is faster to use a vec and linearly scan it or keep it sorted and binary search.
    phantom_asm: PhantomData<ASM>,
    phantom_cc: PhantomData<CC>,
    env: &'r Env<'a>,
    layout_interner: &'r mut STLayoutInterner<'a>,
    interns: &'r mut Interns,
    helper_proc_gen: CodeGenHelp<'a>,
    helper_proc_symbols: Vec<'a, (Symbol, ProcLayout<'a>)>,
    caller_procs: Vec<'a, CallerProc<'a>>,
    buf: Vec<'a, u8>,
    relocs: Vec<'a, Relocation>,
    proc_name: Option<String>,
    is_self_recursive: Option<SelfRecursive>,

    last_seen_map: MutMap<Symbol, *const Stmt<'a>>,
    layout_map: MutMap<Symbol, InLayout<'a>>,
    free_map: MutMap<*const Stmt<'a>, Vec<'a, Symbol>>,

    literal_map: MutMap<Symbol, (*const Literal<'a>, *const InLayout<'a>)>,
    join_map: MutMap<JoinPointId, Vec<'a, (u64, u64)>>,

    storage_manager: StorageManager<'a, 'r, GeneralReg, FloatReg, ASM, CC>,
}

/// new creates a new backend that will output to the specific Object.
pub fn new_backend_64bit<
    'a,
    'r,
    GeneralReg: RegTrait,
    FloatReg: RegTrait,
    ASM: Assembler<GeneralReg, FloatReg>,
    CC: CallConv<GeneralReg, FloatReg, ASM>,
>(
    env: &'r Env<'a>,
    target_info: TargetInfo,
    interns: &'r mut Interns,
    layout_interner: &'r mut STLayoutInterner<'a>,
) -> Backend64Bit<'a, 'r, GeneralReg, FloatReg, ASM, CC> {
    Backend64Bit {
        phantom_asm: PhantomData,
        phantom_cc: PhantomData,
        env,
        interns,
        layout_interner,
        helper_proc_gen: CodeGenHelp::new(env.arena, target_info, env.module_id),
        helper_proc_symbols: bumpalo::vec![in env.arena],
        caller_procs: bumpalo::vec![in env.arena],
        proc_name: None,
        is_self_recursive: None,
        buf: bumpalo::vec![in env.arena],
        relocs: bumpalo::vec![in env.arena],
        last_seen_map: MutMap::default(),
        layout_map: MutMap::default(),
        free_map: MutMap::default(),
        literal_map: MutMap::default(),
        join_map: MutMap::default(),
        storage_manager: storage::new_storage_manager(env, target_info),
    }
}

macro_rules! quadword_and_smaller {
    () => {
        IntWidth::I64
            | IntWidth::U64
            | IntWidth::I32
            | IntWidth::U32
            | IntWidth::I16
            | IntWidth::U16
            | IntWidth::I8
            | IntWidth::U8
    };
}

impl<
        'a,
        'r,
        GeneralReg: RegTrait,
        FloatReg: RegTrait,
        ASM: Assembler<GeneralReg, FloatReg>,
        CC: CallConv<GeneralReg, FloatReg, ASM>,
    > Backend<'a> for Backend64Bit<'a, 'r, GeneralReg, FloatReg, ASM, CC>
{
    fn env(&self) -> &Env<'a> {
        self.env
    }
    fn interns(&self) -> &Interns {
        self.interns
    }
    fn interns_mut(&mut self) -> &mut Interns {
        self.interns
    }
    fn interner(&self) -> &STLayoutInterner<'a> {
        self.layout_interner
    }
    fn module_interns_helpers_mut(
        &mut self,
    ) -> (
        ModuleId,
        &mut STLayoutInterner<'a>,
        &mut Interns,
        &mut CodeGenHelp<'a>,
        &mut Vec<'a, CallerProc<'a>>,
    ) {
        (
            self.env.module_id,
            self.layout_interner,
            self.interns,
            &mut self.helper_proc_gen,
            &mut self.caller_procs,
        )
    }
    fn helper_proc_gen_mut(&mut self) -> &mut CodeGenHelp<'a> {
        &mut self.helper_proc_gen
    }
    fn helper_proc_symbols_mut(&mut self) -> &mut Vec<'a, (Symbol, ProcLayout<'a>)> {
        &mut self.helper_proc_symbols
    }
    fn helper_proc_symbols(&self) -> &Vec<'a, (Symbol, ProcLayout<'a>)> {
        &self.helper_proc_symbols
    }
    fn caller_procs(&self) -> &Vec<'a, CallerProc<'a>> {
        &self.caller_procs
    }

    fn reset(&mut self, name: String, is_self_recursive: SelfRecursive) {
        self.proc_name = Some(name);
        self.is_self_recursive = Some(is_self_recursive);
        self.last_seen_map.clear();
        self.layout_map.clear();
        self.join_map.clear();
        self.free_map.clear();
        self.buf.clear();
        self.storage_manager.reset();
    }

    fn literal_map(&mut self) -> &mut MutMap<Symbol, (*const Literal<'a>, *const InLayout<'a>)> {
        &mut self.literal_map
    }

    fn last_seen_map(&mut self) -> &mut MutMap<Symbol, *const Stmt<'a>> {
        &mut self.last_seen_map
    }

    fn layout_map(&mut self) -> &mut MutMap<Symbol, InLayout<'a>> {
        &mut self.layout_map
    }

    fn set_free_map(&mut self, map: MutMap<*const Stmt<'a>, Vec<'a, Symbol>>) {
        self.free_map = map;
    }

    fn free_map(&mut self) -> &mut MutMap<*const Stmt<'a>, Vec<'a, Symbol>> {
        &mut self.free_map
    }

    fn finalize(&mut self) -> (Vec<u8>, Vec<Relocation>) {
        let mut out = bumpalo::vec![in self.env.arena];

        // Setup stack.
        let used_general_regs = self.storage_manager.general_used_callee_saved_regs();
        let used_float_regs = self.storage_manager.float_used_callee_saved_regs();
        let aligned_stack_size = CC::setup_stack(
            &mut out,
            &used_general_regs,
            &used_float_regs,
            self.storage_manager.stack_size() as i32,
            self.storage_manager.fn_call_stack_size() as i32,
        );
        let setup_offset = out.len();

        // Deal with jumps to the return address.
        let old_relocs = std::mem::replace(&mut self.relocs, bumpalo::vec![in self.env.arena]);

        // Check if their is an unnessary jump to return right at the end of the function.
        let mut end_jmp_size = 0;
        for reloc in old_relocs
            .iter()
            .filter(|reloc| matches!(reloc, Relocation::JmpToReturn { .. }))
        {
            if let Relocation::JmpToReturn {
                inst_loc,
                inst_size,
                ..
            } = reloc
            {
                if *inst_loc as usize + *inst_size as usize == self.buf.len() {
                    end_jmp_size = *inst_size as usize;
                    break;
                }
            }
        }

        // Update jumps to returns.
        let ret_offset = self.buf.len() - end_jmp_size;
        let mut tmp = bumpalo::vec![in self.env.arena];
        for reloc in old_relocs
            .iter()
            .filter(|reloc| matches!(reloc, Relocation::JmpToReturn { .. }))
        {
            if let Relocation::JmpToReturn {
                inst_loc,
                inst_size,
                offset,
            } = reloc
            {
                if *inst_loc as usize + *inst_size as usize != self.buf.len() {
                    self.update_jmp_imm32_offset(&mut tmp, *inst_loc, *offset, ret_offset as u64);
                }
            }
        }

        // Add function body.
        out.extend(&self.buf[..self.buf.len() - end_jmp_size]);

        // Cleanup stack.
        CC::cleanup_stack(
            &mut out,
            &used_general_regs,
            &used_float_regs,
            aligned_stack_size,
            self.storage_manager.fn_call_stack_size() as i32,
        );
        ASM::ret(&mut out);

        // Update other relocs to include stack setup offset.
        let mut out_relocs = bumpalo::vec![in self.env.arena];
        out_relocs.extend(
            old_relocs
                .into_iter()
                .filter(|reloc| !matches!(reloc, Relocation::JmpToReturn { .. }))
                .map(|reloc| match reloc {
                    Relocation::LocalData { offset, data } => Relocation::LocalData {
                        offset: offset + setup_offset as u64,
                        data,
                    },
                    Relocation::LinkedData { offset, name } => Relocation::LinkedData {
                        offset: offset + setup_offset as u64,
                        name,
                    },
                    Relocation::LinkedFunction { offset, name } => Relocation::LinkedFunction {
                        offset: offset + setup_offset as u64,
                        name,
                    },
                    Relocation::JmpToReturn { .. } => unreachable!(),
                }),
        );
        (out, out_relocs)
    }

    fn load_args(&mut self, args: &'a [(InLayout<'a>, Symbol)], ret_layout: &InLayout<'a>) {
        CC::load_args(
            &mut self.buf,
            &mut self.storage_manager,
            self.layout_interner,
            args,
            ret_layout,
        );
    }

    /// Used for generating wrappers for malloc/realloc/free
    fn build_wrapped_jmp(&mut self) -> (&'a [u8], u64) {
        let mut out = bumpalo::vec![in self.env.arena];
        let offset = ASM::tail_call(&mut out);

        (out.into_bump_slice(), offset)
    }

    fn build_fn_pointer(&mut self, dst: &Symbol, fn_name: String) {
        let reg = self.storage_manager.claim_general_reg(&mut self.buf, dst);

        ASM::function_pointer(&mut self.buf, &mut self.relocs, fn_name, reg)
    }

    fn build_fn_call(
        &mut self,
        dst: &Symbol,
        fn_name: String,
        args: &[Symbol],
        arg_layouts: &[InLayout<'a>],
        ret_layout: &InLayout<'a>,
    ) {
        // Save used caller saved regs.
        self.storage_manager
            .push_used_caller_saved_regs_to_stack(&mut self.buf);

        // Put values in param regs or on top of the stack.
        CC::store_args(
            &mut self.buf,
            &mut self.storage_manager,
            self.layout_interner,
            dst,
            args,
            arg_layouts,
            ret_layout,
        );

        // Call function and generate reloc.
        ASM::call(&mut self.buf, &mut self.relocs, fn_name);

        self.move_return_value(dst, ret_layout)
    }

    fn move_return_value(&mut self, dst: &Symbol, ret_layout: &InLayout<'a>) {
        // move return value to dst.
        let ret_repr = self.interner().get_repr(*ret_layout);
        match ret_repr {
            single_register_integers!() => {
                let width = RegisterWidth::try_from_layout(ret_repr).unwrap();

                let dst_reg = self.storage_manager.claim_general_reg(&mut self.buf, dst);
                ASM::movzx_reg_reg(&mut self.buf, width, dst_reg, CC::GENERAL_RETURN_REGS[0]);
            }
            single_register_floats!() => {
                let dst_reg = self.storage_manager.claim_float_reg(&mut self.buf, dst);
                ASM::mov_freg64_freg64(&mut self.buf, dst_reg, CC::FLOAT_RETURN_REGS[0]);
            }
            LayoutRepr::I128 | LayoutRepr::U128 => {
                let offset = self.storage_manager.claim_stack_area(dst, 16);

                ASM::mov_base32_reg64(&mut self.buf, offset, CC::GENERAL_RETURN_REGS[0]);
                ASM::mov_base32_reg64(&mut self.buf, offset + 8, CC::GENERAL_RETURN_REGS[1]);
            }
            pointer_layouts!() => {
                let dst_reg = self.storage_manager.claim_general_reg(&mut self.buf, dst);
                ASM::mov_reg64_reg64(&mut self.buf, dst_reg, CC::GENERAL_RETURN_REGS[0]);
            }
            LayoutRepr::LambdaSet(lambda_set) => {
                self.move_return_value(dst, &lambda_set.runtime_representation())
            }
            LayoutRepr::Union(UnionLayout::NonRecursive(_)) => {
                CC::load_returned_complex_symbol(
                    &mut self.buf,
                    &mut self.storage_manager,
                    self.layout_interner,
                    dst,
                    ret_layout,
                );
            }
            _ => {
                CC::load_returned_complex_symbol(
                    &mut self.buf,
                    &mut self.storage_manager,
                    self.layout_interner,
                    dst,
                    ret_layout,
                );
            }
        }
    }

    fn build_switch(
        &mut self,
        layout_ids: &mut LayoutIds<'a>,
        cond_symbol: &Symbol,
        _cond_layout: &InLayout<'a>, // cond_layout must be a integer due to potential jump table optimizations.
        branches: &'a [(u64, BranchInfo<'a>, Stmt<'a>)],
        default_branch: &(BranchInfo<'a>, &'a Stmt<'a>),
        ret_layout: &InLayout<'a>,
    ) {
        // Switches are a little complex due to keeping track of jumps.
        // In general I am trying to not have to loop over things multiple times or waste memory.
        // The basic plan is to make jumps to nowhere and then correct them once we know the correct address.
        let cond_reg = self
            .storage_manager
            .load_to_general_reg(&mut self.buf, cond_symbol);

        // this state is updated destructively in the branches. We don't want the branches to
        // influence each other, so we must clone here.
        let mut base_storage = self.storage_manager.clone();
        let base_literal_map = self.literal_map.clone();

        let mut max_branch_stack_size = 0;
        let mut ret_jumps = bumpalo::vec![in self.env.arena];
        let mut tmp = bumpalo::vec![in self.env.arena];
        for (val, _branch_info, stmt) in branches.iter() {
            // TODO: look into branch info and if it matters here.
            tmp.clear();
            // Create jump to next branch if cond_sym not equal to value.
            // Since we don't know the offset yet, set it to 0 and overwrite later.
            let jne_location = self.buf.len();
            let start_offset = ASM::jne_reg64_imm64_imm32(
                &mut self.buf,
                &mut self.storage_manager,
                cond_reg,
                *val,
                0,
            );

            // Build all statements in this branch. Using storage as from before any branch.
            self.storage_manager = base_storage.clone();
            self.literal_map = base_literal_map.clone();
            self.build_stmt(layout_ids, stmt, ret_layout);

            // Build unconditional jump to the end of this switch.
            // Since we don't know the offset yet, set it to 0 and overwrite later.
            let jmp_location = self.buf.len();
            let jmp_offset = ASM::jmp_imm32(&mut self.buf, 0x1234_5678);
            ret_jumps.push((jmp_location, jmp_offset));

            // Overwrite the original jne with the correct offset.
            let end_offset = self.buf.len();
            let jne_offset = end_offset - start_offset;
            ASM::jne_reg64_imm64_imm32(
                &mut tmp,
                &mut self.storage_manager,
                cond_reg,
                *val,
                jne_offset as i32,
            );
            for (i, byte) in tmp.iter().enumerate() {
                self.buf[jne_location + i] = *byte;
            }

            // Update important storage information to avoid overwrites.
            max_branch_stack_size =
                std::cmp::max(max_branch_stack_size, self.storage_manager.stack_size());
            base_storage.update_fn_call_stack_size(self.storage_manager.fn_call_stack_size());
        }
        self.storage_manager = base_storage;
        self.literal_map = base_literal_map;
        self.storage_manager
            .update_stack_size(max_branch_stack_size);
        let (_branch_info, stmt) = default_branch;
        self.build_stmt(layout_ids, stmt, ret_layout);

        // Update all return jumps to jump past the default case.
        let ret_offset = self.buf.len();
        for (jmp_location, start_offset) in ret_jumps.into_iter() {
            self.update_jmp_imm32_offset(
                &mut tmp,
                jmp_location as u64,
                start_offset as u64,
                ret_offset as u64,
            );
        }
    }

    fn build_join(
        &mut self,
        layout_ids: &mut LayoutIds<'a>,
        id: &JoinPointId,
        parameters: &'a [Param<'a>],
        body: &'a Stmt<'a>,
        remainder: &'a Stmt<'a>,
        ret_layout: &InLayout<'a>,
    ) {
        // Free everything to the stack to make sure they don't get messed up when looping back to this point.
        // TODO: look into a nicer solution.
        self.storage_manager.free_all_to_stack(&mut self.buf);

        // Ensure all the joinpoint parameters have storage locations.
        // On jumps to the joinpoint, we will overwrite those locations as a way to "pass parameters" to the joinpoint.
        self.storage_manager
            .setup_joinpoint(self.layout_interner, &mut self.buf, id, parameters);

        self.join_map.insert(*id, bumpalo::vec![in self.env.arena]);

        // Build remainder of function first. It is what gets run and jumps to join.
        self.build_stmt(layout_ids, remainder, ret_layout);

        let join_location = self.buf.len() as u64;

        // Build all statements in body.
        self.build_stmt(layout_ids, body, ret_layout);

        // Overwrite the all jumps to the joinpoint with the correct offset.
        let mut tmp = bumpalo::vec![in self.env.arena];
        for (jmp_location, start_offset) in self
            .join_map
            .remove(id)
            .unwrap_or_else(|| internal_error!("join point not defined"))
        {
            tmp.clear();
            self.update_jmp_imm32_offset(&mut tmp, jmp_location, start_offset, join_location);
        }
    }

    fn build_jump(
        &mut self,
        id: &JoinPointId,
        args: &[Symbol],
        arg_layouts: &[InLayout<'a>],
        _ret_layout: &InLayout<'a>,
    ) {
        self.storage_manager
            .setup_jump(self.layout_interner, &mut self.buf, id, args, arg_layouts);

        let jmp_location = self.buf.len();
        let start_offset = ASM::jmp_imm32(&mut self.buf, 0x1234_5678);

        if let Some(vec) = self.join_map.get_mut(id) {
            vec.push((jmp_location as u64, start_offset as u64))
        } else {
            internal_error!("Jump: unknown point specified to jump to: {:?}", id);
        }
    }

    fn build_num_abs(&mut self, dst: &Symbol, src: &Symbol, layout: &InLayout<'a>) {
        match self.interner().get_repr(*layout) {
            LayoutRepr::Builtin(Builtin::Int(IntWidth::I64 | IntWidth::U64)) => {
                let dst_reg = self.storage_manager.claim_general_reg(&mut self.buf, dst);
                let src_reg = self.storage_manager.load_to_general_reg(&mut self.buf, src);
                ASM::abs_reg64_reg64(&mut self.buf, dst_reg, src_reg);
            }
            LayoutRepr::Builtin(Builtin::Float(FloatWidth::F64)) => {
                let dst_reg = self.storage_manager.claim_float_reg(&mut self.buf, dst);
                let src_reg = self.storage_manager.load_to_float_reg(&mut self.buf, src);
                ASM::abs_freg64_freg64(&mut self.buf, &mut self.relocs, dst_reg, src_reg);
            }
            x => todo!("NumAbs: layout, {:?}", x),
        }
    }

    fn build_num_add(&mut self, dst: &Symbol, src1: &Symbol, src2: &Symbol, layout: &InLayout<'a>) {
        match self.layout_interner.get_repr(*layout) {
            LayoutRepr::Builtin(Builtin::Int(quadword_and_smaller!())) => {
                let dst_reg = self.storage_manager.claim_general_reg(&mut self.buf, dst);
                let src1_reg = self
                    .storage_manager
                    .load_to_general_reg(&mut self.buf, src1);
                let src2_reg = self
                    .storage_manager
                    .load_to_general_reg(&mut self.buf, src2);
                ASM::add_reg64_reg64_reg64(&mut self.buf, dst_reg, src1_reg, src2_reg);
            }
            LayoutRepr::Builtin(Builtin::Float(FloatWidth::F64)) => {
                let dst_reg = self.storage_manager.claim_float_reg(&mut self.buf, dst);
                let src1_reg = self.storage_manager.load_to_float_reg(&mut self.buf, src1);
                let src2_reg = self.storage_manager.load_to_float_reg(&mut self.buf, src2);
                ASM::add_freg64_freg64_freg64(&mut self.buf, dst_reg, src1_reg, src2_reg);
            }
            LayoutRepr::Builtin(Builtin::Float(FloatWidth::F32)) => {
                let dst_reg = self.storage_manager.claim_float_reg(&mut self.buf, dst);
                let src1_reg = self.storage_manager.load_to_float_reg(&mut self.buf, src1);
                let src2_reg = self.storage_manager.load_to_float_reg(&mut self.buf, src2);
                ASM::add_freg32_freg32_freg32(&mut self.buf, dst_reg, src1_reg, src2_reg);
            }
            x => todo!("NumAdd: layout, {:?}", x),
        }
    }

    fn build_num_add_saturated(
        &mut self,
        dst: Symbol,
        src1: Symbol,
        src2: Symbol,
        layout: InLayout<'a>,
    ) {
        match self.layout_interner.get_repr(layout) {
            LayoutRepr::Builtin(Builtin::Int(width @ quadword_and_smaller!())) => {
                let intrinsic = bitcode::NUM_ADD_SATURATED_INT[width].to_string();
                self.build_fn_call(&dst, intrinsic, &[src1, src2], &[layout, layout], &layout);
            }
            LayoutRepr::Builtin(Builtin::Float(FloatWidth::F64)) => {
                let dst_reg = self.storage_manager.claim_float_reg(&mut self.buf, &dst);
                let src1_reg = self.storage_manager.load_to_float_reg(&mut self.buf, &src1);
                let src2_reg = self.storage_manager.load_to_float_reg(&mut self.buf, &src2);
                ASM::add_freg64_freg64_freg64(&mut self.buf, dst_reg, src1_reg, src2_reg);
            }
            LayoutRepr::Builtin(Builtin::Float(FloatWidth::F32)) => {
                let dst_reg = self.storage_manager.claim_float_reg(&mut self.buf, &dst);
                let src1_reg = self.storage_manager.load_to_float_reg(&mut self.buf, &src1);
                let src2_reg = self.storage_manager.load_to_float_reg(&mut self.buf, &src2);
                ASM::add_freg32_freg32_freg32(&mut self.buf, dst_reg, src1_reg, src2_reg);
            }
            LayoutRepr::Builtin(Builtin::Decimal) => {
                let intrinsic = bitcode::DEC_ADD_SATURATED.to_string();
                self.build_fn_call(&dst, intrinsic, &[src1, src2], &[layout, layout], &layout);
            }
            x => todo!("NumAddSaturated: layout, {:?}", x),
        }
    }

    fn build_num_add_checked(
        &mut self,
        dst: &Symbol,
        src1: &Symbol,
        src2: &Symbol,
        num_layout: &InLayout<'a>,
        return_layout: &InLayout<'a>,
    ) {
        use Builtin::Int;

        let buf = &mut self.buf;

        let struct_size = self.layout_interner.stack_size(*return_layout);

        let base_offset = self.storage_manager.claim_stack_area(dst, struct_size);

        match self.layout_interner.get_repr(*num_layout) {
            LayoutRepr::Builtin(Int(
                IntWidth::I64 | IntWidth::I32 | IntWidth::I16 | IntWidth::I8,
            )) => {
                let dst_reg = self
                    .storage_manager
                    .claim_general_reg(buf, &Symbol::DEV_TMP);

                let overflow_reg = self
                    .storage_manager
                    .claim_general_reg(buf, &Symbol::DEV_TMP2);

                let src1_reg = self.storage_manager.load_to_general_reg(buf, src1);
                let src2_reg = self.storage_manager.load_to_general_reg(buf, src2);

                ASM::add_reg64_reg64_reg64(buf, dst_reg, src1_reg, src2_reg);
                ASM::set_if_overflow(buf, overflow_reg);

                ASM::mov_base32_reg64(buf, base_offset, dst_reg);
                ASM::mov_base32_reg64(buf, base_offset + 8, overflow_reg);

                self.free_symbol(&Symbol::DEV_TMP);
                self.free_symbol(&Symbol::DEV_TMP2);
            }
            LayoutRepr::Builtin(Int(
                IntWidth::U64 | IntWidth::U32 | IntWidth::U16 | IntWidth::U8,
            )) => {
                todo!("addChecked for unsigned integers")
            }
            LayoutRepr::Builtin(Builtin::Float(FloatWidth::F64)) => {
                todo!("addChecked for f64")
            }
            LayoutRepr::Builtin(Builtin::Float(FloatWidth::F32)) => {
                todo!("addChecked for f32")
            }
            x => todo!("NumAdd: layout, {:?}", x),
        }
    }

    fn build_num_sub_checked(
        &mut self,
        dst: &Symbol,
        src1: &Symbol,
        src2: &Symbol,
        num_layout: &InLayout<'a>,
        return_layout: &InLayout<'a>,
    ) {
        let function_name = match self.interner().get_repr(*num_layout) {
            LayoutRepr::Builtin(Builtin::Int(width)) => &bitcode::NUM_SUB_CHECKED_INT[width],
            LayoutRepr::Builtin(Builtin::Float(width)) => &bitcode::NUM_SUB_CHECKED_FLOAT[width],
            LayoutRepr::Builtin(Builtin::Decimal) => bitcode::DEC_SUB_WITH_OVERFLOW,
            x => internal_error!("NumSubChecked is not defined for {:?}", x),
        };

        self.build_fn_call(
            dst,
            function_name.to_string(),
            &[*src1, *src2],
            &[*num_layout, *num_layout],
            return_layout,
        )
    }

    fn build_num_mul(&mut self, dst: &Symbol, src1: &Symbol, src2: &Symbol, layout: &InLayout<'a>) {
        // for the time being, `num_mul` is implemented as wrapping multiplication. In roc, the normal
        // `mul` should panic on overflow, but we just don't do that yet
        self.build_num_mul_wrap(dst, src1, src2, layout)
    }

    fn build_num_mul_wrap(
        &mut self,
        dst: &Symbol,
        src1: &Symbol,
        src2: &Symbol,
        layout: &InLayout<'a>,
    ) {
        use Builtin::Int;

        match self.layout_interner.get_repr(*layout) {
            LayoutRepr::Builtin(Int(
                IntWidth::I64 | IntWidth::I32 | IntWidth::I16 | IntWidth::I8,
            )) => {
                let dst_reg = self.storage_manager.claim_general_reg(&mut self.buf, dst);
                let src1_reg = self
                    .storage_manager
                    .load_to_general_reg(&mut self.buf, src1);
                let src2_reg = self
                    .storage_manager
                    .load_to_general_reg(&mut self.buf, src2);
                ASM::imul_reg64_reg64_reg64(&mut self.buf, dst_reg, src1_reg, src2_reg);
            }
            LayoutRepr::Builtin(Int(
                IntWidth::U64 | IntWidth::U32 | IntWidth::U16 | IntWidth::U8,
            )) => {
                let dst_reg = self.storage_manager.claim_general_reg(&mut self.buf, dst);
                let src1_reg = self
                    .storage_manager
                    .load_to_general_reg(&mut self.buf, src1);
                let src2_reg = self
                    .storage_manager
                    .load_to_general_reg(&mut self.buf, src2);

                ASM::umul_reg64_reg64_reg64(
                    &mut self.buf,
                    &mut self.storage_manager,
                    dst_reg,
                    src1_reg,
                    src2_reg,
                );
            }
            LayoutRepr::Builtin(Builtin::Int(IntWidth::I128 | IntWidth::U128)) => {
                let int_width = match *layout {
                    Layout::I128 => IntWidth::I128,
                    Layout::U128 => IntWidth::U128,
                    _ => unreachable!(),
                };

                self.build_fn_call(
                    dst,
                    bitcode::NUM_MUL_WRAP_INT[int_width].to_string(),
                    &[*src1, *src2],
                    &[*layout, *layout],
                    layout,
                );
            }
            LayoutRepr::Builtin(Builtin::Float(FloatWidth::F64)) => {
                let dst_reg = self.storage_manager.claim_float_reg(&mut self.buf, dst);
                let src1_reg = self.storage_manager.load_to_float_reg(&mut self.buf, src1);
                let src2_reg = self.storage_manager.load_to_float_reg(&mut self.buf, src2);
                ASM::mul_freg64_freg64_freg64(&mut self.buf, dst_reg, src1_reg, src2_reg);
            }
            LayoutRepr::Builtin(Builtin::Float(FloatWidth::F32)) => {
                let dst_reg = self.storage_manager.claim_float_reg(&mut self.buf, dst);
                let src1_reg = self.storage_manager.load_to_float_reg(&mut self.buf, src1);
                let src2_reg = self.storage_manager.load_to_float_reg(&mut self.buf, src2);
                ASM::mul_freg32_freg32_freg32(&mut self.buf, dst_reg, src1_reg, src2_reg);
            }
            x => todo!("NumMulWrap: layout, {:?}", x),
        }
    }

    fn build_num_div(&mut self, dst: &Symbol, src1: &Symbol, src2: &Symbol, layout: &InLayout<'a>) {
        match self.layout_interner.get_repr(*layout) {
            LayoutRepr::Builtin(Builtin::Int(
                IntWidth::I64 | IntWidth::I32 | IntWidth::I16 | IntWidth::I8,
            )) => {
                let dst_reg = self.storage_manager.claim_general_reg(&mut self.buf, dst);
                let src1_reg = self
                    .storage_manager
                    .load_to_general_reg(&mut self.buf, src1);
                let src2_reg = self
                    .storage_manager
                    .load_to_general_reg(&mut self.buf, src2);

                ASM::idiv_reg64_reg64_reg64(
                    &mut self.buf,
                    &mut self.storage_manager,
                    dst_reg,
                    src1_reg,
                    src2_reg,
                );
            }
            LayoutRepr::Builtin(Builtin::Int(
                IntWidth::U64 | IntWidth::U32 | IntWidth::U16 | IntWidth::U8,
            )) => {
                let dst_reg = self.storage_manager.claim_general_reg(&mut self.buf, dst);
                let src1_reg = self
                    .storage_manager
                    .load_to_general_reg(&mut self.buf, src1);
                let src2_reg = self
                    .storage_manager
                    .load_to_general_reg(&mut self.buf, src2);

                ASM::udiv_reg64_reg64_reg64(
                    &mut self.buf,
                    &mut self.storage_manager,
                    dst_reg,
                    src1_reg,
                    src2_reg,
                );
            }
            LayoutRepr::Builtin(Builtin::Float(FloatWidth::F64)) => {
                let dst_reg = self.storage_manager.claim_float_reg(&mut self.buf, dst);
                let src1_reg = self.storage_manager.load_to_float_reg(&mut self.buf, src1);
                let src2_reg = self.storage_manager.load_to_float_reg(&mut self.buf, src2);
                ASM::div_freg64_freg64_freg64(&mut self.buf, dst_reg, src1_reg, src2_reg);
            }
            LayoutRepr::Builtin(Builtin::Float(FloatWidth::F32)) => {
                let dst_reg = self.storage_manager.claim_float_reg(&mut self.buf, dst);
                let src1_reg = self.storage_manager.load_to_float_reg(&mut self.buf, src1);
                let src2_reg = self.storage_manager.load_to_float_reg(&mut self.buf, src2);
                ASM::div_freg32_freg32_freg32(&mut self.buf, dst_reg, src1_reg, src2_reg);
            }
            x => todo!("NumDiv: layout, {:?}", x),
        }
    }

    fn build_num_rem(&mut self, dst: &Symbol, src1: &Symbol, src2: &Symbol, layout: &InLayout<'a>) {
        match self.layout_interner.get_repr(*layout) {
            LayoutRepr::Builtin(Builtin::Int(
                IntWidth::I64 | IntWidth::I32 | IntWidth::I16 | IntWidth::I8,
            )) => {
                let dst_reg = self.storage_manager.claim_general_reg(&mut self.buf, dst);
                let src1_reg = self
                    .storage_manager
                    .load_to_general_reg(&mut self.buf, src1);
                let src2_reg = self
                    .storage_manager
                    .load_to_general_reg(&mut self.buf, src2);

                ASM::irem_reg64_reg64_reg64(
                    &mut self.buf,
                    &mut self.storage_manager,
                    dst_reg,
                    src1_reg,
                    src2_reg,
                );
            }
            LayoutRepr::Builtin(Builtin::Int(
                IntWidth::U64 | IntWidth::U32 | IntWidth::U16 | IntWidth::U8,
            )) => {
                let dst_reg = self.storage_manager.claim_general_reg(&mut self.buf, dst);
                let src1_reg = self
                    .storage_manager
                    .load_to_general_reg(&mut self.buf, src1);
                let src2_reg = self
                    .storage_manager
                    .load_to_general_reg(&mut self.buf, src2);

                ASM::urem_reg64_reg64_reg64(
                    &mut self.buf,
                    &mut self.storage_manager,
                    dst_reg,
                    src1_reg,
                    src2_reg,
                );
            }
            x => todo!("NumDiv: layout, {:?}", x),
        }
    }

    fn build_num_neg(&mut self, dst: &Symbol, src: &Symbol, layout: &InLayout<'a>) {
        match self.layout_interner.get_repr(*layout) {
            LayoutRepr::Builtin(Builtin::Int(IntWidth::I64 | IntWidth::U64)) => {
                let dst_reg = self.storage_manager.claim_general_reg(&mut self.buf, dst);
                let src_reg = self.storage_manager.load_to_general_reg(&mut self.buf, src);
                ASM::neg_reg64_reg64(&mut self.buf, dst_reg, src_reg);
            }
            x => todo!("NumNeg: layout, {:?}", x),
        }
    }

    fn build_num_sub(&mut self, dst: &Symbol, src1: &Symbol, src2: &Symbol, layout: &InLayout<'a>) {
        // for the time being, `num_sub` is implemented as wrapping subtraction. In roc, the normal
        // `sub` should panic on overflow, but we just don't do that yet
        self.build_num_sub_wrap(dst, src1, src2, layout)
    }

    fn build_num_sub_wrap(
        &mut self,
        dst: &Symbol,
        src1: &Symbol,
        src2: &Symbol,
        layout: &InLayout<'a>,
    ) {
        match self.layout_interner.get_repr(*layout) {
            LayoutRepr::Builtin(Builtin::Int(quadword_and_smaller!())) => {
                let dst_reg = self.storage_manager.claim_general_reg(&mut self.buf, dst);
                let src1_reg = self
                    .storage_manager
                    .load_to_general_reg(&mut self.buf, src1);
                let src2_reg = self
                    .storage_manager
                    .load_to_general_reg(&mut self.buf, src2);
                ASM::sub_reg64_reg64_reg64(&mut self.buf, dst_reg, src1_reg, src2_reg);
            }
            x => todo!("NumSubWrap: layout, {:?}", x),
        }
    }

    fn build_eq(&mut self, dst: &Symbol, src1: &Symbol, src2: &Symbol, arg_layout: &InLayout<'a>) {
        let repr = self.interner().get_repr(*arg_layout);
        match repr {
            single_register_int_builtins!() | LayoutRepr::BOOL => {
                let width = match repr {
                    LayoutRepr::BOOL | LayoutRepr::I8 | LayoutRepr::U8 => RegisterWidth::W8,
                    LayoutRepr::I16 | LayoutRepr::U16 => RegisterWidth::W16,
                    LayoutRepr::U32 | LayoutRepr::I32 => RegisterWidth::W32,
                    LayoutRepr::I64 | LayoutRepr::U64 => RegisterWidth::W64,
                    _ => unreachable!(),
                };

                let dst_reg = self.storage_manager.claim_general_reg(&mut self.buf, dst);
                let src1_reg = self
                    .storage_manager
                    .load_to_general_reg(&mut self.buf, src1);
                let src2_reg = self
                    .storage_manager
                    .load_to_general_reg(&mut self.buf, src2);
                ASM::eq_reg_reg_reg(&mut self.buf, width, dst_reg, src1_reg, src2_reg);
            }
            LayoutRepr::U128 | LayoutRepr::I128 => {
                let dst_reg = self.storage_manager.claim_general_reg(&mut self.buf, dst);

                // put the arguments on the stack
                let (src1_offset, _) = self.storage_manager.stack_offset_and_size(src1);
                let (src2_offset, _) = self.storage_manager.stack_offset_and_size(src2);

                let tmp1_symbol = self.debug_symbol("eq_tmp1");
                let tmp2_symbol = self.debug_symbol("eq_tmp2");

                let buf = &mut self.buf;

                let tmp1 = self.storage_manager.claim_general_reg(buf, &tmp1_symbol);
                let tmp2 = self.storage_manager.claim_general_reg(buf, &tmp2_symbol);

                // move the upper 8 bytes of both arguments into a register
                ASM::mov_reg64_base32(buf, tmp1, src1_offset);
                ASM::mov_reg64_base32(buf, tmp2, src2_offset);

                // store the result in our destination
                ASM::eq_reg64_reg64_reg64(buf, dst_reg, tmp1, tmp2);

                // move the lower 8 bytes of both arguments into a register
                ASM::mov_reg64_base32(buf, tmp1, src1_offset + 8);
                ASM::mov_reg64_base32(buf, tmp2, src2_offset + 8);

                // store the result in tmp1
                ASM::eq_reg64_reg64_reg64(buf, tmp1, tmp1, tmp2);

                // now and dst and tmp1, storing the result in dst
                ASM::and_reg64_reg64_reg64(buf, dst_reg, dst_reg, tmp1);

                self.storage_manager.free_symbol(&tmp1_symbol);
                self.storage_manager.free_symbol(&tmp2_symbol);
            }
            LayoutRepr::F32 | LayoutRepr::F64 => {
                let float_width = if repr == LayoutRepr::F32 {
                    FloatWidth::F32
                } else {
                    FloatWidth::F64
                };

                let buf = &mut self.buf;

                let dst_reg = self.storage_manager.claim_general_reg(buf, dst);

                let src_reg1 = self.storage_manager.load_to_float_reg(buf, src1);
                let src_reg2 = self.storage_manager.load_to_float_reg(buf, src2);

                ASM::eq_freg_freg_reg64(&mut self.buf, dst_reg, src_reg1, src_reg2, float_width)
            }
            LayoutRepr::DEC => todo!("NumEq: layout, {:?}", self.layout_interner.dbg(Layout::DEC)),
            LayoutRepr::STR => {
                // use a zig call
                self.build_fn_call(
                    dst,
                    bitcode::STR_EQUAL.to_string(),
                    &[*src1, *src2],
                    &[Layout::STR, Layout::STR],
                    &Layout::BOOL,
                );

                // mask the result; we pass booleans around as 64-bit values, but branch on 0x0 and 0x1.
                // Zig gives back values where not all of the upper bits are zero, so we must clear them ourselves
                let tmp = &Symbol::DEV_TMP;
                let tmp_reg = self.storage_manager.claim_general_reg(&mut self.buf, tmp);
                ASM::mov_reg64_imm64(&mut self.buf, tmp_reg, true as i64);

                let width = RegisterWidth::W8; // we're comparing booleans
                let dst_reg = self.storage_manager.load_to_general_reg(&mut self.buf, dst);
                ASM::eq_reg_reg_reg(&mut self.buf, width, dst_reg, dst_reg, tmp_reg);

                self.free_symbol(tmp);
            }
            _ => {
                let ident_ids = self
                    .interns
                    .all_ident_ids
                    .get_mut(&self.env.module_id)
                    .unwrap();

                // generate a proc

                let (eq_symbol, eq_linker_data) = self.helper_proc_gen.gen_refcount_proc(
                    ident_ids,
                    self.layout_interner,
                    *arg_layout,
                    HelperOp::Eq,
                );

                let fn_name = self.lambda_name_to_string(
                    LambdaName::no_niche(eq_symbol),
                    [*arg_layout, *arg_layout].into_iter(),
                    None,
                    Layout::U8,
                );

                self.helper_proc_symbols.extend(eq_linker_data);

                self.build_fn_call(
                    dst,
                    fn_name,
                    &[*src1, *src2],
                    &[*arg_layout, *arg_layout],
                    &Layout::U8,
                )
            }
        }
    }

    fn build_neq(&mut self, dst: &Symbol, src1: &Symbol, src2: &Symbol, arg_layout: &InLayout<'a>) {
        match self.interner().get_repr(*arg_layout) {
            single_register_int_builtins!() | LayoutRepr::BOOL => {
                let width = match *arg_layout {
                    Layout::BOOL | Layout::I8 | Layout::U8 => RegisterWidth::W8,
                    Layout::I16 | Layout::U16 => RegisterWidth::W16,
                    Layout::U32 | Layout::I32 => RegisterWidth::W32,
                    Layout::I64 | Layout::U64 => RegisterWidth::W64,
                    _ => unreachable!(),
                };

                let dst_reg = self.storage_manager.claim_general_reg(&mut self.buf, dst);
                let src1_reg = self
                    .storage_manager
                    .load_to_general_reg(&mut self.buf, src1);
                let src2_reg = self
                    .storage_manager
                    .load_to_general_reg(&mut self.buf, src2);
                ASM::neq_reg_reg_reg(&mut self.buf, width, dst_reg, src1_reg, src2_reg);
            }
            LayoutRepr::STR => {
                self.build_fn_call(
                    dst,
                    bitcode::STR_EQUAL.to_string(),
                    &[*src1, *src2],
                    &[Layout::STR, Layout::STR],
                    &Layout::BOOL,
                );

                // negate the result
                let tmp = &Symbol::DEV_TMP;
                let tmp_reg = self.storage_manager.claim_general_reg(&mut self.buf, tmp);
                ASM::mov_reg64_imm64(&mut self.buf, tmp_reg, true as i64);

                let width = RegisterWidth::W8; // we're comparing booleans
                let dst_reg = self.storage_manager.load_to_general_reg(&mut self.buf, dst);
                ASM::neq_reg_reg_reg(&mut self.buf, width, dst_reg, dst_reg, tmp_reg);

                self.free_symbol(tmp)
            }
            _ => {
                // defer to equality

                self.build_eq(dst, src1, src2, arg_layout);

                let dst_reg = self.storage_manager.load_to_general_reg(&mut self.buf, dst);

                self.storage_manager
                    .with_tmp_general_reg(&mut self.buf, |_, buf, tmp| {
                        ASM::mov_reg64_imm64(buf, tmp, -1);
                        ASM::xor_reg64_reg64_reg64(buf, dst_reg, tmp, dst_reg);

                        ASM::mov_reg64_imm64(buf, tmp, 1);
                        ASM::and_reg64_reg64_reg64(buf, dst_reg, tmp, dst_reg);
                    })
            }
        }
    }

    fn build_not(&mut self, dst: &Symbol, src: &Symbol, arg_layout: &InLayout<'a>) {
        match self.interner().get_repr(*arg_layout) {
            LayoutRepr::BOOL => {
                let dst_reg = self.storage_manager.claim_general_reg(&mut self.buf, dst);
                let src_reg = self.storage_manager.load_to_general_reg(&mut self.buf, src);

                // Not would usually be implemented as `xor src, -1` followed by `and src, 1`
                // but since our booleans are represented as `0x101010101010101` currently, we can simply XOR with that
                let bool_val = [true as u8; 8];
                ASM::mov_reg64_imm64(&mut self.buf, dst_reg, i64::from_ne_bytes(bool_val));
                ASM::xor_reg64_reg64_reg64(&mut self.buf, src_reg, src_reg, dst_reg);
                ASM::mov_reg64_reg64(&mut self.buf, dst_reg, src_reg);
            }
            x => todo!("Not: layout, {:?}", x),
        }
    }

    fn build_num_to_frac(
        &mut self,
        dst: &Symbol,
        src: &Symbol,
        arg_layout: &InLayout<'a>,
        ret_layout: &InLayout<'a>,
    ) {
        let dst_reg = self.storage_manager.claim_float_reg(&mut self.buf, dst);
        match (
            self.layout_interner.get_repr(*arg_layout),
            self.layout_interner.get_repr(*ret_layout),
        ) {
            (
                LayoutRepr::Builtin(Builtin::Int(IntWidth::I32 | IntWidth::I64)),
                LayoutRepr::Builtin(Builtin::Float(FloatWidth::F64)),
            ) => {
                let src_reg = self.storage_manager.load_to_general_reg(&mut self.buf, src);
                ASM::to_float_freg64_reg64(&mut self.buf, dst_reg, src_reg);
            }
            (
                LayoutRepr::Builtin(Builtin::Int(IntWidth::I32 | IntWidth::I64)),
                LayoutRepr::Builtin(Builtin::Float(FloatWidth::F32)),
            ) => {
                let src_reg = self.storage_manager.load_to_general_reg(&mut self.buf, src);
                ASM::to_float_freg32_reg64(&mut self.buf, dst_reg, src_reg);
            }
            (
                LayoutRepr::Builtin(Builtin::Float(FloatWidth::F64)),
                LayoutRepr::Builtin(Builtin::Float(FloatWidth::F32)),
            ) => {
                let src_reg = self.storage_manager.load_to_float_reg(&mut self.buf, src);
                ASM::to_float_freg32_freg64(&mut self.buf, dst_reg, src_reg);
            }
            (
                LayoutRepr::Builtin(Builtin::Float(FloatWidth::F32)),
                LayoutRepr::Builtin(Builtin::Float(FloatWidth::F64)),
            ) => {
                let src_reg = self.storage_manager.load_to_float_reg(&mut self.buf, src);
                ASM::to_float_freg64_freg32(&mut self.buf, dst_reg, src_reg);
            }
            (
                LayoutRepr::Builtin(Builtin::Float(FloatWidth::F64)),
                LayoutRepr::Builtin(Builtin::Float(FloatWidth::F64)),
            ) => {
                let src_reg = self.storage_manager.load_to_float_reg(&mut self.buf, src);
                ASM::mov_freg64_freg64(&mut self.buf, dst_reg, src_reg);
            }
            (
                LayoutRepr::Builtin(Builtin::Float(FloatWidth::F32)),
                LayoutRepr::Builtin(Builtin::Float(FloatWidth::F32)),
            ) => {
                let src_reg = self.storage_manager.load_to_float_reg(&mut self.buf, src);
                ASM::mov_freg64_freg64(&mut self.buf, dst_reg, src_reg);
            }
            (a, r) => todo!("NumToFrac: layout, arg {:?}, ret {:?}", a, r),
        }
    }

    fn build_num_is_nan(&mut self, dst: &Symbol, src: &Symbol, arg_layout: &InLayout<'a>) {
        let float_width = match *arg_layout {
            Layout::F32 => FloatWidth::F32,
            Layout::F64 => FloatWidth::F64,
            _ => unreachable!(),
        };

        let dst_reg = self.storage_manager.claim_general_reg(&mut self.buf, dst);
        let src_reg = self.storage_manager.load_to_float_reg(&mut self.buf, src);

        ASM::is_nan_freg_reg64(&mut self.buf, dst_reg, src_reg, float_width);
    }

    fn build_num_is_infinite(&mut self, dst: &Symbol, src: &Symbol, arg_layout: &InLayout<'a>) {
        let dst_reg = self.storage_manager.claim_general_reg(&mut self.buf, dst);
        let src_reg = self.storage_manager.load_to_float_reg(&mut self.buf, src);

        self.storage_manager.with_tmp_general_reg(
            &mut self.buf,
            |_storage_manager, buf, mask_reg| {
                match *arg_layout {
                    Layout::F32 => {
                        ASM::mov_reg64_imm64(buf, mask_reg, 0x7fff_ffff);
                        ASM::xor_reg64_reg64_reg64(buf, dst_reg, dst_reg, dst_reg); // zero out dst reg
                        ASM::mov_reg32_freg32(buf, dst_reg, src_reg);
                        ASM::and_reg64_reg64_reg64(buf, dst_reg, dst_reg, mask_reg);

                        ASM::mov_reg64_imm64(buf, mask_reg, 0x7f80_0000);
                        ASM::eq_reg_reg_reg(buf, RegisterWidth::W32, dst_reg, dst_reg, mask_reg);
                    }
                    Layout::F64 => {
                        ASM::mov_reg64_imm64(buf, mask_reg, 0x7fff_ffff_ffff_ffff);
                        ASM::mov_reg64_freg64(buf, dst_reg, src_reg);
                        ASM::and_reg64_reg64_reg64(buf, dst_reg, dst_reg, mask_reg);

                        ASM::mov_reg64_imm64(buf, mask_reg, 0x7ff0_0000_0000_0000);
                        ASM::eq_reg_reg_reg(buf, RegisterWidth::W64, dst_reg, dst_reg, mask_reg);
                    }
                    _ => unreachable!(),
                }
            },
        );
    }

    fn build_num_is_finite(&mut self, dst: &Symbol, src: &Symbol, arg_layout: &InLayout<'a>) {
        let dst_reg = self.storage_manager.claim_general_reg(&mut self.buf, dst);
        let src_reg = self.storage_manager.load_to_float_reg(&mut self.buf, src);

        self.storage_manager.with_tmp_general_reg(
            &mut self.buf,
            |_storage_manager, buf, mask_reg| {
                match *arg_layout {
                    Layout::F32 => {
                        ASM::mov_reg64_imm64(buf, mask_reg, 0x7f80_0000);
                        ASM::xor_reg64_reg64_reg64(buf, dst_reg, dst_reg, dst_reg); // zero out dst reg
                        ASM::mov_reg32_freg32(buf, dst_reg, src_reg);
                        ASM::and_reg64_reg64_reg64(buf, dst_reg, dst_reg, mask_reg);
                        ASM::neq_reg_reg_reg(buf, RegisterWidth::W32, dst_reg, dst_reg, mask_reg);
                    }
                    Layout::F64 => {
                        ASM::mov_reg64_imm64(buf, mask_reg, 0x7ff0_0000_0000_0000);
                        ASM::mov_reg64_freg64(buf, dst_reg, src_reg);
                        ASM::and_reg64_reg64_reg64(buf, dst_reg, dst_reg, mask_reg);
                        ASM::neq_reg_reg_reg(buf, RegisterWidth::W64, dst_reg, dst_reg, mask_reg);
                    }
                    _ => unreachable!(),
                }
            },
        );
    }

    fn build_num_cmp(
        &mut self,
        dst: &Symbol,
        src1: &Symbol,
        src2: &Symbol,
        arg_layout: &InLayout<'a>,
    ) {
        // This implements the expression:
        //            (x != y) as u8 + (x < y) as u8
        // For x==y:  (false as u8)  + (false as u8) = 0 = RocOrder::Eq
        // For x>y:   (true as u8)   + (false as u8) = 1 = RocOrder::Gt
        // For x<y:   (true as u8)   + (true as u8)  = 2 = RocOrder::Lt
        // u8 is represented in the stack machine as i32, but written to memory as 1 byte
        let not_equal = self.debug_symbol("not_equal");

        self.build_neq(&not_equal, src1, src2, arg_layout);
        self.build_num_lt(dst, src1, src2, arg_layout);

        let neq_reg = self
            .storage_manager
            .load_to_general_reg(&mut self.buf, &not_equal);
        let dst_reg = self.storage_manager.load_to_general_reg(&mut self.buf, dst);

        ASM::add_reg64_reg64_reg64(&mut self.buf, dst_reg, dst_reg, neq_reg);

        self.free_symbol(&not_equal);
    }

    fn build_num_lt(
        &mut self,
        dst: &Symbol,
        src1: &Symbol,
        src2: &Symbol,
        arg_layout: &InLayout<'a>,
    ) {
        self.compare(CompareOperation::LessThan, dst, src1, src2, arg_layout)
    }

    fn build_num_gt(
        &mut self,
        dst: &Symbol,
        src1: &Symbol,
        src2: &Symbol,
        arg_layout: &InLayout<'a>,
    ) {
        self.compare(CompareOperation::GreaterThan, dst, src1, src2, arg_layout)
    }

    fn build_num_lte(
        &mut self,
        dst: &Symbol,
        src1: &Symbol,
        src2: &Symbol,
        arg_layout: &InLayout<'a>,
    ) {
        self.compare(
            CompareOperation::LessThanOrEqual,
            dst,
            src1,
            src2,
            arg_layout,
        )
    }

    fn build_num_gte(
        &mut self,
        dst: &Symbol,
        src1: &Symbol,
        src2: &Symbol,
        arg_layout: &InLayout<'a>,
    ) {
        self.compare(
            CompareOperation::GreaterThanOrEqual,
            dst,
            src1,
            src2,
            arg_layout,
        )
    }

    fn build_indirect_inc(&mut self, layout: InLayout<'a>) -> Symbol {
        let ident_ids = self
            .interns
            .all_ident_ids
            .get_mut(&self.env.module_id)
            .unwrap();

        let (refcount_proc_name, linker_data) = self.helper_proc_gen.gen_refcount_proc(
            ident_ids,
            self.layout_interner,
            layout,
            HelperOp::IndirectInc,
        );

        self.helper_proc_symbols_mut().extend(linker_data);

        refcount_proc_name
    }

    fn build_indirect_dec(&mut self, layout: InLayout<'a>) -> Symbol {
        let ident_ids = self
            .interns
            .all_ident_ids
            .get_mut(&self.env.module_id)
            .unwrap();

        let (refcount_proc_name, linker_data) = self.helper_proc_gen.gen_refcount_proc(
            ident_ids,
            self.layout_interner,
            layout,
            HelperOp::IndirectDec,
        );

        self.helper_proc_symbols_mut().extend(linker_data);

        refcount_proc_name
    }

    fn build_higher_order_lowlevel(
        &mut self,
        dst: &Symbol,
        higher_order: &HigherOrderLowLevel<'a>,
        ret_layout: InLayout<'a>,
    ) {
        let ident_ids = self
            .interns
            .all_ident_ids
            .get_mut(&self.env.module_id)
            .unwrap();

        let caller_proc = CallerProc::new(
            self.env.arena,
            self.env.module_id,
            ident_ids,
            self.layout_interner,
            &higher_order.passed_function,
            higher_order.closure_env_layout,
        );

        match higher_order.op {
            HigherOrder::ListMap { xs } => {
                let old_element_layout = higher_order.passed_function.argument_layouts[0];
                let new_element_layout = higher_order.passed_function.return_layout;

                let input_list_layout = LayoutRepr::Builtin(Builtin::List(old_element_layout));
                let input_list_in_layout = self
                    .layout_interner
                    .insert_direct_no_semantic(input_list_layout);

                let caller = self.debug_symbol("caller");
                let data = self.debug_symbol("data");
                let alignment = self.debug_symbol("alignment");
                let old_element_width = self.debug_symbol("old_element_width");
                let new_element_width = self.debug_symbol("new_element_width");

                self.load_layout_alignment(new_element_layout, alignment);

                self.load_layout_stack_size(old_element_layout, old_element_width);
                self.load_layout_stack_size(new_element_layout, new_element_width);

                let caller_string = self.lambda_name_to_string(
                    LambdaName::no_niche(caller_proc.proc_symbol),
                    std::iter::empty(),
                    None,
                    Layout::UNIT,
                );

                // self.helper_proc_symbols .extend([(caller_proc.proc_symbol, caller_proc.proc_layout)]);
                self.caller_procs.push(caller_proc);

                // function pointer to a function that takes a pointer, and increments
                let inc_n_data = if let Some(closure_env_layout) = higher_order.closure_env_layout {
                    self.increment_fn_pointer(closure_env_layout)
                } else {
                    // null pointer
                    self.load_literal_i64(&Symbol::DEV_TMP, 0);
                    Symbol::DEV_TMP
                };

                self.build_fn_pointer(&caller, caller_string);

                if let Some(_closure_data_layout) = higher_order.closure_env_layout {
                    let data_symbol = higher_order.passed_function.captured_environment;
                    self.storage_manager
                        .ensure_symbol_on_stack(&mut self.buf, &data_symbol);
                    let (new_elem_offset, _) =
                        self.storage_manager.stack_offset_and_size(&data_symbol);

                    // Load address of output element into register.
                    let reg = self.storage_manager.claim_general_reg(&mut self.buf, &data);
                    ASM::add_reg64_reg64_imm32(
                        &mut self.buf,
                        reg,
                        CC::BASE_PTR_REG,
                        new_elem_offset,
                    );
                } else {
                    // use a null pointer
                    self.load_literal(&data, &Layout::U64, &Literal::Int(0u128.to_be_bytes()));
                }

                // we pass a null pointer when the data is not owned. the zig code must not call this!
                let data_is_owned = higher_order.closure_env_layout.is_some()
                    && higher_order.passed_function.owns_captured_environment;

                self.load_literal(
                    &Symbol::DEV_TMP2,
                    &Layout::BOOL,
                    &Literal::Bool(data_is_owned),
                );

                //    list: RocList,
                //    caller: Caller1,
                //    data: Opaque,
                //    inc_n_data: IncN,
                //    data_is_owned: bool,
                //    alignment: u32,
                //    old_element_width: usize,
                //    new_element_width: usize,

                let arguments = [
                    xs,
                    caller,
                    data,
                    inc_n_data,
                    Symbol::DEV_TMP2,
                    alignment,
                    old_element_width,
                    new_element_width,
                ];

                let ptr = Layout::U64;
                let usize_ = Layout::U64;

                let layouts = [
                    input_list_in_layout,
                    ptr,
                    ptr,
                    ptr,
                    Layout::BOOL,
                    Layout::U32,
                    usize_,
                    usize_,
                ];

                // Setup the return location.
                let base_offset = self
                    .storage_manager
                    .claim_stack_area(dst, self.layout_interner.stack_size(ret_layout));

                self.build_fn_call(
                    &Symbol::DEV_TMP3,
                    bitcode::LIST_MAP.to_string(),
                    &arguments,
                    &layouts,
                    &ret_layout,
                );

                self.free_symbol(&Symbol::DEV_TMP);
                self.free_symbol(&Symbol::DEV_TMP2);

                // Return list value from fn call
                self.storage_manager.copy_symbol_to_stack_offset(
                    self.layout_interner,
                    &mut self.buf,
                    base_offset,
                    &Symbol::DEV_TMP3,
                    &ret_layout,
                );

                self.free_symbol(&Symbol::DEV_TMP3);
            }
            HigherOrder::ListMap2 { .. } => todo!(),
            HigherOrder::ListMap3 { .. } => todo!(),
            HigherOrder::ListMap4 { .. } => todo!(),
            HigherOrder::ListSortWith { .. } => todo!(),
        }
    }

    fn build_list_len(&mut self, dst: &Symbol, list: &Symbol) {
        self.storage_manager.list_len(&mut self.buf, dst, list);
    }

    fn build_list_with_capacity(
        &mut self,
        dst: &Symbol,
        capacity: Symbol,
        capacity_layout: InLayout<'a>,
        elem_layout: InLayout<'a>,
        ret_layout: &InLayout<'a>,
    ) {
        // List alignment argument (u32).
        self.load_layout_alignment(*ret_layout, Symbol::DEV_TMP);

        // Load element_width argument (usize).
        self.load_layout_stack_size(elem_layout, Symbol::DEV_TMP2);

        // Setup the return location.
        let base_offset = self
            .storage_manager
            .claim_stack_area(dst, self.layout_interner.stack_size(*ret_layout));

        let lowlevel_args = [
            capacity,
            // alignment
            Symbol::DEV_TMP,
            // element_width
            Symbol::DEV_TMP2,
        ];
        let lowlevel_arg_layouts = [capacity_layout, Layout::U32, Layout::U64];

        self.build_fn_call(
            &Symbol::DEV_TMP3,
            bitcode::LIST_WITH_CAPACITY.to_string(),
            &lowlevel_args,
            &lowlevel_arg_layouts,
            ret_layout,
        );
        self.free_symbol(&Symbol::DEV_TMP);
        self.free_symbol(&Symbol::DEV_TMP2);

        // Copy from list to the output record.
        self.storage_manager.copy_symbol_to_stack_offset(
            self.layout_interner,
            &mut self.buf,
            base_offset,
            &Symbol::DEV_TMP3,
            ret_layout,
        );

        self.free_symbol(&Symbol::DEV_TMP3);
    }

    fn build_list_reserve(
        &mut self,
        dst: &Symbol,
        args: &'a [Symbol],
        arg_layouts: &[InLayout<'a>],
        ret_layout: &InLayout<'a>,
    ) {
        let list = args[0];
        let list_layout = arg_layouts[0];
        let spare = args[1];
        let spare_layout = arg_layouts[1];

        // Load list alignment argument (u32).
        self.load_layout_alignment(list_layout, Symbol::DEV_TMP);

        // Load element_width argument (usize).
        self.load_layout_stack_size(*ret_layout, Symbol::DEV_TMP2);

        // Load UpdateMode.Immutable argument (0u8)
        let u8_layout = Layout::U8;
        let update_mode = 0u8;
        self.load_literal(
            &Symbol::DEV_TMP3,
            &u8_layout,
            &Literal::Int((update_mode as i128).to_ne_bytes()),
        );

        // Setup the return location.
        let base_offset = self
            .storage_manager
            .claim_stack_area(dst, self.layout_interner.stack_size(*ret_layout));

        let lowlevel_args = bumpalo::vec![
        in self.env.arena;
            list,
            // alignment
            Symbol::DEV_TMP,
            spare,
            // element_width
            Symbol::DEV_TMP2,
            // update_mode
            Symbol::DEV_TMP3,

         ];
        let lowlevel_arg_layouts = [
            list_layout,
            Layout::U32,
            spare_layout,
            Layout::U64,
            u8_layout,
        ];

        self.build_fn_call(
            &Symbol::DEV_TMP4,
            bitcode::LIST_RESERVE.to_string(),
            &lowlevel_args,
            &lowlevel_arg_layouts,
            ret_layout,
        );
        self.free_symbol(&Symbol::DEV_TMP);
        self.free_symbol(&Symbol::DEV_TMP2);
        self.free_symbol(&Symbol::DEV_TMP3);

        // Return list value from fn call
        self.storage_manager.copy_symbol_to_stack_offset(
            self.layout_interner,
            &mut self.buf,
            base_offset,
            &Symbol::DEV_TMP4,
            ret_layout,
        );

        self.free_symbol(&Symbol::DEV_TMP4);
    }

    fn build_list_append_unsafe(
        &mut self,
        dst: &Symbol,
        args: &'a [Symbol],
        arg_layouts: &[InLayout<'a>],
        ret_layout: &InLayout<'a>,
    ) {
        let list = args[0];
        let list_layout = arg_layouts[0];
        let elem = args[1];
        let elem_layout = arg_layouts[1];

        // Have to pass the input element by pointer, so put it on the stack and load it's address.
        self.storage_manager
            .ensure_symbol_on_stack(&mut self.buf, &elem);
        let (new_elem_offset, _) = self.storage_manager.stack_offset_and_size(&elem);

        // Load address of output element into register.
        let reg = self
            .storage_manager
            .claim_general_reg(&mut self.buf, &Symbol::DEV_TMP);
        ASM::add_reg64_reg64_imm32(&mut self.buf, reg, CC::BASE_PTR_REG, new_elem_offset);

        // Load element_witdh argument (usize).
        self.load_layout_stack_size(elem_layout, Symbol::DEV_TMP2);

        // Setup the return location.
        let base_offset = self
            .storage_manager
            .claim_stack_area(dst, self.layout_interner.stack_size(*ret_layout));

        let lowlevel_args = [
            list,
            // element
            Symbol::DEV_TMP,
            // element_width
            Symbol::DEV_TMP2,
        ];
        let lowlevel_arg_layouts = [list_layout, Layout::U64, Layout::U64];

        self.build_fn_call(
            &Symbol::DEV_TMP3,
            bitcode::LIST_APPEND_UNSAFE.to_string(),
            &lowlevel_args,
            &lowlevel_arg_layouts,
            ret_layout,
        );
        self.free_symbol(&Symbol::DEV_TMP);
        self.free_symbol(&Symbol::DEV_TMP2);

        // Return list value from fn call
        self.storage_manager.copy_symbol_to_stack_offset(
            self.layout_interner,
            &mut self.buf,
            base_offset,
            &Symbol::DEV_TMP3,
            ret_layout,
        );

        self.free_symbol(&Symbol::DEV_TMP3);
    }

    fn build_list_get_unsafe(
        &mut self,
        dst: &Symbol,
        list: &Symbol,
        index: &Symbol,
        ret_layout: &InLayout<'a>,
    ) {
        let (base_offset, _) = self.storage_manager.stack_offset_and_size(list);
        let index_reg = self
            .storage_manager
            .load_to_general_reg(&mut self.buf, index);
        let ret_stack_size = self.layout_interner.stack_size(*ret_layout);
        // TODO: This can be optimized with smarter instructions.
        // Also can probably be moved into storage manager at least partly.
        self.storage_manager.with_tmp_general_reg(
            &mut self.buf,
            |storage_manager, buf, list_ptr| {
                ASM::mov_reg64_base32(buf, list_ptr, base_offset);
                storage_manager.with_tmp_general_reg(buf, |storage_manager, buf, tmp| {
                    // calculate `element_width * index`
                    ASM::mov_reg64_imm64(buf, tmp, ret_stack_size as i64);
                    ASM::imul_reg64_reg64_reg64(buf, tmp, tmp, index_reg);

                    // add the offset to the list pointer, store in `tmp`
                    ASM::add_reg64_reg64_reg64(buf, tmp, tmp, list_ptr);
                    let element_ptr = tmp;

                    Self::ptr_read(
                        buf,
                        storage_manager,
                        self.layout_interner,
                        element_ptr,
                        0,
                        *ret_layout,
                        *dst,
                    );
                });
            },
        );
    }

    fn build_list_replace_unsafe(
        &mut self,
        dst: &Symbol,
        args: &'a [Symbol],
        arg_layouts: &[InLayout<'a>],
        ret_layout: &InLayout<'a>,
    ) {
        // We want to delegate to the zig builtin, but it takes some extra parameters.
        // Firstly, it takes the alignment of the list.
        // Secondly, it takes the stack size of an element.
        // Thirdly, it takes a pointer that it will write the output element to.
        let list = args[0];
        let list_layout = arg_layouts[0];
        let index = args[1];
        let index_layout = arg_layouts[1];
        let elem = args[2];
        let elem_layout = arg_layouts[2];

        // Load list alignment argument (u32).
        self.load_layout_alignment(list_layout, Symbol::DEV_TMP);

        // Have to pass the input element by pointer, so put it on the stack and load it's address.
        self.storage_manager
            .ensure_symbol_on_stack(&mut self.buf, &elem);
        let u64_layout = Layout::U64;
        let (new_elem_offset, _) = self.storage_manager.stack_offset_and_size(&elem);
        // Load address of output element into register.
        let reg = self
            .storage_manager
            .claim_general_reg(&mut self.buf, &Symbol::DEV_TMP2);
        ASM::add_reg64_reg64_imm32(&mut self.buf, reg, CC::BASE_PTR_REG, new_elem_offset);

        // Load the elements size.
        self.load_layout_stack_size(elem_layout, Symbol::DEV_TMP3);

        // Setup the return location.
        let base_offset = self
            .storage_manager
            .claim_stack_area(dst, self.layout_interner.stack_size(*ret_layout));

        let ret_fields =
            if let LayoutRepr::Struct(field_layouts) = self.layout_interner.get_repr(*ret_layout) {
                field_layouts
            } else {
                internal_error!(
                    "Expected replace to return a struct instead found: {:?}",
                    ret_layout
                )
            };

        // Only return list and old element.
        debug_assert_eq!(ret_fields.len(), 2);

        let (out_list_offset, out_elem_offset) = if ret_fields[0] == elem_layout {
            (
                base_offset + self.layout_interner.stack_size(ret_fields[0]) as i32,
                base_offset,
            )
        } else {
            (
                base_offset,
                base_offset + self.layout_interner.stack_size(ret_fields[0]) as i32,
            )
        };

        // Load address of output element into register.
        let reg = self
            .storage_manager
            .claim_general_reg(&mut self.buf, &Symbol::DEV_TMP4);
        ASM::add_reg64_reg64_imm32(&mut self.buf, reg, CC::BASE_PTR_REG, out_elem_offset);

        let lowlevel_args = bumpalo::vec![
        in self.env.arena;
            list,
            Symbol::DEV_TMP,
            index,
            Symbol::DEV_TMP2,
            Symbol::DEV_TMP3,
            Symbol::DEV_TMP4,
         ];
        let lowlevel_arg_layouts = [
            list_layout,
            Layout::U32,
            index_layout,
            u64_layout,
            u64_layout,
            u64_layout,
        ];

        self.build_fn_call(
            &Symbol::DEV_TMP5,
            bitcode::LIST_REPLACE.to_string(),
            &lowlevel_args,
            &lowlevel_arg_layouts,
            &list_layout,
        );
        self.free_symbol(&Symbol::DEV_TMP);
        self.free_symbol(&Symbol::DEV_TMP2);
        self.free_symbol(&Symbol::DEV_TMP3);
        self.free_symbol(&Symbol::DEV_TMP4);

        // Copy from list to the output record.
        self.storage_manager.copy_symbol_to_stack_offset(
            self.layout_interner,
            &mut self.buf,
            out_list_offset,
            &Symbol::DEV_TMP5,
            &list_layout,
        );

        self.free_symbol(&Symbol::DEV_TMP5);
    }

    fn build_list_concat(
        &mut self,
        dst: &Symbol,
        args: &'a [Symbol],
        arg_layouts: &[InLayout<'a>],
        elem_layout: InLayout<'a>,
        ret_layout: &InLayout<'a>,
    ) {
        let list_a = args[0];
        let list_a_layout = arg_layouts[0];
        let list_b = args[1];
        let list_b_layout = arg_layouts[1];

        // Load list alignment argument (u32).
        self.load_layout_alignment(*ret_layout, Symbol::DEV_TMP);

        // Load element_width argument (usize).
        self.load_layout_stack_size(elem_layout, Symbol::DEV_TMP2);

        // Setup the return location.
        let base_offset = self
            .storage_manager
            .claim_stack_area(dst, self.layout_interner.stack_size(*ret_layout));

        let lowlevel_args = bumpalo::vec![
        in self.env.arena;
            list_a,
            list_b,
            // alignment
            Symbol::DEV_TMP,
            // element_width
            Symbol::DEV_TMP2,
         ];
        let lowlevel_arg_layouts = [list_a_layout, list_b_layout, Layout::U32, Layout::U64];

        self.build_fn_call(
            &Symbol::DEV_TMP3,
            bitcode::LIST_CONCAT.to_string(),
            &lowlevel_args,
            &lowlevel_arg_layouts,
            ret_layout,
        );

        self.free_symbol(&Symbol::DEV_TMP);
        self.free_symbol(&Symbol::DEV_TMP2);

        // Return list value from fn call
        self.storage_manager.copy_symbol_to_stack_offset(
            self.layout_interner,
            &mut self.buf,
            base_offset,
            &Symbol::DEV_TMP3,
            ret_layout,
        );

        self.free_symbol(&Symbol::DEV_TMP3);
    }

    fn build_list_prepend(
        &mut self,
        dst: &Symbol,
        args: &'a [Symbol],
        arg_layouts: &[InLayout<'a>],
        ret_layout: &InLayout<'a>,
    ) {
        let list = args[0];
        let list_layout = arg_layouts[0];
        let elem = args[1];
        let elem_layout = arg_layouts[1];

        // List alignment argument (u32).
        self.load_layout_alignment(*ret_layout, Symbol::DEV_TMP);

        // Have to pass the input element by pointer, so put it on the stack and load it's address.
        self.storage_manager
            .ensure_symbol_on_stack(&mut self.buf, &elem);
        let (new_elem_offset, _) = self.storage_manager.stack_offset_and_size(&elem);

        // Load address of input element into register.
        let reg = self
            .storage_manager
            .claim_general_reg(&mut self.buf, &Symbol::DEV_TMP2);
        ASM::add_reg64_reg64_imm32(&mut self.buf, reg, CC::BASE_PTR_REG, new_elem_offset);

        // Load element_witdh argument (usize).
        self.load_layout_stack_size(elem_layout, Symbol::DEV_TMP3);

        // Setup the return location.
        let base_offset = self
            .storage_manager
            .claim_stack_area(dst, self.layout_interner.stack_size(*ret_layout));

        let lowlevel_args = [
            list,
            // alignment
            Symbol::DEV_TMP,
            // element
            Symbol::DEV_TMP2,
            // element_width
            Symbol::DEV_TMP3,
        ];
        let lowlevel_arg_layouts = [list_layout, Layout::U32, Layout::U64, Layout::U64];

        self.build_fn_call(
            &Symbol::DEV_TMP4,
            bitcode::LIST_PREPEND.to_string(),
            &lowlevel_args,
            &lowlevel_arg_layouts,
            ret_layout,
        );
        self.free_symbol(&Symbol::DEV_TMP);
        self.free_symbol(&Symbol::DEV_TMP2);
        self.free_symbol(&Symbol::DEV_TMP3);

        // Return list value from fn call
        self.storage_manager.copy_symbol_to_stack_offset(
            self.layout_interner,
            &mut self.buf,
            base_offset,
            &Symbol::DEV_TMP4,
            ret_layout,
        );

        self.free_symbol(&Symbol::DEV_TMP4);
    }

    fn build_ptr_cast(&mut self, dst: &Symbol, src: &Symbol) {
        let src_reg = self.storage_manager.load_to_general_reg(&mut self.buf, src);
        let dst_reg = self.storage_manager.claim_general_reg(&mut self.buf, dst);

        ASM::mov_reg64_reg64(&mut self.buf, dst_reg, src_reg)
    }

    fn create_empty_array(&mut self, sym: &Symbol) {
        let base_offset = self.storage_manager.claim_stack_area(sym, 24);
        self.storage_manager
            .with_tmp_general_reg(&mut self.buf, |_storage_manager, buf, reg| {
                ASM::mov_reg64_imm64(buf, reg, 0);
                ASM::mov_base32_reg64(buf, base_offset, reg);
                ASM::mov_base32_reg64(buf, base_offset + 8, reg);
                ASM::mov_base32_reg64(buf, base_offset + 16, reg);
            });
    }

    fn create_array(
        &mut self,
        sym: &Symbol,
        element_in_layout: &InLayout<'a>,
        elements: &[ListLiteralElement<'a>],
    ) {
        let element_layout = self.layout_interner.get_repr(*element_in_layout);
        let element_width = self.layout_interner.stack_size(*element_in_layout) as u64;

        // load the total size of the data we want to store (excludes refcount)
        let data_bytes_symbol = Symbol::DEV_TMP;
        let data_bytes = element_width * elements.len() as u64;
        self.load_literal(
            &data_bytes_symbol,
            &Layout::U64,
            &Literal::Int((data_bytes as i128).to_ne_bytes()),
        );

        // Load allocation alignment (u32)
        let element_alignment_symbol = Symbol::DEV_TMP2;
        self.load_layout_alignment(Layout::U32, element_alignment_symbol);

        self.allocate_with_refcount(
            Symbol::DEV_TMP3,
            data_bytes_symbol,
            element_alignment_symbol,
        );

        self.free_symbol(&data_bytes_symbol);
        self.free_symbol(&element_alignment_symbol);

        // The pointer already points to the first element
        let ptr_reg = self
            .storage_manager
            .load_to_general_reg(&mut self.buf, &Symbol::DEV_TMP3);

        // Copy everything into output array.
        let mut element_offset = 0;
        for elem in elements {
            // TODO: this could be a lot faster when loading large lists
            // if we move matching on the element layout to outside this loop.
            // It also greatly bloats the code here.
            // Refactor this and switch to one external match.
            // We also could make loadining indivitual literals much faster
            let element_symbol = match elem {
                ListLiteralElement::Symbol(sym) => {
                    self.load_literal_symbols(&[*sym]);
                    *sym
                }
                ListLiteralElement::Literal(lit) => {
                    self.load_literal(&Symbol::DEV_TMP, element_in_layout, lit);
                    Symbol::DEV_TMP
                }
            };

            Self::ptr_write(
                &mut self.buf,
                &mut self.storage_manager,
                self.layout_interner,
                ptr_reg,
                element_offset,
                element_width,
                element_layout,
                element_symbol,
            );

            element_offset += element_width as i32;
            if element_symbol == Symbol::DEV_TMP {
                self.free_symbol(&element_symbol);
            }
        }

        // Setup list on stack.
        self.storage_manager.with_tmp_general_reg(
            &mut self.buf,
            |storage_manager, buf, tmp_reg| {
                let base_offset = storage_manager.claim_stack_area(sym, 24);
                ASM::mov_base32_reg64(buf, base_offset, ptr_reg);

                ASM::mov_reg64_imm64(buf, tmp_reg, elements.len() as i64);
                ASM::mov_base32_reg64(buf, base_offset + 8, tmp_reg);
                ASM::mov_base32_reg64(buf, base_offset + 16, tmp_reg);
            },
        );
        self.free_symbol(&Symbol::DEV_TMP3);
    }

    fn create_struct(&mut self, sym: &Symbol, layout: &InLayout<'a>, fields: &'a [Symbol]) {
        self.storage_manager.create_struct(
            self.layout_interner,
            &mut self.buf,
            sym,
            layout,
            fields,
        );
    }

    fn load_struct_at_index(
        &mut self,
        sym: &Symbol,
        structure: &Symbol,
        index: u64,
        field_layouts: &'a [InLayout<'a>],
    ) {
        self.storage_manager.load_field_at_index(
            self.layout_interner,
            sym,
            structure,
            index,
            field_layouts,
        );
    }

    fn load_union_at_index(
        &mut self,
        sym: &Symbol,
        structure: &Symbol,
        tag_id: TagIdIntType,
        index: u64,
        union_layout: &UnionLayout<'a>,
    ) {
        match union_layout {
            UnionLayout::NonRecursive(tag_layouts) => {
                self.storage_manager.load_field_at_index(
                    self.layout_interner,
                    sym,
                    structure,
                    index,
                    tag_layouts[tag_id as usize],
                );
            }
            UnionLayout::NonNullableUnwrapped(field_layouts) => {
                let element_layout = field_layouts[index as usize];

                let ptr_reg = self
                    .storage_manager
                    .load_to_general_reg(&mut self.buf, structure);

                let mut offset = 0;
                for field in &field_layouts[..index as usize] {
                    offset += self.layout_interner.stack_size(*field);
                }

                Self::ptr_read(
                    &mut self.buf,
                    &mut self.storage_manager,
                    self.layout_interner,
                    ptr_reg,
                    offset as i32,
                    element_layout,
                    *sym,
                );
            }
            UnionLayout::NullableUnwrapped {
                nullable_id,
                other_fields,
            } => {
                debug_assert_ne!(tag_id, *nullable_id as TagIdIntType);

                let element_layout = other_fields[index as usize];

                let ptr_reg = self
                    .storage_manager
                    .load_to_general_reg(&mut self.buf, structure);

                let mut offset = 0;
                for field in &other_fields[..index as usize] {
                    offset += self.layout_interner.stack_size(*field);
                }

                Self::ptr_read(
                    &mut self.buf,
                    &mut self.storage_manager,
                    self.layout_interner,
                    ptr_reg,
                    offset as i32,
                    element_layout,
                    *sym,
                );
            }

            UnionLayout::NullableWrapped {
                nullable_id,
                other_tags,
            } => {
                debug_assert_ne!(tag_id, *nullable_id as TagIdIntType);

                let other_fields = if tag_id < *nullable_id {
                    other_tags[tag_id as usize]
                } else {
                    other_tags[tag_id as usize - 1]
                };

                let element_layout = other_fields[index as usize];

                let ptr_reg = self
                    .storage_manager
                    .load_to_general_reg(&mut self.buf, structure);

                let (mask_symbol, mask_reg) = self.clear_tag_id(ptr_reg);

                let mut offset = 0;
                for field in &other_fields[..index as usize] {
                    offset += self.layout_interner.stack_size(*field);
                }

                Self::ptr_read(
                    &mut self.buf,
                    &mut self.storage_manager,
                    self.layout_interner,
                    mask_reg,
                    offset as i32,
                    element_layout,
                    *sym,
                );

                self.free_symbol(&mask_symbol)
            }
            UnionLayout::Recursive(tag_layouts) => {
                let other_fields = tag_layouts[tag_id as usize];
                let element_layout = other_fields[index as usize];

                let ptr_reg = self
                    .storage_manager
                    .load_to_general_reg(&mut self.buf, structure);

                // mask out the tag id bits
                let (unmasked_symbol, unmasked_reg) =
                    if union_layout.stores_tag_id_as_data(self.storage_manager.target_info) {
                        (None, ptr_reg)
                    } else {
                        let (mask_symbol, mask_reg) = self.clear_tag_id(ptr_reg);
                        (Some(mask_symbol), mask_reg)
                    };

                let mut offset = 0;
                for field in &other_fields[..index as usize] {
                    offset += self.layout_interner.stack_size(*field);
                }

                Self::ptr_read(
                    &mut self.buf,
                    &mut self.storage_manager,
                    self.layout_interner,
                    unmasked_reg,
                    offset as i32,
                    element_layout,
                    *sym,
                );

                if let Some(unmasked_symbol) = unmasked_symbol {
                    self.free_symbol(&unmasked_symbol);
                }
            }
        }
    }

    fn load_union_field_ptr_at_index(
        &mut self,
        sym: &Symbol,
        structure: &Symbol,
        tag_id: TagIdIntType,
        index: u64,
        union_layout: &UnionLayout<'a>,
    ) {
        let ptr_reg = self
            .storage_manager
            .load_to_general_reg(&mut self.buf, structure);

        let sym_reg = self.storage_manager.claim_general_reg(&mut self.buf, sym);

        match union_layout {
            UnionLayout::NonRecursive(_) => {
                unreachable!("operation not supported")
            }
            UnionLayout::NonNullableUnwrapped(field_layouts) => {
                let mut offset = 0;
                for field in &field_layouts[..index as usize] {
                    offset += self.layout_interner.stack_size(*field);
                }

                ASM::add_reg64_reg64_imm32(&mut self.buf, sym_reg, ptr_reg, offset as i32);
            }
            UnionLayout::NullableUnwrapped {
                nullable_id,
                other_fields,
            } => {
                debug_assert_ne!(tag_id, *nullable_id as TagIdIntType);

                let mut offset = 0;
                for field in &other_fields[..index as usize] {
                    offset += self.layout_interner.stack_size(*field);
                }

                ASM::add_reg64_reg64_imm32(&mut self.buf, sym_reg, ptr_reg, offset as i32);
            }

            UnionLayout::NullableWrapped {
                nullable_id,
                other_tags,
            } => {
                debug_assert_ne!(tag_id, *nullable_id as TagIdIntType);

                let other_fields = if tag_id < *nullable_id {
                    other_tags[tag_id as usize]
                } else {
                    other_tags[tag_id as usize - 1]
                };

                let (mask_symbol, mask_reg) = self.clear_tag_id(ptr_reg);

                let mut offset = 0;
                for field in &other_fields[..index as usize] {
                    offset += self.layout_interner.stack_size(*field);
                }

                ASM::add_reg64_reg64_imm32(&mut self.buf, sym_reg, mask_reg, offset as i32);

                self.free_symbol(&mask_symbol);
            }
            UnionLayout::Recursive(tag_layouts) => {
                let other_fields = tag_layouts[tag_id as usize];

                let ptr_reg = self
                    .storage_manager
                    .load_to_general_reg(&mut self.buf, structure);

                // mask out the tag id bits
                let (unmasked_symbol, unmasked_reg) =
                    if union_layout.stores_tag_id_as_data(self.storage_manager.target_info) {
                        (None, ptr_reg)
                    } else {
                        let (mask_symbol, mask_reg) = self.clear_tag_id(ptr_reg);
                        (Some(mask_symbol), mask_reg)
                    };

                let mut offset = 0;
                for field in &other_fields[..index as usize] {
                    offset += self.layout_interner.stack_size(*field);
                }

                ASM::add_reg64_reg64_imm32(&mut self.buf, sym_reg, unmasked_reg, offset as i32);

                if let Some(unmasked_symbol) = unmasked_symbol {
                    self.free_symbol(&unmasked_symbol);
                }
            }
        }
    }

    fn build_ptr_store(
        &mut self,
        sym: Symbol,
        ptr: Symbol,
        value: Symbol,
        element_layout: InLayout<'a>,
    ) {
        let ptr_reg = self
            .storage_manager
            .load_to_general_reg(&mut self.buf, &ptr);

        let element_width = self.layout_interner.stack_size(element_layout) as u64;
        let element_offset = 0;

        let layout = self.layout_interner.get_repr(element_layout);

        Self::ptr_write(
            &mut self.buf,
            &mut self.storage_manager,
            self.layout_interner,
            ptr_reg,
            element_offset,
            element_width,
            layout,
            value,
        );

        if value == Symbol::DEV_TMP {
            self.free_symbol(&value);
        }

        // box is just a pointer on the stack
        let base_offset = self.storage_manager.claim_pointer_stack_area(sym);
        ASM::mov_base32_reg64(&mut self.buf, base_offset, ptr_reg);
    }

    fn build_ptr_load(&mut self, sym: Symbol, ptr: Symbol, element_layout: InLayout<'a>) {
        let ptr_reg = self
            .storage_manager
            .load_to_general_reg(&mut self.buf, &ptr);

        let offset = 0;

        Self::ptr_read(
            &mut self.buf,
            &mut self.storage_manager,
            self.layout_interner,
            ptr_reg,
            offset,
            element_layout,
            sym,
        );
    }

    fn build_ptr_clear_tag_id(&mut self, sym: Symbol, ptr: Symbol) {
        let buf = &mut self.buf;

        let ptr_reg = self.storage_manager.load_to_general_reg(buf, &ptr);
        let sym_reg = self.storage_manager.claim_general_reg(buf, &sym);

        ASM::mov_reg64_imm64(buf, sym_reg, !0b111);
        ASM::and_reg64_reg64_reg64(buf, sym_reg, sym_reg, ptr_reg);
    }

    fn build_alloca(&mut self, sym: Symbol, value: Symbol, element_layout: InLayout<'a>) {
        // 1. acquire some stack space
        let element_width = self.interner().stack_size(element_layout);
        let allocation = self.debug_symbol("stack_allocation");
        let ptr = self.debug_symbol("ptr");
        let base_offset = self
            .storage_manager
            .claim_stack_area(&allocation, element_width);

        let ptr_reg = self.storage_manager.claim_general_reg(&mut self.buf, &ptr);

        ASM::mov_reg64_reg64(&mut self.buf, ptr_reg, CC::BASE_PTR_REG);
        ASM::add_reg64_reg64_imm32(&mut self.buf, ptr_reg, ptr_reg, base_offset);

        self.build_ptr_store(sym, ptr, value, element_layout);
    }

    fn expr_box(
        &mut self,
        sym: Symbol,
        value: Symbol,
        element_layout: InLayout<'a>,
        reuse: Option<Symbol>,
    ) {
        let element_width_symbol = Symbol::DEV_TMP;
        self.load_layout_stack_size(element_layout, element_width_symbol);

        // Load allocation alignment (u32)
        let element_alignment_symbol = Symbol::DEV_TMP2;
        self.load_layout_alignment(Layout::U32, element_alignment_symbol);

        let allocation = self.debug_symbol("allocation");

        match reuse {
            None => {
                self.allocate_with_refcount(
                    allocation,
                    element_width_symbol,
                    element_alignment_symbol,
                );
            }
            Some(reuse) => {
                self.allocate_with_refcount_if_null(allocation, reuse, element_layout);
            }
        };

        self.free_symbol(&element_width_symbol);
        self.free_symbol(&element_alignment_symbol);

        self.build_ptr_store(sym, allocation, value, element_layout);

        self.free_symbol(&allocation);
    }

    fn expr_unbox(&mut self, dst: Symbol, ptr: Symbol, element_layout: InLayout<'a>) {
        self.build_ptr_load(dst, ptr, element_layout)
    }

    fn get_tag_id(&mut self, sym: &Symbol, structure: &Symbol, union_layout: &UnionLayout<'a>) {
        let layout_interner: &mut STLayoutInterner<'a> = self.layout_interner;
        let _buf: &mut Vec<'a, u8> = &mut self.buf;
        match union_layout {
            UnionLayout::NonRecursive(tags) => {
                self.storage_manager.load_union_tag_id_nonrecursive(
                    layout_interner,
                    &mut self.buf,
                    sym,
                    structure,
                    tags,
                );
            }
            UnionLayout::NonNullableUnwrapped(_) => {
                let dst_reg = self.storage_manager.claim_general_reg(&mut self.buf, sym);
                ASM::mov_reg64_imm64(&mut self.buf, dst_reg, 0);
            }
            UnionLayout::NullableUnwrapped { nullable_id, .. } => {
                // simple is_null check on the pointer
                let tmp = Symbol::DEV_TMP5;
                let reg = self.storage_manager.claim_general_reg(&mut self.buf, &tmp);
                ASM::mov_reg64_imm64(&mut self.buf, reg, 0);

                let dst_reg = self.storage_manager.claim_general_reg(&mut self.buf, sym);
                let src1_reg = reg;
                let src2_reg = self
                    .storage_manager
                    .load_to_general_reg(&mut self.buf, structure);

                match *nullable_id {
                    true => {
                        ASM::eq_reg_reg_reg(
                            &mut self.buf,
                            RegisterWidth::W64,
                            dst_reg,
                            src1_reg,
                            src2_reg,
                        );
                    }
                    false => {
                        ASM::neq_reg_reg_reg(
                            &mut self.buf,
                            RegisterWidth::W64,
                            dst_reg,
                            src1_reg,
                            src2_reg,
                        );
                    }
                }

                self.free_symbol(&tmp);
            }
            UnionLayout::NullableWrapped {
                nullable_id,
                other_tags,
            } => {
                let number_of_tags = other_tags.len() + 1;

                let dst_reg = self.storage_manager.claim_general_reg(&mut self.buf, sym);

                // build a table to index into with the value that we find
                let nullable_id = *nullable_id as usize;
                let it = std::iter::once(nullable_id)
                    .chain(0..nullable_id)
                    .chain(nullable_id + 1..number_of_tags);

                let table = self.debug_symbol("tag_id_table");
                let table_offset = self
                    .storage_manager
                    .claim_stack_area(&table, (number_of_tags * 2) as _);

                let mut offset = table_offset;
                for i in it {
                    ASM::mov_reg64_imm64(&mut self.buf, dst_reg, i as i64);
                    ASM::mov_base32_reg16(&mut self.buf, offset, dst_reg);

                    offset += 2;
                }

                self.free_symbol(&table);

                // mask the 3 lowest bits
                let tmp = Symbol::DEV_TMP5;
                let reg = self.storage_manager.claim_general_reg(&mut self.buf, &tmp);
                ASM::mov_reg64_imm64(&mut self.buf, reg, 0b111);

                let src1_reg = reg;
                let src2_reg = self
                    .storage_manager
                    .load_to_general_reg(&mut self.buf, structure);

                ASM::and_reg64_reg64_reg64(&mut self.buf, dst_reg, src1_reg, src2_reg);

                // we're indexing into an array of u16, so double this index
                // also the stack grows down, so negate the index
                ASM::mov_reg64_imm64(&mut self.buf, reg, 2);
                ASM::umul_reg64_reg64_reg64(
                    &mut self.buf,
                    &mut self.storage_manager,
                    dst_reg,
                    dst_reg,
                    reg,
                );

                // index into the table
                ASM::add_reg64_reg64_reg64(&mut self.buf, dst_reg, dst_reg, CC::BASE_PTR_REG);

                // load the 16-bit value at the pointer
                ASM::mov_reg16_mem16_offset32(&mut self.buf, dst_reg, dst_reg, table_offset);

                // keep only the lowest 16 bits
                ASM::mov_reg64_imm64(&mut self.buf, reg, 0xFFFF);
                ASM::and_reg64_reg64_reg64(&mut self.buf, dst_reg, dst_reg, reg);

                self.free_symbol(&tmp);
            }

            UnionLayout::Recursive(_) => {
                let dst_reg = self.storage_manager.claim_general_reg(&mut self.buf, sym);

                let target_info = self.storage_manager.target_info;
                if union_layout.stores_tag_id_as_data(target_info) {
                    let offset = union_layout.tag_id_offset(self.interner()).unwrap() as i32;

                    let ptr_reg = self
                        .storage_manager
                        .load_to_general_reg(&mut self.buf, structure);

                    match union_layout.tag_id_layout() {
                        Layout::U8 => {
                            ASM::mov_reg8_mem8_offset32(&mut self.buf, dst_reg, ptr_reg, offset);
                            ASM::movzx_reg_reg(&mut self.buf, RegisterWidth::W8, dst_reg, dst_reg)
                        }
                        Layout::U16 => {
                            ASM::mov_reg16_mem16_offset32(&mut self.buf, dst_reg, ptr_reg, offset);
                            ASM::movzx_reg_reg(&mut self.buf, RegisterWidth::W16, dst_reg, dst_reg)
                        }
                        _ => unreachable!(),
                    }
                } else {
                    // mask the 3 lowest bits
                    let tmp = Symbol::DEV_TMP5;
                    let reg = self.storage_manager.claim_general_reg(&mut self.buf, &tmp);
                    ASM::mov_reg64_imm64(&mut self.buf, reg, 0b111);

                    let src1_reg = reg;
                    let src2_reg = self
                        .storage_manager
                        .load_to_general_reg(&mut self.buf, structure);

                    ASM::and_reg64_reg64_reg64(&mut self.buf, dst_reg, src1_reg, src2_reg);

                    self.free_symbol(&tmp);
                }
            }
        };
    }

    fn tag(
        &mut self,
        sym: &Symbol,
        fields: &'a [Symbol],
        union_layout: &UnionLayout<'a>,
        tag_id: TagIdIntType,
        reuse: Option<Symbol>,
    ) {
        let layout_interner: &mut STLayoutInterner<'a> = self.layout_interner;
        let buf: &mut Vec<'a, u8> = &mut self.buf;

        let (data_size, data_alignment) = union_layout.data_size_and_alignment(layout_interner);

        match union_layout {
            UnionLayout::NonRecursive(field_layouts) => {
                let id_offset = data_size - data_alignment;
                let base_offset = self.storage_manager.claim_stack_area(sym, data_size);
                let mut current_offset = base_offset;

                let it = fields.iter().zip(field_layouts[tag_id as usize].iter());
                for (field, field_layout) in it {
                    self.storage_manager.copy_symbol_to_stack_offset(
                        layout_interner,
                        buf,
                        current_offset,
                        field,
                        field_layout,
                    );
                    let field_size = layout_interner.stack_size(*field_layout);
                    current_offset += field_size as i32;
                }

                // put the tag id in the right place
                self.storage_manager
                    .with_tmp_general_reg(buf, |_symbol_storage, buf, reg| {
                        ASM::mov_reg64_imm64(buf, reg, tag_id as i64);

                        let total_id_offset = base_offset as u32 + id_offset;
                        debug_assert!(total_id_offset % data_alignment == 0);

                        // pick the right instruction based on the alignment of the tag id
                        if field_layouts.len() <= u8::MAX as _ {
                            ASM::mov_base32_reg8(buf, total_id_offset as i32, reg);
                        } else {
                            ASM::mov_base32_reg16(buf, total_id_offset as i32, reg);
                        }
                    });
            }
            UnionLayout::NonNullableUnwrapped(field_layouts) => {
                // construct the payload as a struct on the stack
                let temp_sym = Symbol::DEV_TMP5;
                let layout = self
                    .layout_interner
                    .insert_direct_no_semantic(LayoutRepr::Struct(field_layouts));

                self.load_literal_symbols(fields);
                self.storage_manager.create_struct(
                    self.layout_interner,
                    &mut self.buf,
                    &temp_sym,
                    &layout,
                    fields,
                );

                // now effectively box this struct
                self.expr_box(*sym, temp_sym, layout, reuse);

                self.free_symbol(&temp_sym);
            }
            UnionLayout::NullableUnwrapped {
                nullable_id,
                other_fields,
            } => {
                if tag_id == *nullable_id as TagIdIntType {
                    // it's just a null pointer
                    self.load_literal_i64(sym, 0);
                } else {
                    // construct the payload as a struct on the stack
                    let temp_sym = Symbol::DEV_TMP5;
                    let layout = self
                        .layout_interner
                        .insert_direct_no_semantic(LayoutRepr::Struct(other_fields));

                    self.load_literal_symbols(fields);
                    self.storage_manager.create_struct(
                        self.layout_interner,
                        &mut self.buf,
                        &temp_sym,
                        &layout,
                        fields,
                    );

                    // now effectively box this struct
                    self.expr_box(*sym, temp_sym, layout, reuse);

                    self.free_symbol(&temp_sym);
                }
            }
            UnionLayout::NullableWrapped {
                nullable_id,
                other_tags,
            } => {
                let nullable_id = *nullable_id;

                if tag_id == nullable_id as TagIdIntType {
                    // it's just a null pointer
                    self.load_literal_i64(sym, 0);
                } else {
                    let other_fields = if tag_id < nullable_id {
                        other_tags[tag_id as usize]
                    } else {
                        other_tags[tag_id as usize - 1]
                    };

                    // construct the payload as a struct on the stack
                    let temp_sym = Symbol::DEV_TMP5;
                    let layout = self
                        .layout_interner
                        .insert_direct_no_semantic(LayoutRepr::Struct(other_fields));

                    self.load_literal_symbols(fields);
                    self.storage_manager.create_struct(
                        self.layout_interner,
                        &mut self.buf,
                        &temp_sym,
                        &layout,
                        fields,
                    );

                    // now effectively box this struct
                    let untagged_pointer_symbol = self.debug_symbol("untagged_pointer");
                    self.expr_box(untagged_pointer_symbol, temp_sym, layout, reuse);

                    self.free_symbol(&temp_sym);

                    let tag_id_symbol = self.debug_symbol("tag_id");

                    // index zero is taken up by the nullable tag, so any tags before it in the
                    // ordering need to be incremented by one
                    let pointer_tag = if tag_id < nullable_id {
                        tag_id + 1
                    } else {
                        tag_id
                    };

                    // finally, we need to tag the pointer
                    debug_assert!(tag_id < 8);
                    self.load_literal_i64(&tag_id_symbol, pointer_tag as _);

                    self.build_int_bitwise_or(
                        sym,
                        &untagged_pointer_symbol,
                        &tag_id_symbol,
                        IntWidth::U64,
                    );

                    self.free_symbol(&untagged_pointer_symbol);
                    self.free_symbol(&tag_id_symbol);
                }
            }
            UnionLayout::Recursive(tags) => {
                self.load_literal_symbols(fields);

                let whole_struct_symbol = self.debug_symbol("whole_struct_symbol");
                let tag_id_symbol = self.debug_symbol("tag_id_symbol");
                let other_fields = tags[tag_id as usize];

                let stores_tag_id_as_data =
                    union_layout.stores_tag_id_as_data(self.storage_manager.target_info);

                // construct the payload as a struct on the stack
                let data_struct_layout = self
                    .layout_interner
                    .insert_direct_no_semantic(LayoutRepr::Struct(other_fields));

                let tag_id_layout = union_layout.tag_id_layout();

                if stores_tag_id_as_data {
                    let inner_struct_symbol = self.debug_symbol("inner_struct_symbol");

                    self.storage_manager.create_struct(
                        self.layout_interner,
                        &mut self.buf,
                        &inner_struct_symbol,
                        &data_struct_layout,
                        fields,
                    );

                    self.load_literal_i64(&tag_id_symbol, tag_id as _);

                    let arena = self.env.arena;
                    let whole_struct_layout =
                        self.layout_interner
                            .insert_direct_no_semantic(LayoutRepr::Struct(
                                arena.alloc([data_struct_layout, tag_id_layout]),
                            ));

                    self.storage_manager.create_struct(
                        self.layout_interner,
                        &mut self.buf,
                        &whole_struct_symbol,
                        &whole_struct_layout,
                        arena.alloc([inner_struct_symbol, tag_id_symbol]),
                    );

                    self.expr_box(*sym, whole_struct_symbol, whole_struct_layout, reuse);

                    self.free_symbol(&tag_id_symbol);
                    self.free_symbol(&whole_struct_symbol);
                    self.free_symbol(&inner_struct_symbol);
                } else {
                    self.load_literal_symbols(fields);
                    self.storage_manager.create_struct(
                        self.layout_interner,
                        &mut self.buf,
                        &whole_struct_symbol,
                        &data_struct_layout,
                        fields,
                    );

                    // now effectively box this struct
                    let untagged_pointer_symbol = self.debug_symbol("untagged_pointer");
                    self.expr_box(
                        untagged_pointer_symbol,
                        whole_struct_symbol,
                        data_struct_layout,
                        reuse,
                    );

                    self.free_symbol(&whole_struct_symbol);

                    // finally, we need to tag the pointer
                    debug_assert!(tag_id < 8);
                    self.load_literal_i64(&tag_id_symbol, tag_id as _);

                    self.build_int_bitwise_or(
                        sym,
                        &untagged_pointer_symbol,
                        &tag_id_symbol,
                        IntWidth::U64,
                    );

                    self.free_symbol(&untagged_pointer_symbol);
                    self.free_symbol(&tag_id_symbol);
                }
            }
        }
    }

    fn load_literal(&mut self, sym: &Symbol, layout: &InLayout<'a>, lit: &Literal<'a>) {
        let layout = self.layout_interner.get_repr(*layout);

        if let LayoutRepr::LambdaSet(lambda_set) = layout {
            return self.load_literal(sym, &lambda_set.runtime_representation(), lit);
        }

        match (lit, layout) {
            (
                Literal::Int(x),
                LayoutRepr::Builtin(Builtin::Int(
                    IntWidth::U8
                    | IntWidth::U16
                    | IntWidth::U32
                    | IntWidth::U64
                    | IntWidth::I8
                    | IntWidth::I16
                    | IntWidth::I32
                    | IntWidth::I64,
                )),
            ) => {
                let reg = self.storage_manager.claim_general_reg(&mut self.buf, sym);
                let val = *x;
                ASM::mov_reg64_imm64(&mut self.buf, reg, i128::from_ne_bytes(val) as i64);
            }
            (
                Literal::Int(bytes) | Literal::U128(bytes),
                LayoutRepr::Builtin(Builtin::Int(IntWidth::I128 | IntWidth::U128)),
            ) => {
                self.storage_manager.with_tmp_general_reg(
                    &mut self.buf,
                    |storage_manager, buf, reg| {
                        let base_offset = storage_manager.claim_stack_area(sym, 16);

                        let mut num_bytes = [0; 8];
                        num_bytes.copy_from_slice(&bytes[..8]);
                        let num = i64::from_ne_bytes(num_bytes);
                        ASM::mov_reg64_imm64(buf, reg, num);
                        ASM::mov_base32_reg64(buf, base_offset, reg);

                        num_bytes.copy_from_slice(&bytes[8..16]);
                        let num = i64::from_ne_bytes(num_bytes);
                        ASM::mov_reg64_imm64(buf, reg, num);
                        ASM::mov_base32_reg64(buf, base_offset + 8, reg);
                    },
                );
            }
            (Literal::Byte(x), LayoutRepr::Builtin(Builtin::Int(IntWidth::U8 | IntWidth::I8))) => {
                let reg = self.storage_manager.claim_general_reg(&mut self.buf, sym);
                let val = *x;
                ASM::mov_reg64_imm64(&mut self.buf, reg, val as i64);
            }
            (Literal::Bool(x), LayoutRepr::Builtin(Builtin::Bool)) => {
                let reg = self.storage_manager.claim_general_reg(&mut self.buf, sym);
                ASM::mov_reg64_imm64(&mut self.buf, reg, *x as i64);
            }
            (Literal::Float(x), LayoutRepr::Builtin(Builtin::Float(FloatWidth::F64))) => {
                let reg = self.storage_manager.claim_float_reg(&mut self.buf, sym);
                let val = *x;
                ASM::mov_freg64_imm64(&mut self.buf, &mut self.relocs, reg, val);
            }
            (Literal::Float(x), LayoutRepr::Builtin(Builtin::Float(FloatWidth::F32))) => {
                let reg = self.storage_manager.claim_float_reg(&mut self.buf, sym);
                let val = *x as f32;
                ASM::mov_freg32_imm32(&mut self.buf, &mut self.relocs, reg, val);
            }
            (Literal::Decimal(bytes), LayoutRepr::Builtin(Builtin::Decimal)) => {
                self.storage_manager.with_tmp_general_reg(
                    &mut self.buf,
                    |storage_manager, buf, reg| {
                        let base_offset = storage_manager.claim_stack_area(sym, 16);

                        let mut num_bytes = [0; 8];
                        num_bytes.copy_from_slice(&bytes[..8]);
                        let num = i64::from_ne_bytes(num_bytes);
                        ASM::mov_reg64_imm64(buf, reg, num);
                        ASM::mov_base32_reg64(buf, base_offset, reg);

                        num_bytes.copy_from_slice(&bytes[8..16]);
                        let num = i64::from_ne_bytes(num_bytes);
                        ASM::mov_reg64_imm64(buf, reg, num);
                        ASM::mov_base32_reg64(buf, base_offset + 8, reg);
                    },
                );
            }
            (Literal::Str(x), LayoutRepr::Builtin(Builtin::Str)) => {
                if x.len() < 24 {
                    // Load small string.
                    self.storage_manager.with_tmp_general_reg(
                        &mut self.buf,
                        |storage_manager, buf, reg| {
                            let base_offset = storage_manager.claim_stack_area(sym, 24);
                            let mut bytes = [0; 24];
                            bytes[..x.len()].copy_from_slice(x.as_bytes());
                            bytes[23] = (x.len() as u8) | 0b1000_0000;

                            let mut num_bytes = [0; 8];
                            num_bytes.copy_from_slice(&bytes[..8]);
                            let num = i64::from_ne_bytes(num_bytes);
                            ASM::mov_reg64_imm64(buf, reg, num);
                            ASM::mov_base32_reg64(buf, base_offset, reg);

                            num_bytes.copy_from_slice(&bytes[8..16]);
                            let num = i64::from_ne_bytes(num_bytes);
                            ASM::mov_reg64_imm64(buf, reg, num);
                            ASM::mov_base32_reg64(buf, base_offset + 8, reg);

                            num_bytes.copy_from_slice(&bytes[16..]);
                            let num = i64::from_ne_bytes(num_bytes);
                            ASM::mov_reg64_imm64(buf, reg, num);
                            ASM::mov_base32_reg64(buf, base_offset + 16, reg);
                        },
                    );
                } else {
                    // load large string (pretend it's a `List U8`). We should move this data into
                    // the binary eventually because our RC algorithm won't free this value
                    let elements: Vec<_> = x
                        .as_bytes()
                        .iter()
                        .map(|b| ListLiteralElement::Literal(Literal::Byte(*b)))
                        .collect_in(self.storage_manager.env.arena);

                    self.create_array(sym, &Layout::U8, elements.into_bump_slice())
                }
            }
            _ => todo!("loading literal {:?} with layout {:?}", lit, layout),
        }
    }

    fn free_symbol(&mut self, sym: &Symbol) {
        self.join_map.remove(&JoinPointId(*sym));
        self.storage_manager.free_symbol(sym);
    }

    fn return_symbol(&mut self, sym: &Symbol, layout: &InLayout<'a>) {
        let repr = self.layout_interner.get_repr(*layout);
        if self.storage_manager.is_stored_primitive(sym) {
            // Just load it to the correct type of reg as a stand alone value.
            match repr {
                single_register_integers!() | pointer_layouts!() => {
                    self.storage_manager.load_to_specified_general_reg(
                        &mut self.buf,
                        sym,
                        CC::GENERAL_RETURN_REGS[0],
                    );
                }
                single_register_floats!() => {
                    self.storage_manager.load_to_specified_float_reg(
                        &mut self.buf,
                        sym,
                        CC::FLOAT_RETURN_REGS[0],
                    );
                }
                LayoutRepr::LambdaSet(lambda_set) => {
                    self.return_symbol(sym, &lambda_set.runtime_representation())
                }
                LayoutRepr::Union(UnionLayout::NonRecursive(_))
                | LayoutRepr::Builtin(_)
                | LayoutRepr::Struct(_) => {
                    internal_error!("All primitive values should fit in a single register");
                }
            }
        } else {
            CC::return_complex_symbol(
                &mut self.buf,
                &mut self.storage_manager,
                self.layout_interner,
                sym,
                layout,
            )
        }
        let inst_loc = self.buf.len() as u64;
        let offset = ASM::jmp_imm32(&mut self.buf, 0x1234_5678) as u64;
        self.relocs.push(Relocation::JmpToReturn {
            inst_loc,
            inst_size: self.buf.len() as u64 - inst_loc,
            offset,
        });
    }

    fn build_int_bitwise_and(
        &mut self,
        dst: &Symbol,
        src1: &Symbol,
        src2: &Symbol,
        int_width: IntWidth,
    ) {
        let buf = &mut self.buf;

        match int_width {
            IntWidth::U128 | IntWidth::I128 => todo!(),
            _ => {
                let dst_reg = self.storage_manager.claim_general_reg(buf, dst);
                let src1_reg = self.storage_manager.load_to_general_reg(buf, src1);
                let src2_reg = self.storage_manager.load_to_general_reg(buf, src2);
                ASM::and_reg64_reg64_reg64(buf, dst_reg, src1_reg, src2_reg);
            }
        }
    }

    fn build_int_bitwise_or(
        &mut self,
        dst: &Symbol,
        src1: &Symbol,
        src2: &Symbol,
        int_width: IntWidth,
    ) {
        let buf = &mut self.buf;

        match int_width {
            IntWidth::U128 | IntWidth::I128 => todo!(),
            _ => {
                let dst_reg = self.storage_manager.claim_general_reg(buf, dst);
                let src1_reg = self.storage_manager.load_to_general_reg(buf, src1);
                let src2_reg = self.storage_manager.load_to_general_reg(buf, src2);
                ASM::or_reg64_reg64_reg64(buf, dst_reg, src1_reg, src2_reg);
            }
        }
    }

    fn build_int_bitwise_xor(
        &mut self,
        dst: &Symbol,
        src1: &Symbol,
        src2: &Symbol,
        int_width: IntWidth,
    ) {
        let buf = &mut self.buf;

        match int_width {
            IntWidth::U128 | IntWidth::I128 => todo!(),
            _ => {
                let dst_reg = self.storage_manager.claim_general_reg(buf, dst);
                let src1_reg = self.storage_manager.load_to_general_reg(buf, src1);
                let src2_reg = self.storage_manager.load_to_general_reg(buf, src2);
                ASM::xor_reg64_reg64_reg64(buf, dst_reg, src1_reg, src2_reg);
            }
        }
    }

    fn build_int_shift_left(
        &mut self,
        dst: &Symbol,
        src1: &Symbol,
        src2: &Symbol,
        int_width: IntWidth,
    ) {
        let buf = &mut self.buf;

        match int_width {
            IntWidth::U128 | IntWidth::I128 => todo!(),
            _ => {
                let dst_reg = self.storage_manager.claim_general_reg(buf, dst);
                let src1_reg = self.storage_manager.load_to_general_reg(buf, src1);
                let src2_reg = self.storage_manager.load_to_general_reg(buf, src2);

                ASM::shl_reg64_reg64_reg64(
                    buf,
                    &mut self.storage_manager,
                    dst_reg,
                    src1_reg,
                    src2_reg,
                );
            }
        }
    }

    fn build_int_shift_right(
        &mut self,
        dst: &Symbol,
        src1: &Symbol,
        src2: &Symbol,
        int_width: IntWidth,
    ) {
        let buf = &mut self.buf;

        match int_width {
            IntWidth::U128 | IntWidth::I128 => todo!(),
            _ => {
                let dst_reg = self.storage_manager.claim_general_reg(buf, dst);
                let src1_reg = self.storage_manager.load_to_general_reg(buf, src1);
                let src2_reg = self.storage_manager.load_to_general_reg(buf, src2);

                // to get sign extension "for free", we move our bits to the left
                // so the integers sign bit is stored in the register's sign bit.
                // Then we arithmetic shift right, getting the correct sign extension behavior,
                // then shift logical right to get the bits back into the position they should
                // be for our particular integer width
                let sign_extend_shift_amount = 64 - (int_width.stack_size() as i64 * 8);

                if sign_extend_shift_amount > 0 {
                    self.storage_manager.with_tmp_general_reg(
                        buf,
                        |storage_manager, buf, tmp_reg| {
                            ASM::mov_reg64_imm64(buf, tmp_reg, sign_extend_shift_amount);
                            ASM::shl_reg64_reg64_reg64(
                                buf,
                                storage_manager,
                                src1_reg,
                                src1_reg,
                                tmp_reg,
                            );
                        },
                    )
                }

                ASM::sar_reg64_reg64_reg64(
                    buf,
                    &mut self.storage_manager,
                    dst_reg,
                    src1_reg,
                    src2_reg,
                );

                if sign_extend_shift_amount > 0 {
                    // shift back if needed
                    self.storage_manager.with_tmp_general_reg(
                        &mut self.buf,
                        |storage_manager, buf, tmp_reg| {
                            ASM::mov_reg64_imm64(buf, tmp_reg, sign_extend_shift_amount);
                            ASM::shr_reg64_reg64_reg64(
                                buf,
                                storage_manager,
                                dst_reg,
                                dst_reg,
                                tmp_reg,
                            );
                        },
                    )
                }
            }
        }
    }

    fn build_int_shift_right_zero_fill(
        &mut self,
        dst: &Symbol,
        src1: &Symbol,
        src2: &Symbol,
        int_width: IntWidth,
    ) {
        let buf = &mut self.buf;

        match int_width {
            IntWidth::U128 | IntWidth::I128 => {
                let layout = match int_width {
                    IntWidth::I128 => Layout::I128,
                    IntWidth::U128 => Layout::U128,
                    _ => unreachable!(),
                };

                self.build_fn_call(
                    dst,
                    bitcode::NUM_SHIFT_RIGHT_ZERO_FILL[int_width].to_string(),
                    &[*src1, *src2],
                    &[layout, layout],
                    &layout,
                );
            }
            _ => {
                let dst_reg = self.storage_manager.claim_general_reg(buf, dst);
                let src1_reg = self.storage_manager.load_to_general_reg(buf, src1);
                let src2_reg = self.storage_manager.load_to_general_reg(buf, src2);

                ASM::shr_reg64_reg64_reg64(
                    buf,
                    &mut self.storage_manager,
                    dst_reg,
                    src1_reg,
                    src2_reg,
                );
            }
        }
    }

    fn build_num_sqrt(&mut self, dst: Symbol, src: Symbol, float_width: FloatWidth) {
        let buf = &mut self.buf;

        let dst_reg = self.storage_manager.claim_float_reg(buf, &dst);
        let src_reg = self.storage_manager.load_to_float_reg(buf, &src);

        match float_width {
            FloatWidth::F32 => ASM::sqrt_freg32_freg32(buf, dst_reg, src_reg),
            FloatWidth::F64 => ASM::sqrt_freg64_freg64(buf, dst_reg, src_reg),
        }
    }

    fn build_num_int_cast(
        &mut self,
        dst: &Symbol,
        src: &Symbol,
        source: IntWidth,
        target: IntWidth,
    ) {
        use IntWidth::*;

        let buf = &mut self.buf;

        match (source, target) {
            (U128, U64) => {
                let dst_reg = self.storage_manager.claim_general_reg(buf, dst);

                let (offset, _size) = self.storage_manager.stack_offset_and_size(src);

                ASM::mov_reg64_base32(buf, dst_reg, offset + 8);

                return;
            }
            (U64, U128) => {
                let src_reg = self.storage_manager.load_to_general_reg(buf, src);

                let base_offset = self.storage_manager.claim_stack_area(dst, 16);

                let tmp = Symbol::DEV_TMP;
                let tmp_reg = self.storage_manager.claim_general_reg(buf, &tmp);

                // move a zero into the lower 8 bytes
                ASM::mov_reg64_imm64(buf, tmp_reg, 0x0);
                ASM::mov_base32_reg64(buf, base_offset, tmp_reg);

                ASM::mov_base32_reg64(buf, base_offset + 8, src_reg);

                self.free_symbol(&tmp);

                return;
            }

            _ => {}
        }

        let dst_reg = self.storage_manager.claim_general_reg(buf, dst);
        let src_reg = self.storage_manager.load_to_general_reg(buf, src);

        if source.stack_size() == target.stack_size() {
            match source.stack_size() {
                8 => ASM::mov_reg64_reg64(buf, dst_reg, src_reg),
                4 => ASM::mov_reg32_reg32(buf, dst_reg, src_reg),
                2 => ASM::mov_reg16_reg16(buf, dst_reg, src_reg),
                1 => ASM::mov_reg8_reg8(buf, dst_reg, src_reg),
                _ => todo!("int cast from {source:?} to {target:?}"),
            }
        } else {
            match (source, target) {
                // -- CASTING UP --
                (I8 | U8, U16 | U32 | U64) => {
                    // zero  out the register
                    ASM::xor_reg64_reg64_reg64(buf, dst_reg, dst_reg, dst_reg);

                    // move the 8-bit integer
                    ASM::mov_reg_reg(buf, RegisterWidth::W8, dst_reg, src_reg);
                }
                (U16, U32 | U64) => {
                    // zero  out the register
                    ASM::xor_reg64_reg64_reg64(buf, dst_reg, dst_reg, dst_reg);

                    // move the 16-bit integer
                    ASM::mov_reg_reg(buf, RegisterWidth::W16, dst_reg, src_reg);
                }
                (U32, U64) => {
                    // zero  out the register
                    ASM::xor_reg64_reg64_reg64(buf, dst_reg, dst_reg, dst_reg);

                    // move the 32-bit integer
                    ASM::mov_reg_reg(buf, RegisterWidth::W32, dst_reg, src_reg);
                }
                (I8, I16 | I32 | I64) => {
                    ASM::movsx_reg_reg(buf, RegisterWidth::W8, dst_reg, src_reg)
                }
                (I16, I32 | I64) => ASM::movsx_reg_reg(buf, RegisterWidth::W16, dst_reg, src_reg),
                (I32, I64) => ASM::movsx_reg_reg(buf, RegisterWidth::W32, dst_reg, src_reg),
                // -- CASTING DOWN --
                (U64 | I64, I32 | U32) => {
                    // move as a 32-bit integer (leaving any other bits behind)
                    ASM::mov_reg_reg(buf, RegisterWidth::W32, dst_reg, src_reg);
                }
                (U64 | I64 | U32 | I32, I16 | U16) => {
                    // move as a 16-bit integer (leaving any other bits behind)
                    ASM::mov_reg_reg(buf, RegisterWidth::W16, dst_reg, src_reg);
                }
                (U64 | I64 | U32 | I32 | U16 | I16, I8 | U8) => {
                    // move as an 8-bit integer (leaving any other bits behind)
                    ASM::mov_reg_reg(buf, RegisterWidth::W8, dst_reg, src_reg);
                }
                _ => todo!("int cast from {source:?} to {target:?}"),
            }
        }
    }
}

/// This impl block is for ir related instructions that need backend specific information.
/// For example, loading a symbol for doing a computation.
impl<
        'a,
        'r,
        FloatReg: RegTrait,
        GeneralReg: RegTrait,
        ASM: Assembler<GeneralReg, FloatReg>,
        CC: CallConv<GeneralReg, FloatReg, ASM>,
    > Backend64Bit<'a, 'r, GeneralReg, FloatReg, ASM, CC>
{
    fn clear_tag_id(&mut self, ptr_reg: GeneralReg) -> (Symbol, GeneralReg) {
        let unmasked_symbol = self.debug_symbol("unmasked");
        let unmasked_reg = self
            .storage_manager
            .claim_general_reg(&mut self.buf, &unmasked_symbol);

        ASM::mov_reg64_imm64(&mut self.buf, unmasked_reg, (!0b111) as _);

        ASM::and_reg64_reg64_reg64(&mut self.buf, unmasked_reg, ptr_reg, unmasked_reg);

        (unmasked_symbol, unmasked_reg)
    }

    fn compare(
        &mut self,
        op: CompareOperation,
        dst: &Symbol,
        src1: &Symbol,
        src2: &Symbol,
        arg_layout: &InLayout<'a>,
    ) {
        match self.interner().get_repr(*arg_layout) {
            single_register_integers!() => {
                let buf = &mut self.buf;

                let dst = self.storage_manager.claim_general_reg(buf, dst);
                let src1 = self.storage_manager.load_to_general_reg(buf, src1);
                let src2 = self.storage_manager.load_to_general_reg(buf, src2);

                let int_width = arg_layout.try_int_width().unwrap();
                let register_width = match int_width.stack_size() {
                    8 => RegisterWidth::W64,
                    4 => RegisterWidth::W32,
                    2 => RegisterWidth::W16,
                    1 => RegisterWidth::W8,
                    _ => unreachable!(),
                };

                if int_width.is_signed() {
                    ASM::signed_compare_reg64(buf, register_width, op, dst, src1, src2)
                } else {
                    ASM::unsigned_compare_reg64(buf, register_width, op, dst, src1, src2)
                }
            }
            LayoutRepr::F32 | LayoutRepr::F64 => {
                let float_width = match *arg_layout {
                    Layout::F32 => FloatWidth::F32,
                    Layout::F64 => FloatWidth::F64,
                    _ => unreachable!(),
                };

                let dst_reg = self.storage_manager.claim_general_reg(&mut self.buf, dst);
                let src1_reg = self.storage_manager.load_to_float_reg(&mut self.buf, src1);
                let src2_reg = self.storage_manager.load_to_float_reg(&mut self.buf, src2);

                ASM::cmp_freg_freg_reg64(
                    &mut self.buf,
                    dst_reg,
                    src1_reg,
                    src2_reg,
                    float_width,
                    op,
                );
            }
            x => todo!("NumLt: layout, {:?}", x),
        }
    }

    fn allocate_with_refcount(
        &mut self,
        dst: Symbol,
        data_bytes: Symbol,
        element_alignment: Symbol,
    ) {
        self.build_fn_call(
            &dst,
            bitcode::UTILS_ALLOCATE_WITH_REFCOUNT.to_string(),
            &[data_bytes, element_alignment],
            &[Layout::U64, Layout::U32],
            &Layout::U64,
        );
    }

    fn allocate_with_refcount_if_null(&mut self, dst: Symbol, src: Symbol, layout: InLayout) {
        let src_reg = self
            .storage_manager
            .load_to_general_reg(&mut self.buf, &src);

        // dummy mov that is supposed to move src into dst, and is filled in later because don't know yet which
        // register the other branch will pick for dst. We must pass two different registers here
        // otherwise the whole instruction is skipped!
        let dst_reg = self.storage_manager.claim_general_reg(&mut self.buf, &dst);
        let mov_start_index = self.buf.len();
        ASM::mov_reg64_reg64(&mut self.buf, dst_reg, src_reg);

        // jump to where the pointer is valid, because it is already valid if non-zero
        let jmp_start_index = self.buf.len();
        let jmp_end_index =
            ASM::jne_reg64_imm64_imm32(&mut self.buf, &mut self.storage_manager, src_reg, 0x0, 0);

        self.free_symbol(&dst);

        // so, the pointer is NULL, allocate

        let data_bytes = self.debug_symbol("data_bytes");
        self.load_layout_stack_size(layout, data_bytes);

        let element_alignment = self.debug_symbol("element_alignment");
        self.load_layout_alignment(layout, element_alignment);

        self.allocate_with_refcount(dst, data_bytes, element_alignment);

        self.free_symbol(&data_bytes);
        self.free_symbol(&element_alignment);

        let mut tmp = bumpalo::vec![in self.env.arena];

        // update the jump
        let destination_index = self.buf.len();
        ASM::jne_reg64_imm64_imm32(
            &mut tmp,
            &mut self.storage_manager,
            src_reg,
            0x0,
            (destination_index - jmp_end_index) as i32,
        );

        self.buf[jmp_start_index..][..tmp.len()].copy_from_slice(tmp.as_slice());

        // figure out what register was actually used
        let dst_reg = self
            .storage_manager
            .load_to_general_reg(&mut self.buf, &dst);

        tmp.clear();
        ASM::mov_reg64_reg64(&mut tmp, dst_reg, src_reg);

        self.buf[mov_start_index..][..tmp.len()].copy_from_slice(tmp.as_slice());
    }

    fn unbox_str_or_list(
        buf: &mut Vec<'a, u8>,
        storage_manager: &mut StorageManager<'a, 'r, GeneralReg, FloatReg, ASM, CC>,
        dst: Symbol,
        ptr_reg: GeneralReg,
        tmp_reg: GeneralReg,
        offset: i32,
    ) {
        let base_offset = storage_manager.claim_stack_area(&dst, 24);

        ASM::mov_reg64_mem64_offset32(buf, tmp_reg, ptr_reg, offset);
        ASM::mov_base32_reg64(buf, base_offset, tmp_reg);

        ASM::mov_reg64_mem64_offset32(buf, tmp_reg, ptr_reg, offset + 8);
        ASM::mov_base32_reg64(buf, base_offset + 8, tmp_reg);

        ASM::mov_reg64_mem64_offset32(buf, tmp_reg, ptr_reg, offset + 16);
        ASM::mov_base32_reg64(buf, base_offset + 16, tmp_reg);
    }

    fn unbox_to_stack(
        buf: &mut Vec<'a, u8>,
        storage_manager: &mut StorageManager<'a, 'r, GeneralReg, FloatReg, ASM, CC>,
        dst: Symbol,
        stack_size: u32,
        ptr_reg: GeneralReg,
        tmp_reg: GeneralReg,
        read_offset: i32,
    ) {
        let mut copied = 0;
        let size = stack_size as i32;

        if size == 0 {
            storage_manager.no_data(&dst);
            return;
        }

        let base_offset = storage_manager.claim_stack_area(&dst, stack_size);

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

    fn ptr_read(
        buf: &mut Vec<'a, u8>,
        storage_manager: &mut StorageManager<'a, 'r, GeneralReg, FloatReg, ASM, CC>,
        layout_interner: &STLayoutInterner<'a>,
        ptr_reg: GeneralReg,
        offset: i32,
        element_in_layout: InLayout<'a>,
        dst: Symbol,
    ) {
        match layout_interner.get_repr(element_in_layout) {
            LayoutRepr::Builtin(builtin) => match builtin {
                Builtin::Int(int_width) => match int_width {
                    IntWidth::I128 | IntWidth::U128 => {
                        // can we treat this as 2 u64's?
                        todo!()
                    }
                    IntWidth::I64 | IntWidth::U64 => {
                        let dst_reg = storage_manager.claim_general_reg(buf, &dst);
                        ASM::mov_reg64_mem64_offset32(buf, dst_reg, ptr_reg, offset);
                    }
                    IntWidth::I32 | IntWidth::U32 => {
                        let dst_reg = storage_manager.claim_general_reg(buf, &dst);
                        ASM::mov_reg32_mem32_offset32(buf, dst_reg, ptr_reg, offset);
                    }
                    IntWidth::I16 | IntWidth::U16 => {
                        let dst_reg = storage_manager.claim_general_reg(buf, &dst);
                        ASM::xor_reg64_reg64_reg64(buf, dst_reg, dst_reg, dst_reg);
                        ASM::mov_reg16_mem16_offset32(buf, dst_reg, ptr_reg, offset);
                    }
                    IntWidth::I8 | IntWidth::U8 => {
                        let dst_reg = storage_manager.claim_general_reg(buf, &dst);
                        ASM::xor_reg64_reg64_reg64(buf, dst_reg, dst_reg, dst_reg);
                        ASM::mov_reg8_mem8_offset32(buf, dst_reg, ptr_reg, offset);
                    }
                },
                Builtin::Float(FloatWidth::F64) => {
                    let dst_reg = storage_manager.claim_float_reg(buf, &dst);
                    ASM::mov_freg64_mem64_offset32(buf, dst_reg, ptr_reg, offset);
                }
                Builtin::Float(FloatWidth::F32) => {
                    let dst_reg = storage_manager.claim_float_reg(buf, &dst);
                    ASM::mov_freg32_mem32_offset32(buf, dst_reg, ptr_reg, offset);
                }
                Builtin::Bool => {
                    // the same as an 8-bit integer
                    let dst_reg = storage_manager.claim_general_reg(buf, &dst);

                    ASM::xor_reg64_reg64_reg64(buf, dst_reg, dst_reg, dst_reg);
                    ASM::mov_reg8_mem8_offset32(buf, dst_reg, ptr_reg, offset);
                }
                Builtin::Decimal => {
                    // same as 128-bit integer
                }
                Builtin::Str | Builtin::List(_) => {
                    storage_manager.with_tmp_general_reg(buf, |storage_manager, buf, tmp_reg| {
                        Self::unbox_str_or_list(
                            buf,
                            storage_manager,
                            dst,
                            ptr_reg,
                            tmp_reg,
                            offset,
                        );
                    });
                }
            },

            pointer_layouts!() => {
                // the same as 64-bit integer (for 64-bit targets)
                let dst_reg = storage_manager.claim_general_reg(buf, &dst);
                ASM::mov_reg64_mem64_offset32(buf, dst_reg, ptr_reg, offset);
            }

            LayoutRepr::Struct { .. } => {
                // put it on the stack
                let stack_size = layout_interner.stack_size(element_in_layout);

                storage_manager.with_tmp_general_reg(buf, |storage_manager, buf, tmp_reg| {
                    Self::unbox_to_stack(
                        buf,
                        storage_manager,
                        dst,
                        stack_size,
                        ptr_reg,
                        tmp_reg,
                        offset,
                    );
                });
            }

            LayoutRepr::Union(UnionLayout::NonRecursive(_)) => {
                // put it on the stack
                let stack_size = layout_interner.stack_size(element_in_layout);

                storage_manager.with_tmp_general_reg(buf, |storage_manager, buf, tmp_reg| {
                    Self::unbox_to_stack(
                        buf,
                        storage_manager,
                        dst,
                        stack_size,
                        ptr_reg,
                        tmp_reg,
                        offset,
                    );
                });
            }

            LayoutRepr::LambdaSet(lambda_set) => {
                Self::ptr_read(
                    buf,
                    storage_manager,
                    layout_interner,
                    ptr_reg,
                    offset,
                    lambda_set.runtime_representation(),
                    dst,
                );
            }
        }
    }

    #[allow(clippy::too_many_arguments)]
    fn ptr_write(
        buf: &mut Vec<'a, u8>,
        storage_manager: &mut StorageManager<'a, 'r, GeneralReg, FloatReg, ASM, CC>,
        layout_interner: &STLayoutInterner<'a>,
        ptr_reg: GeneralReg,
        element_offset: i32,
        element_width: u64,
        element_layout: LayoutRepr<'a>,
        value: Symbol,
    ) {
        match element_layout {
            LayoutRepr::Builtin(Builtin::Int(IntWidth::I64 | IntWidth::U64)) => {
                let sym_reg = storage_manager.load_to_general_reg(buf, &value);
                ASM::mov_mem64_offset32_reg64(buf, ptr_reg, element_offset, sym_reg);
            }
            LayoutRepr::Builtin(Builtin::Int(IntWidth::I32 | IntWidth::U32)) => {
                let sym_reg = storage_manager.load_to_general_reg(buf, &value);
                ASM::mov_mem32_offset32_reg32(buf, ptr_reg, element_offset, sym_reg);
            }
            LayoutRepr::Builtin(Builtin::Int(IntWidth::I16 | IntWidth::U16)) => {
                let sym_reg = storage_manager.load_to_general_reg(buf, &value);
                ASM::mov_mem16_offset32_reg16(buf, ptr_reg, element_offset, sym_reg);
            }
            LayoutRepr::Builtin(Builtin::Int(IntWidth::I8 | IntWidth::U8) | Builtin::Bool) => {
                let sym_reg = storage_manager.load_to_general_reg(buf, &value);
                ASM::mov_mem8_offset32_reg8(buf, ptr_reg, element_offset, sym_reg);
            }
            LayoutRepr::Builtin(Builtin::Float(FloatWidth::F64 | FloatWidth::F32)) => {
                let sym_reg = storage_manager.load_to_float_reg(buf, &value);
                ASM::movesd_mem64_offset32_freg64(buf, ptr_reg, element_offset, sym_reg);
            }
            pointer_layouts!() => {
                let sym_reg = storage_manager.load_to_general_reg(buf, &value);
                ASM::mov_mem64_offset32_reg64(buf, ptr_reg, element_offset, sym_reg);
            }
            LayoutRepr::LambdaSet(lambda_set) => {
                let repr = layout_interner.get_repr(lambda_set.runtime_representation());

                Self::ptr_write(
                    buf,
                    storage_manager,
                    layout_interner,
                    ptr_reg,
                    element_offset,
                    element_width,
                    repr,
                    value,
                );
            }

            _other => {
                if element_width == 0 {
                    return;
                }

                let (from_offset, stack_size) = storage_manager.stack_offset_and_size(&value);
                debug_assert!(from_offset % 8 == 0);

                storage_manager.with_tmp_general_reg(buf, |_storage_manager, buf, tmp_reg| {
                    let mut copied = 0;
                    let size = stack_size as i32;

                    if size - copied >= 8 {
                        for _ in (0..(size - copied)).step_by(8) {
                            ASM::mov_reg64_base32(buf, tmp_reg, from_offset + copied);
                            ASM::mov_mem64_offset32_reg64(
                                buf,
                                ptr_reg,
                                element_offset + copied,
                                tmp_reg,
                            );

                            copied += 8;
                        }
                    }

                    if size - copied >= 4 {
                        for _ in (0..(size - copied)).step_by(4) {
                            ASM::mov_reg32_base32(buf, tmp_reg, from_offset + copied);
                            ASM::mov_mem32_offset32_reg32(
                                buf,
                                ptr_reg,
                                element_offset + copied,
                                tmp_reg,
                            );

                            copied += 4;
                        }
                    }

                    if size - copied >= 2 {
                        for _ in (0..(size - copied)).step_by(2) {
                            ASM::mov_reg16_base32(buf, tmp_reg, from_offset + copied);
                            ASM::mov_mem16_offset32_reg16(
                                buf,
                                ptr_reg,
                                element_offset + copied,
                                tmp_reg,
                            );

                            copied += 2;
                        }
                    }

                    if size - copied >= 1 {
                        for _ in (0..(size - copied)).step_by(1) {
                            ASM::mov_reg8_base32(buf, tmp_reg, from_offset + copied);
                            ASM::mov_mem8_offset32_reg8(
                                buf,
                                ptr_reg,
                                element_offset + copied,
                                tmp_reg,
                            );

                            copied += 1;
                        }
                    }
                });
            }
        }
    }

    /// Updates a jump instruction to a new offset and returns the number of bytes written.
    fn update_jmp_imm32_offset(
        &mut self,
        tmp: &mut Vec<'a, u8>,
        jmp_location: u64,
        base_offset: u64,
        target_offset: u64,
    ) {
        tmp.clear();
        let jmp_offset = target_offset as i32 - base_offset as i32;
        ASM::jmp_imm32(tmp, jmp_offset);
        for (i, byte) in tmp.iter().enumerate() {
            self.buf[jmp_location as usize + i] = *byte;
        }
    }

    /// Loads the alignment bytes of `layout` into the given `symbol`
    fn load_layout_alignment(&mut self, layout: InLayout<'_>, symbol: Symbol) {
        let u32_layout = Layout::U32;
        let alignment = self.layout_interner.alignment_bytes(layout);
        let alignment_literal = Literal::Int((alignment as i128).to_ne_bytes());

        self.load_literal(&symbol, &u32_layout, &alignment_literal);
    }

    /// Loads the stack size of `layout` into the given `symbol`
    fn load_layout_stack_size(&mut self, layout: InLayout<'_>, symbol: Symbol) {
        let u64_layout = Layout::U64;
        let width = self.layout_interner.stack_size(layout);
        let width_literal = Literal::Int((width as i128).to_ne_bytes());

        self.load_literal(&symbol, &u64_layout, &width_literal);
    }
}

#[macro_export]
macro_rules! sign_extended_int_builtins {
    () => {
        Layout::I8 | Layout::I16 | Layout::I32 | Layout::I64 | Layout::I128
    };
}

#[macro_export]
macro_rules! zero_extended_int_builtins {
    () => {
        Layout::U8 | Layout::U16 | Layout::U32 | Layout::U64 | Layout::U128
    };
}

#[macro_export]
macro_rules! single_register_int_builtins {
    () => {
        LayoutRepr::I8
            | LayoutRepr::I16
            | LayoutRepr::I32
            | LayoutRepr::I64
            | LayoutRepr::U8
            | LayoutRepr::U16
            | LayoutRepr::U32
            | LayoutRepr::U64
    };
}

#[macro_export]
macro_rules! single_register_integers {
    () => {
        LayoutRepr::BOOL | single_register_int_builtins!() | LayoutRepr::OPAQUE_PTR
    };
}

#[macro_export]
macro_rules! single_register_floats {
    () => {
        LayoutRepr::F32 | LayoutRepr::F64
    };
}

#[macro_export]
macro_rules! single_register_layouts {
    () => {
        single_register_integers!() | single_register_floats!()
    };
}

#[macro_export]
macro_rules! pointer_layouts {
    () => {
        LayoutRepr::Boxed(_)
            | LayoutRepr::Ptr(_)
            | LayoutRepr::RecursivePointer(_)
            | LayoutRepr::Union(
                UnionLayout::Recursive(_)
                    | UnionLayout::NonNullableUnwrapped(_)
                    | UnionLayout::NullableWrapped { .. }
                    | UnionLayout::NullableUnwrapped { .. },
            )
    };
}
