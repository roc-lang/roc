use crate::{
    single_register_floats, single_register_int_builtins, single_register_integers, Backend, Env,
    Relocation,
};
use bumpalo::collections::Vec;
use roc_builtins::bitcode::{self, FloatWidth, IntWidth};
use roc_collections::all::MutMap;
use roc_error_macros::internal_error;
use roc_module::symbol::{Interns, Symbol};
use roc_mono::code_gen_help::CodeGenHelp;
use roc_mono::ir::{
    BranchInfo, JoinPointId, ListLiteralElement, Literal, Param, ProcLayout, SelfRecursive, Stmt,
};
use roc_mono::layout::{Builtin, Layout, TagIdIntType, UnionLayout};
use roc_target::TargetInfo;
use std::marker::PhantomData;

pub(crate) mod aarch64;
#[cfg(test)]
mod disassembler_test_macro;
pub(crate) mod storage;
pub(crate) mod x86_64;

use storage::{RegStorage, StorageManager};

const REFCOUNT_ONE: u64 = i64::MIN as u64;
// TODO: on all number functions double check and deal with over/underflow.

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

    fn setup_stack<'a>(
        buf: &mut Vec<'a, u8>,
        general_saved_regs: &[GeneralReg],
        float_saved_regs: &[FloatReg],
        requested_stack_size: i32,
        fn_call_stack_size: i32,
    ) -> i32;
    fn cleanup_stack<'a>(
        buf: &mut Vec<'a, u8>,
        general_saved_regs: &[GeneralReg],
        float_saved_regs: &[FloatReg],
        aligned_stack_size: i32,
        fn_call_stack_size: i32,
    );

    /// load_args updates the storage manager to know where every arg is stored.
    fn load_args<'a>(
        buf: &mut Vec<'a, u8>,
        storage_manager: &mut StorageManager<'a, GeneralReg, FloatReg, ASM, Self>,
        args: &'a [(Layout<'a>, Symbol)],
        // ret_layout is needed because if it is a complex type, we pass a pointer as the first arg.
        ret_layout: &Layout<'a>,
    );

    /// store_args stores the args in registers and on the stack for function calling.
    /// It also updates the amount of temporary stack space needed in the storage manager.
    fn store_args<'a>(
        buf: &mut Vec<'a, u8>,
        storage_manager: &mut StorageManager<'a, GeneralReg, FloatReg, ASM, Self>,
        dst: &Symbol,
        args: &[Symbol],
        arg_layouts: &[Layout<'a>],
        // ret_layout is needed because if it is a complex type, we pass a pointer as the first arg.
        ret_layout: &Layout<'a>,
    );

    /// return_complex_symbol returns the specified complex/non-primative symbol.
    /// It uses the layout to determine how the data should be returned.
    fn return_complex_symbol<'a>(
        buf: &mut Vec<'a, u8>,
        storage_manager: &mut StorageManager<'a, GeneralReg, FloatReg, ASM, Self>,
        sym: &Symbol,
        layout: &Layout<'a>,
    );

    /// load_returned_complex_symbol loads a complex symbol that was returned from a function call.
    /// It uses the layout to determine how the data should be loaded into the symbol.
    fn load_returned_complex_symbol<'a>(
        buf: &mut Vec<'a, u8>,
        storage_manager: &mut StorageManager<'a, GeneralReg, FloatReg, ASM, Self>,
        sym: &Symbol,
        layout: &Layout<'a>,
    );
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

    fn call(buf: &mut Vec<'_, u8>, relocs: &mut Vec<'_, Relocation>, fn_name: String);

    /// Jumps by an offset of offset bytes unconditionally.
    /// It should always generate the same number of bytes to enable replacement if offset changes.
    /// It returns the base offset to calculate the jump from (generally the instruction after the jump).
    fn jmp_imm32(buf: &mut Vec<'_, u8>, offset: i32) -> usize;

    fn tail_call(buf: &mut Vec<'_, u8>) -> u64;

    /// Jumps by an offset of offset bytes if reg is not equal to imm.
    /// It should always generate the same number of bytes to enable replacement if offset changes.
    /// It returns the base offset to calculate the jump from (generally the instruction after the jump).
    fn jne_reg64_imm64_imm32(
        buf: &mut Vec<'_, u8>,
        reg: GeneralReg,
        imm: u64,
        offset: i32,
    ) -> usize;

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
    fn mov_reg64_reg64(buf: &mut Vec<'_, u8>, dst: GeneralReg, src: GeneralReg);

    // base32 is similar to stack based instructions but they reference the base/frame pointer.
    fn mov_freg64_base32(buf: &mut Vec<'_, u8>, dst: FloatReg, offset: i32);
    fn mov_reg64_base32(buf: &mut Vec<'_, u8>, dst: GeneralReg, offset: i32);
    fn mov_base32_freg64(buf: &mut Vec<'_, u8>, offset: i32, src: FloatReg);
    fn mov_base32_reg64(buf: &mut Vec<'_, u8>, offset: i32, src: GeneralReg);

    fn mov_reg64_mem64_offset32(
        buf: &mut Vec<'_, u8>,
        dst: GeneralReg,
        src: GeneralReg,
        offset: i32,
    );
    fn mov_mem64_offset32_reg64(
        buf: &mut Vec<'_, u8>,
        dst: GeneralReg,
        offset: i32,
        src: GeneralReg,
    );

    /// Sign extends the data at `offset` with `size` as it copies it to `dst`
    /// size must be less than or equal to 8.
    fn movsx_reg64_base32(buf: &mut Vec<'_, u8>, dst: GeneralReg, offset: i32, size: u8);
    /// Zero extends the data at `offset` with `size` as it copies it to `dst`
    /// size must be less than or equal to 8.
    fn movzx_reg64_base32(buf: &mut Vec<'_, u8>, dst: GeneralReg, offset: i32, size: u8);

    fn mov_freg64_stack32(buf: &mut Vec<'_, u8>, dst: FloatReg, offset: i32);
    fn mov_reg64_stack32(buf: &mut Vec<'_, u8>, dst: GeneralReg, offset: i32);
    fn mov_stack32_freg64(buf: &mut Vec<'_, u8>, offset: i32, src: FloatReg);
    fn mov_stack32_reg64(buf: &mut Vec<'_, u8>, offset: i32, src: GeneralReg);

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
        storage_manager: &mut StorageManager<'a, GeneralReg, FloatReg, ASM, CC>,
        dst: GeneralReg,
        src1: GeneralReg,
        src2: GeneralReg,
    ) where
        ASM: Assembler<GeneralReg, FloatReg>,
        CC: CallConv<GeneralReg, FloatReg, ASM>;

    fn idiv_reg64_reg64_reg64<'a, ASM, CC>(
        buf: &mut Vec<'a, u8>,
        storage_manager: &mut StorageManager<'a, GeneralReg, FloatReg, ASM, CC>,
        dst: GeneralReg,
        src1: GeneralReg,
        src2: GeneralReg,
    ) where
        ASM: Assembler<GeneralReg, FloatReg>,
        CC: CallConv<GeneralReg, FloatReg, ASM>;
    fn udiv_reg64_reg64_reg64<'a, ASM, CC>(
        buf: &mut Vec<'a, u8>,
        storage_manager: &mut StorageManager<'a, GeneralReg, FloatReg, ASM, CC>,
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

    fn eq_reg64_reg64_reg64(
        buf: &mut Vec<'_, u8>,
        dst: GeneralReg,
        src1: GeneralReg,
        src2: GeneralReg,
    );

    fn neq_reg64_reg64_reg64(
        buf: &mut Vec<'_, u8>,
        dst: GeneralReg,
        src1: GeneralReg,
        src2: GeneralReg,
    );

    fn lt_reg64_reg64_reg64(
        buf: &mut Vec<'_, u8>,
        dst: GeneralReg,
        src1: GeneralReg,
        src2: GeneralReg,
    );

    fn to_float_freg32_reg64(buf: &mut Vec<'_, u8>, dst: FloatReg, src: GeneralReg);

    fn to_float_freg64_reg64(buf: &mut Vec<'_, u8>, dst: FloatReg, src: GeneralReg);

    fn to_float_freg32_freg64(buf: &mut Vec<'_, u8>, dst: FloatReg, src: FloatReg);

    fn to_float_freg64_freg32(buf: &mut Vec<'_, u8>, dst: FloatReg, src: FloatReg);

    fn lte_reg64_reg64_reg64(
        buf: &mut Vec<'_, u8>,
        dst: GeneralReg,
        src1: GeneralReg,
        src2: GeneralReg,
    );

    fn gte_reg64_reg64_reg64(
        buf: &mut Vec<'_, u8>,
        dst: GeneralReg,
        src1: GeneralReg,
        src2: GeneralReg,
    );

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
    GeneralReg: RegTrait,
    FloatReg: RegTrait,
    ASM: Assembler<GeneralReg, FloatReg>,
    CC: CallConv<GeneralReg, FloatReg, ASM>,
> {
    // TODO: A number of the uses of MutMap could probably be some form of linear mutmap
    // They are likely to be small enough that it is faster to use a vec and linearly scan it or keep it sorted and binary search.
    phantom_asm: PhantomData<ASM>,
    phantom_cc: PhantomData<CC>,
    target_info: TargetInfo,
    env: &'a Env<'a>,
    interns: &'a mut Interns,
    helper_proc_gen: CodeGenHelp<'a>,
    helper_proc_symbols: Vec<'a, (Symbol, ProcLayout<'a>)>,
    buf: Vec<'a, u8>,
    relocs: Vec<'a, Relocation>,
    proc_name: Option<String>,
    is_self_recursive: Option<SelfRecursive>,

    last_seen_map: MutMap<Symbol, *const Stmt<'a>>,
    layout_map: MutMap<Symbol, Layout<'a>>,
    free_map: MutMap<*const Stmt<'a>, Vec<'a, Symbol>>,

    literal_map: MutMap<Symbol, (*const Literal<'a>, *const Layout<'a>)>,
    join_map: MutMap<JoinPointId, Vec<'a, (u64, u64)>>,

    storage_manager: StorageManager<'a, GeneralReg, FloatReg, ASM, CC>,
}

/// new creates a new backend that will output to the specific Object.
pub fn new_backend_64bit<
    'a,
    GeneralReg: RegTrait,
    FloatReg: RegTrait,
    ASM: Assembler<GeneralReg, FloatReg>,
    CC: CallConv<GeneralReg, FloatReg, ASM>,
>(
    env: &'a Env,
    target_info: TargetInfo,
    interns: &'a mut Interns,
) -> Backend64Bit<'a, GeneralReg, FloatReg, ASM, CC> {
    Backend64Bit {
        phantom_asm: PhantomData,
        phantom_cc: PhantomData,
        target_info,
        env,
        interns,
        helper_proc_gen: CodeGenHelp::new(
            env.arena,
            env.layout_interner,
            target_info,
            env.module_id,
        ),
        helper_proc_symbols: bumpalo::vec![in env.arena],
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
        GeneralReg: RegTrait,
        FloatReg: RegTrait,
        ASM: Assembler<GeneralReg, FloatReg>,
        CC: CallConv<GeneralReg, FloatReg, ASM>,
    > Backend<'a> for Backend64Bit<'a, GeneralReg, FloatReg, ASM, CC>
{
    fn env(&self) -> &Env<'a> {
        self.env
    }
    fn interns(&self) -> &Interns {
        self.interns
    }
    fn env_interns_helpers_mut(&mut self) -> (&Env<'a>, &mut Interns, &mut CodeGenHelp<'a>) {
        (self.env, self.interns, &mut self.helper_proc_gen)
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

    fn literal_map(&mut self) -> &mut MutMap<Symbol, (*const Literal<'a>, *const Layout<'a>)> {
        &mut self.literal_map
    }

    fn last_seen_map(&mut self) -> &mut MutMap<Symbol, *const Stmt<'a>> {
        &mut self.last_seen_map
    }

    fn layout_map(&mut self) -> &mut MutMap<Symbol, Layout<'a>> {
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

    fn load_args(&mut self, args: &'a [(Layout<'a>, Symbol)], ret_layout: &Layout<'a>) {
        CC::load_args(&mut self.buf, &mut self.storage_manager, args, ret_layout);
    }

    /// Used for generating wrappers for malloc/realloc/free
    fn build_wrapped_jmp(&mut self) -> (&'a [u8], u64) {
        let mut out = bumpalo::vec![in self.env.arena];
        let offset = ASM::tail_call(&mut out);

        (out.into_bump_slice(), offset)
    }

    fn build_fn_call(
        &mut self,
        dst: &Symbol,
        fn_name: String,
        args: &[Symbol],
        arg_layouts: &[Layout<'a>],
        ret_layout: &Layout<'a>,
    ) {
        if let Some(SelfRecursive::SelfRecursive(id)) = self.is_self_recursive {
            if &fn_name == self.proc_name.as_ref().unwrap() && self.join_map.contains_key(&id) {
                return self.build_jump(&id, args, arg_layouts, ret_layout);
            }
        }
        // Save used caller saved regs.
        self.storage_manager
            .push_used_caller_saved_regs_to_stack(&mut self.buf);

        // Put values in param regs or on top of the stack.
        CC::store_args(
            &mut self.buf,
            &mut self.storage_manager,
            dst,
            args,
            arg_layouts,
            ret_layout,
        );

        // Call function and generate reloc.
        ASM::call(&mut self.buf, &mut self.relocs, fn_name);

        // move return value to dst.
        match ret_layout {
            single_register_integers!() => {
                let dst_reg = self.storage_manager.claim_general_reg(&mut self.buf, dst);
                ASM::mov_reg64_reg64(&mut self.buf, dst_reg, CC::GENERAL_RETURN_REGS[0]);
            }
            single_register_floats!() => {
                let dst_reg = self.storage_manager.claim_float_reg(&mut self.buf, dst);
                ASM::mov_freg64_freg64(&mut self.buf, dst_reg, CC::FLOAT_RETURN_REGS[0]);
            }
            _ => {
                CC::load_returned_complex_symbol(
                    &mut self.buf,
                    &mut self.storage_manager,
                    dst,
                    ret_layout,
                );
            }
        }
    }

    fn build_switch(
        &mut self,
        cond_symbol: &Symbol,
        _cond_layout: &Layout<'a>, // cond_layout must be a integer due to potential jump table optimizations.
        branches: &'a [(u64, BranchInfo<'a>, Stmt<'a>)],
        default_branch: &(BranchInfo<'a>, &'a Stmt<'a>),
        ret_layout: &Layout<'a>,
    ) {
        // Switches are a little complex due to keeping track of jumps.
        // In general I am trying to not have to loop over things multiple times or waste memory.
        // The basic plan is to make jumps to nowhere and then correct them once we know the correct address.
        let cond_reg = self
            .storage_manager
            .load_to_general_reg(&mut self.buf, cond_symbol);

        let mut base_storage = self.storage_manager.clone();
        let mut max_branch_stack_size = 0;
        let mut ret_jumps = bumpalo::vec![in self.env.arena];
        let mut tmp = bumpalo::vec![in self.env.arena];
        for (val, _branch_info, stmt) in branches.iter() {
            // TODO: look into branch info and if it matters here.
            tmp.clear();
            // Create jump to next branch if cond_sym not equal to value.
            // Since we don't know the offset yet, set it to 0 and overwrite later.
            let jne_location = self.buf.len();
            let start_offset = ASM::jne_reg64_imm64_imm32(&mut self.buf, cond_reg, *val, 0);

            // Build all statements in this branch. Using storage as from before any branch.
            self.storage_manager = base_storage.clone();
            self.build_stmt(stmt, ret_layout);

            // Build unconditional jump to the end of this switch.
            // Since we don't know the offset yet, set it to 0 and overwrite later.
            let jmp_location = self.buf.len();
            let jmp_offset = ASM::jmp_imm32(&mut self.buf, 0x1234_5678);
            ret_jumps.push((jmp_location, jmp_offset));

            // Overwrite the original jne with the correct offset.
            let end_offset = self.buf.len();
            let jne_offset = end_offset - start_offset;
            ASM::jne_reg64_imm64_imm32(&mut tmp, cond_reg, *val, jne_offset as i32);
            for (i, byte) in tmp.iter().enumerate() {
                self.buf[jne_location + i] = *byte;
            }

            // Update important storage information to avoid overwrites.
            max_branch_stack_size =
                std::cmp::max(max_branch_stack_size, self.storage_manager.stack_size());
            base_storage.update_fn_call_stack_size(self.storage_manager.fn_call_stack_size());
        }
        self.storage_manager = base_storage;
        self.storage_manager
            .update_stack_size(max_branch_stack_size);
        let (_branch_info, stmt) = default_branch;
        self.build_stmt(stmt, ret_layout);

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
        id: &JoinPointId,
        parameters: &'a [Param<'a>],
        body: &'a Stmt<'a>,
        remainder: &'a Stmt<'a>,
        ret_layout: &Layout<'a>,
    ) {
        // Free everything to the stack to make sure they don't get messed up when looping back to this point.
        // TODO: look into a nicer solution.
        self.storage_manager.free_all_to_stack(&mut self.buf);

        // Ensure all the joinpoint parameters have storage locations.
        // On jumps to the joinpoint, we will overwrite those locations as a way to "pass parameters" to the joinpoint.
        self.storage_manager
            .setup_joinpoint(&mut self.buf, id, parameters);

        self.join_map.insert(*id, bumpalo::vec![in self.env.arena]);

        // Build remainder of function first. It is what gets run and jumps to join.
        self.build_stmt(remainder, ret_layout);

        let join_location = self.buf.len() as u64;

        // Build all statements in body.
        self.build_stmt(body, ret_layout);

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
        arg_layouts: &[Layout<'a>],
        _ret_layout: &Layout<'a>,
    ) {
        self.storage_manager
            .setup_jump(&mut self.buf, id, args, arg_layouts);

        let jmp_location = self.buf.len();
        let start_offset = ASM::jmp_imm32(&mut self.buf, 0x1234_5678);

        if let Some(vec) = self.join_map.get_mut(id) {
            vec.push((jmp_location as u64, start_offset as u64))
        } else {
            internal_error!("Jump: unknown point specified to jump to: {:?}", id);
        }
    }

    fn build_num_abs(&mut self, dst: &Symbol, src: &Symbol, layout: &Layout<'a>) {
        match layout {
            Layout::Builtin(Builtin::Int(IntWidth::I64 | IntWidth::U64)) => {
                let dst_reg = self.storage_manager.claim_general_reg(&mut self.buf, dst);
                let src_reg = self.storage_manager.load_to_general_reg(&mut self.buf, src);
                ASM::abs_reg64_reg64(&mut self.buf, dst_reg, src_reg);
            }
            Layout::Builtin(Builtin::Float(FloatWidth::F64)) => {
                let dst_reg = self.storage_manager.claim_float_reg(&mut self.buf, dst);
                let src_reg = self.storage_manager.load_to_float_reg(&mut self.buf, src);
                ASM::abs_freg64_freg64(&mut self.buf, &mut self.relocs, dst_reg, src_reg);
            }
            x => todo!("NumAbs: layout, {:?}", x),
        }
    }

    fn build_num_add(&mut self, dst: &Symbol, src1: &Symbol, src2: &Symbol, layout: &Layout<'a>) {
        match layout {
            Layout::Builtin(Builtin::Int(quadword_and_smaller!())) => {
                let dst_reg = self.storage_manager.claim_general_reg(&mut self.buf, dst);
                let src1_reg = self
                    .storage_manager
                    .load_to_general_reg(&mut self.buf, src1);
                let src2_reg = self
                    .storage_manager
                    .load_to_general_reg(&mut self.buf, src2);
                ASM::add_reg64_reg64_reg64(&mut self.buf, dst_reg, src1_reg, src2_reg);
            }
            Layout::Builtin(Builtin::Float(FloatWidth::F64)) => {
                let dst_reg = self.storage_manager.claim_float_reg(&mut self.buf, dst);
                let src1_reg = self.storage_manager.load_to_float_reg(&mut self.buf, src1);
                let src2_reg = self.storage_manager.load_to_float_reg(&mut self.buf, src2);
                ASM::add_freg64_freg64_freg64(&mut self.buf, dst_reg, src1_reg, src2_reg);
            }
            Layout::Builtin(Builtin::Float(FloatWidth::F32)) => {
                let dst_reg = self.storage_manager.claim_float_reg(&mut self.buf, dst);
                let src1_reg = self.storage_manager.load_to_float_reg(&mut self.buf, src1);
                let src2_reg = self.storage_manager.load_to_float_reg(&mut self.buf, src2);
                ASM::add_freg32_freg32_freg32(&mut self.buf, dst_reg, src1_reg, src2_reg);
            }
            x => todo!("NumAdd: layout, {:?}", x),
        }
    }

    fn build_num_add_checked(
        &mut self,
        dst: &Symbol,
        src1: &Symbol,
        src2: &Symbol,
        num_layout: &Layout<'a>,
        return_layout: &Layout<'a>,
    ) {
        use Builtin::Int;

        let buf = &mut self.buf;

        let struct_size = return_layout.stack_size(self.env.layout_interner, self.target_info);

        let base_offset = self.storage_manager.claim_stack_area(dst, struct_size);

        match num_layout {
            Layout::Builtin(Int(IntWidth::I64 | IntWidth::I32 | IntWidth::I16 | IntWidth::I8)) => {
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
            Layout::Builtin(Int(IntWidth::U64 | IntWidth::U32 | IntWidth::U16 | IntWidth::U8)) => {
                todo!("addChecked for unsigned integers")
            }
            Layout::Builtin(Builtin::Float(FloatWidth::F64)) => {
                todo!("addChecked for f64")
            }
            Layout::Builtin(Builtin::Float(FloatWidth::F32)) => {
                todo!("addChecked for f32")
            }
            x => todo!("NumAdd: layout, {:?}", x),
        }
    }

    fn build_num_mul(&mut self, dst: &Symbol, src1: &Symbol, src2: &Symbol, layout: &Layout<'a>) {
        use Builtin::Int;

        match layout {
            Layout::Builtin(Int(IntWidth::I64 | IntWidth::I32 | IntWidth::I16 | IntWidth::I8)) => {
                let dst_reg = self.storage_manager.claim_general_reg(&mut self.buf, dst);
                let src1_reg = self
                    .storage_manager
                    .load_to_general_reg(&mut self.buf, src1);
                let src2_reg = self
                    .storage_manager
                    .load_to_general_reg(&mut self.buf, src2);
                ASM::imul_reg64_reg64_reg64(&mut self.buf, dst_reg, src1_reg, src2_reg);
            }
            Layout::Builtin(Int(IntWidth::U64 | IntWidth::U32 | IntWidth::U16 | IntWidth::U8)) => {
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
            Layout::Builtin(Builtin::Float(FloatWidth::F64)) => {
                let dst_reg = self.storage_manager.claim_float_reg(&mut self.buf, dst);
                let src1_reg = self.storage_manager.load_to_float_reg(&mut self.buf, src1);
                let src2_reg = self.storage_manager.load_to_float_reg(&mut self.buf, src2);
                ASM::mul_freg64_freg64_freg64(&mut self.buf, dst_reg, src1_reg, src2_reg);
            }
            Layout::Builtin(Builtin::Float(FloatWidth::F32)) => {
                let dst_reg = self.storage_manager.claim_float_reg(&mut self.buf, dst);
                let src1_reg = self.storage_manager.load_to_float_reg(&mut self.buf, src1);
                let src2_reg = self.storage_manager.load_to_float_reg(&mut self.buf, src2);
                ASM::mul_freg32_freg32_freg32(&mut self.buf, dst_reg, src1_reg, src2_reg);
            }
            x => todo!("NumMul: layout, {:?}", x),
        }
    }

    fn build_num_div(&mut self, dst: &Symbol, src1: &Symbol, src2: &Symbol, layout: &Layout<'a>) {
        match layout {
            Layout::Builtin(Builtin::Int(
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
            Layout::Builtin(Builtin::Int(
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
            Layout::Builtin(Builtin::Float(FloatWidth::F64)) => {
                let dst_reg = self.storage_manager.claim_float_reg(&mut self.buf, dst);
                let src1_reg = self.storage_manager.load_to_float_reg(&mut self.buf, src1);
                let src2_reg = self.storage_manager.load_to_float_reg(&mut self.buf, src2);
                ASM::div_freg64_freg64_freg64(&mut self.buf, dst_reg, src1_reg, src2_reg);
            }
            Layout::Builtin(Builtin::Float(FloatWidth::F32)) => {
                let dst_reg = self.storage_manager.claim_float_reg(&mut self.buf, dst);
                let src1_reg = self.storage_manager.load_to_float_reg(&mut self.buf, src1);
                let src2_reg = self.storage_manager.load_to_float_reg(&mut self.buf, src2);
                ASM::div_freg32_freg32_freg32(&mut self.buf, dst_reg, src1_reg, src2_reg);
            }
            x => todo!("NumDiv: layout, {:?}", x),
        }
    }

    fn build_num_neg(&mut self, dst: &Symbol, src: &Symbol, layout: &Layout<'a>) {
        match layout {
            Layout::Builtin(Builtin::Int(IntWidth::I64 | IntWidth::U64)) => {
                let dst_reg = self.storage_manager.claim_general_reg(&mut self.buf, dst);
                let src_reg = self.storage_manager.load_to_general_reg(&mut self.buf, src);
                ASM::neg_reg64_reg64(&mut self.buf, dst_reg, src_reg);
            }
            x => todo!("NumNeg: layout, {:?}", x),
        }
    }

    fn build_num_sub(&mut self, dst: &Symbol, src1: &Symbol, src2: &Symbol, layout: &Layout<'a>) {
        match layout {
            Layout::Builtin(Builtin::Int(IntWidth::I64 | IntWidth::U64)) => {
                let dst_reg = self.storage_manager.claim_general_reg(&mut self.buf, dst);
                let src1_reg = self
                    .storage_manager
                    .load_to_general_reg(&mut self.buf, src1);
                let src2_reg = self
                    .storage_manager
                    .load_to_general_reg(&mut self.buf, src2);
                ASM::sub_reg64_reg64_reg64(&mut self.buf, dst_reg, src1_reg, src2_reg);
            }
            x => todo!("NumSub: layout, {:?}", x),
        }
    }

    fn build_eq(&mut self, dst: &Symbol, src1: &Symbol, src2: &Symbol, arg_layout: &Layout<'a>) {
        match arg_layout {
            Layout::Builtin(single_register_int_builtins!()) => {
                let dst_reg = self.storage_manager.claim_general_reg(&mut self.buf, dst);
                let src1_reg = self
                    .storage_manager
                    .load_to_general_reg(&mut self.buf, src1);
                let src2_reg = self
                    .storage_manager
                    .load_to_general_reg(&mut self.buf, src2);
                ASM::eq_reg64_reg64_reg64(&mut self.buf, dst_reg, src1_reg, src2_reg);
            }
            x => todo!("NumEq: layout, {:?}", x),
        }
    }

    fn build_neq(&mut self, dst: &Symbol, src1: &Symbol, src2: &Symbol, arg_layout: &Layout<'a>) {
        match arg_layout {
            Layout::Builtin(Builtin::Int(IntWidth::I64 | IntWidth::U64)) => {
                let dst_reg = self.storage_manager.claim_general_reg(&mut self.buf, dst);
                let src1_reg = self
                    .storage_manager
                    .load_to_general_reg(&mut self.buf, src1);
                let src2_reg = self
                    .storage_manager
                    .load_to_general_reg(&mut self.buf, src2);
                ASM::neq_reg64_reg64_reg64(&mut self.buf, dst_reg, src1_reg, src2_reg);
            }
            x => todo!("NumNeq: layout, {:?}", x),
        }
    }

    fn build_num_lt(
        &mut self,
        dst: &Symbol,
        src1: &Symbol,
        src2: &Symbol,
        arg_layout: &Layout<'a>,
    ) {
        match arg_layout {
            Layout::Builtin(Builtin::Int(IntWidth::I64 | IntWidth::U64)) => {
                let dst_reg = self.storage_manager.claim_general_reg(&mut self.buf, dst);
                let src1_reg = self
                    .storage_manager
                    .load_to_general_reg(&mut self.buf, src1);
                let src2_reg = self
                    .storage_manager
                    .load_to_general_reg(&mut self.buf, src2);
                ASM::lt_reg64_reg64_reg64(&mut self.buf, dst_reg, src1_reg, src2_reg);
            }
            x => todo!("NumLt: layout, {:?}", x),
        }
    }

    fn build_num_to_frac(
        &mut self,
        dst: &Symbol,
        src: &Symbol,
        arg_layout: &Layout<'a>,
        ret_layout: &Layout<'a>,
    ) {
        let dst_reg = self.storage_manager.claim_float_reg(&mut self.buf, dst);
        match (arg_layout, ret_layout) {
            (
                Layout::Builtin(Builtin::Int(IntWidth::I32 | IntWidth::I64)),
                Layout::Builtin(Builtin::Float(FloatWidth::F64)),
            ) => {
                let src_reg = self.storage_manager.load_to_general_reg(&mut self.buf, src);
                ASM::to_float_freg64_reg64(&mut self.buf, dst_reg, src_reg);
            }
            (
                Layout::Builtin(Builtin::Int(IntWidth::I32 | IntWidth::I64)),
                Layout::Builtin(Builtin::Float(FloatWidth::F32)),
            ) => {
                let src_reg = self.storage_manager.load_to_general_reg(&mut self.buf, src);
                ASM::to_float_freg32_reg64(&mut self.buf, dst_reg, src_reg);
            }
            (
                Layout::Builtin(Builtin::Float(FloatWidth::F64)),
                Layout::Builtin(Builtin::Float(FloatWidth::F32)),
            ) => {
                let src_reg = self.storage_manager.load_to_float_reg(&mut self.buf, src);
                ASM::to_float_freg32_freg64(&mut self.buf, dst_reg, src_reg);
            }
            (
                Layout::Builtin(Builtin::Float(FloatWidth::F32)),
                Layout::Builtin(Builtin::Float(FloatWidth::F64)),
            ) => {
                let src_reg = self.storage_manager.load_to_float_reg(&mut self.buf, src);
                ASM::to_float_freg64_freg32(&mut self.buf, dst_reg, src_reg);
            }
            (
                Layout::Builtin(Builtin::Float(FloatWidth::F64)),
                Layout::Builtin(Builtin::Float(FloatWidth::F64)),
            ) => {
                let src_reg = self.storage_manager.load_to_float_reg(&mut self.buf, src);
                ASM::mov_freg64_freg64(&mut self.buf, dst_reg, src_reg);
            }
            (
                Layout::Builtin(Builtin::Float(FloatWidth::F32)),
                Layout::Builtin(Builtin::Float(FloatWidth::F32)),
            ) => {
                let src_reg = self.storage_manager.load_to_float_reg(&mut self.buf, src);
                ASM::mov_freg64_freg64(&mut self.buf, dst_reg, src_reg);
            }
            (a, r) => todo!("NumToFrac: layout, arg {:?}, ret {:?}", a, r),
        }
    }

    fn build_num_lte(
        &mut self,
        dst: &Symbol,
        src1: &Symbol,
        src2: &Symbol,
        arg_layout: &Layout<'a>,
    ) {
        match arg_layout {
            Layout::Builtin(single_register_int_builtins!()) => {
                let dst_reg = self.storage_manager.claim_general_reg(&mut self.buf, dst);
                let src1_reg = self
                    .storage_manager
                    .load_to_general_reg(&mut self.buf, src1);
                let src2_reg = self
                    .storage_manager
                    .load_to_general_reg(&mut self.buf, src2);
                ASM::lte_reg64_reg64_reg64(&mut self.buf, dst_reg, src1_reg, src2_reg);
            }
            x => todo!("NumLte: layout, {:?}", x),
        }
    }

    fn build_num_gte(
        &mut self,
        dst: &Symbol,
        src1: &Symbol,
        src2: &Symbol,
        arg_layout: &Layout<'a>,
    ) {
        match arg_layout {
            Layout::Builtin(single_register_int_builtins!()) => {
                let dst_reg = self.storage_manager.claim_general_reg(&mut self.buf, dst);
                let src1_reg = self
                    .storage_manager
                    .load_to_general_reg(&mut self.buf, src1);
                let src2_reg = self
                    .storage_manager
                    .load_to_general_reg(&mut self.buf, src2);
                ASM::gte_reg64_reg64_reg64(&mut self.buf, dst_reg, src1_reg, src2_reg);
            }
            x => todo!("NumGte: layout, {:?}", x),
        }
    }

    fn build_list_len(&mut self, dst: &Symbol, list: &Symbol) {
        self.storage_manager.list_len(&mut self.buf, dst, list);
    }

    fn build_list_get_unsafe(
        &mut self,
        dst: &Symbol,
        list: &Symbol,
        index: &Symbol,
        ret_layout: &Layout<'a>,
    ) {
        let (base_offset, _) = self.storage_manager.stack_offset_and_size(list);
        let index_reg = self
            .storage_manager
            .load_to_general_reg(&mut self.buf, index);
        let ret_stack_size =
            ret_layout.stack_size(self.env.layout_interner, self.storage_manager.target_info());
        // TODO: This can be optimized with smarter instructions.
        // Also can probably be moved into storage manager at least partly.
        self.storage_manager.with_tmp_general_reg(
            &mut self.buf,
            |storage_manager, buf, list_ptr| {
                ASM::mov_reg64_base32(buf, list_ptr, base_offset as i32);
                storage_manager.with_tmp_general_reg(buf, |storage_manager, buf, tmp| {
                    ASM::mov_reg64_imm64(buf, tmp, ret_stack_size as i64);
                    ASM::imul_reg64_reg64_reg64(buf, tmp, tmp, index_reg);
                    ASM::add_reg64_reg64_reg64(buf, tmp, tmp, list_ptr);
                    match ret_layout {
                        single_register_integers!() if ret_stack_size == 8 => {
                            let dst_reg = storage_manager.claim_general_reg(buf, dst);
                            ASM::mov_reg64_mem64_offset32(buf, dst_reg, tmp, 0);
                        }
                        x => internal_error!("Loading list element with layout: {:?}", x),
                    }
                });
            },
        );
    }

    fn build_list_replace_unsafe(
        &mut self,
        dst: &Symbol,
        args: &'a [Symbol],
        arg_layouts: &[Layout<'a>],
        ret_layout: &Layout<'a>,
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

        let u32_layout = &Layout::Builtin(Builtin::Int(IntWidth::U32));
        let list_alignment = list_layout
            .alignment_bytes(self.env.layout_interner, self.storage_manager.target_info());
        self.load_literal(
            &Symbol::DEV_TMP,
            u32_layout,
            &Literal::Int((list_alignment as i128).to_ne_bytes()),
        );

        // Have to pass the input element by pointer, so put it on the stack and load it's address.
        self.storage_manager
            .ensure_symbol_on_stack(&mut self.buf, &elem);
        let u64_layout = &Layout::Builtin(Builtin::Int(IntWidth::U64));
        let (new_elem_offset, _) = self.storage_manager.stack_offset_and_size(&elem);
        // Load address of output element into register.
        let reg = self
            .storage_manager
            .claim_general_reg(&mut self.buf, &Symbol::DEV_TMP2);
        ASM::add_reg64_reg64_imm32(&mut self.buf, reg, CC::BASE_PTR_REG, new_elem_offset);

        // Load the elements size.
        let elem_stack_size =
            elem_layout.stack_size(self.env.layout_interner, self.storage_manager.target_info());
        self.load_literal(
            &Symbol::DEV_TMP3,
            u64_layout,
            &Literal::Int((elem_stack_size as i128).to_ne_bytes()),
        );

        // Setup the return location.
        let base_offset = self.storage_manager.claim_stack_area(
            dst,
            ret_layout.stack_size(self.env.layout_interner, self.storage_manager.target_info()),
        );

        let ret_fields = if let Layout::Struct { field_layouts, .. } = ret_layout {
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
                base_offset
                    + ret_fields[0]
                        .stack_size(self.env.layout_interner, self.storage_manager.target_info())
                        as i32,
                base_offset,
            )
        } else {
            (
                base_offset,
                base_offset
                    + ret_fields[0]
                        .stack_size(self.env.layout_interner, self.storage_manager.target_info())
                        as i32,
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
        let lowlevel_arg_layouts = bumpalo::vec![
        in self.env.arena;
                list_layout,
                *u32_layout,
                index_layout,
                *u64_layout,
                *u64_layout,
                *u64_layout,
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
            &mut self.buf,
            out_list_offset,
            &Symbol::DEV_TMP5,
            &list_layout,
        );

        self.free_symbol(&Symbol::DEV_TMP5);
    }

    fn build_ptr_cast(&mut self, dst: &Symbol, src: &Symbol) {
        let dst_reg = self.storage_manager.claim_general_reg(&mut self.buf, dst);
        self.storage_manager
            .ensure_symbol_on_stack(&mut self.buf, src);
        let (offset, _) = self.storage_manager.stack_offset_and_size(src);
        ASM::add_reg64_reg64_imm32(&mut self.buf, dst_reg, CC::BASE_PTR_REG, offset);
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
        elem_layout: &Layout<'a>,
        elems: &'a [ListLiteralElement<'a>],
    ) {
        // Allocate
        // This requires at least 8 for the refcount alignment.
        let allocation_alignment = std::cmp::max(
            8,
            elem_layout.allocation_alignment_bytes(
                self.env.layout_interner,
                self.storage_manager.target_info(),
            ) as u64,
        );

        let elem_size = elem_layout
            .stack_size(self.env.layout_interner, self.storage_manager.target_info())
            as u64;
        let allocation_size = elem_size * elems.len() as u64 + allocation_alignment /* add space for refcount */;
        let u64_layout = Layout::Builtin(Builtin::Int(IntWidth::U64));
        self.load_literal(
            &Symbol::DEV_TMP,
            &u64_layout,
            &Literal::Int((allocation_size as i128).to_ne_bytes()),
        );
        let u32_layout = Layout::Builtin(Builtin::Int(IntWidth::U32));
        self.load_literal(
            &Symbol::DEV_TMP2,
            &u32_layout,
            &Literal::Int((allocation_alignment as i128).to_ne_bytes()),
        );

        self.build_fn_call(
            &Symbol::DEV_TMP3,
            "roc_alloc".to_string(),
            &[Symbol::DEV_TMP, Symbol::DEV_TMP2],
            &[u64_layout, u32_layout],
            &u64_layout,
        );
        self.free_symbol(&Symbol::DEV_TMP);
        self.free_symbol(&Symbol::DEV_TMP2);

        // Fill pointer with elems
        let ptr_reg = self
            .storage_manager
            .load_to_general_reg(&mut self.buf, &Symbol::DEV_TMP3);
        // Point to first element of array.
        ASM::add_reg64_reg64_imm32(&mut self.buf, ptr_reg, ptr_reg, allocation_alignment as i32);

        // fill refcount at -8.
        self.storage_manager.with_tmp_general_reg(
            &mut self.buf,
            |_storage_manager, buf, tmp_reg| {
                ASM::mov_reg64_imm64(buf, tmp_reg, REFCOUNT_ONE as i64);
                ASM::mov_mem64_offset32_reg64(buf, ptr_reg, -8, tmp_reg);
            },
        );

        // Copy everything into output array.
        let mut elem_offset = 0;
        for elem in elems {
            // TODO: this could be a lot faster when loading large lists
            // if we move matching on the element layout to outside this loop.
            // It also greatly bloats the code here.
            // Refactor this and switch to one external match.
            // We also could make loadining indivitual literals much faster
            let elem_sym = match elem {
                ListLiteralElement::Symbol(sym) => sym,
                ListLiteralElement::Literal(lit) => {
                    self.load_literal(&Symbol::DEV_TMP, elem_layout, lit);
                    &Symbol::DEV_TMP
                }
            };
            // TODO: Expand to all types.
            match elem_layout {
                Layout::Builtin(Builtin::Int(IntWidth::I64 | IntWidth::U64)) => {
                    let sym_reg = self
                        .storage_manager
                        .load_to_general_reg(&mut self.buf, elem_sym);
                    ASM::mov_mem64_offset32_reg64(&mut self.buf, ptr_reg, elem_offset, sym_reg);
                }
                _ if elem_size == 0 => {}
                _ if elem_size > 8 => {
                    let (from_offset, size) = self.storage_manager.stack_offset_and_size(elem_sym);
                    debug_assert!(from_offset % 8 == 0);
                    debug_assert!(size % 8 == 0);
                    debug_assert_eq!(size as u64, elem_size);
                    self.storage_manager.with_tmp_general_reg(
                        &mut self.buf,
                        |_storage_manager, buf, tmp_reg| {
                            for i in (0..size as i32).step_by(8) {
                                ASM::mov_reg64_base32(buf, tmp_reg, from_offset + i);
                                ASM::mov_mem64_offset32_reg64(buf, ptr_reg, elem_offset, tmp_reg);
                            }
                        },
                    );
                }
                x => todo!("copying data to list with layout, {:?}", x),
            }
            elem_offset += elem_size as i32;
            if elem_sym == &Symbol::DEV_TMP {
                self.free_symbol(elem_sym);
            }
        }

        // Setup list on stack.
        self.storage_manager.with_tmp_general_reg(
            &mut self.buf,
            |storage_manager, buf, tmp_reg| {
                let base_offset = storage_manager.claim_stack_area(sym, 24);
                ASM::mov_base32_reg64(buf, base_offset, ptr_reg);

                ASM::mov_reg64_imm64(buf, tmp_reg, elems.len() as i64);
                ASM::mov_base32_reg64(buf, base_offset + 8, tmp_reg);
                ASM::mov_base32_reg64(buf, base_offset + 16, tmp_reg);
            },
        );
        self.free_symbol(&Symbol::DEV_TMP3);
    }

    fn create_struct(&mut self, sym: &Symbol, layout: &Layout<'a>, fields: &'a [Symbol]) {
        self.storage_manager
            .create_struct(&mut self.buf, sym, layout, fields);
    }

    fn load_struct_at_index(
        &mut self,
        sym: &Symbol,
        structure: &Symbol,
        index: u64,
        field_layouts: &'a [Layout<'a>],
    ) {
        self.storage_manager
            .load_field_at_index(sym, structure, index, field_layouts);
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
            UnionLayout::NonRecursive(tag_layouts) | UnionLayout::Recursive(tag_layouts) => {
                self.storage_manager.load_field_at_index(
                    sym,
                    structure,
                    index,
                    tag_layouts[tag_id as usize],
                );
            }
            x => todo!("loading from union type: {:?}", x),
        }
    }

    fn get_tag_id(&mut self, sym: &Symbol, structure: &Symbol, union_layout: &UnionLayout<'a>) {
        self.storage_manager
            .load_union_tag_id(&mut self.buf, sym, structure, union_layout);
    }

    fn tag(
        &mut self,
        sym: &Symbol,
        fields: &'a [Symbol],
        union_layout: &UnionLayout<'a>,
        tag_id: TagIdIntType,
    ) {
        self.storage_manager
            .create_union(&mut self.buf, sym, union_layout, fields, tag_id)
    }

    fn load_literal(&mut self, sym: &Symbol, layout: &Layout<'a>, lit: &Literal<'a>) {
        match (lit, layout) {
            (
                Literal::Int(x),
                Layout::Builtin(Builtin::Int(
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
            (Literal::Bool(x), Layout::Builtin(Builtin::Bool)) => {
                let reg = self.storage_manager.claim_general_reg(&mut self.buf, sym);
                let val = [*x as u8; 16];
                ASM::mov_reg64_imm64(&mut self.buf, reg, i128::from_ne_bytes(val) as i64);
            }
            (Literal::Float(x), Layout::Builtin(Builtin::Float(FloatWidth::F64))) => {
                let reg = self.storage_manager.claim_float_reg(&mut self.buf, sym);
                let val = *x;
                ASM::mov_freg64_imm64(&mut self.buf, &mut self.relocs, reg, val);
            }
            (Literal::Float(x), Layout::Builtin(Builtin::Float(FloatWidth::F32))) => {
                let reg = self.storage_manager.claim_float_reg(&mut self.buf, sym);
                let val = *x as f32;
                ASM::mov_freg32_imm32(&mut self.buf, &mut self.relocs, reg, val);
            }
            (Literal::Str(x), Layout::Builtin(Builtin::Str)) if x.len() < 24 => {
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
            }
            x => todo!("loading literal, {:?}", x),
        }
    }

    fn free_symbol(&mut self, sym: &Symbol) {
        self.join_map.remove(&JoinPointId(*sym));
        self.storage_manager.free_symbol(sym);
    }

    fn return_symbol(&mut self, sym: &Symbol, layout: &Layout<'a>) {
        if self.storage_manager.is_stored_primitive(sym) {
            // Just load it to the correct type of reg as a stand alone value.
            match layout {
                single_register_integers!() => {
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
                _ => {
                    internal_error!("All primitive valuse should fit in a single register");
                }
            }
        } else {
            CC::return_complex_symbol(&mut self.buf, &mut self.storage_manager, sym, layout)
        }
        let inst_loc = self.buf.len() as u64;
        let offset = ASM::jmp_imm32(&mut self.buf, 0x1234_5678) as u64;
        self.relocs.push(Relocation::JmpToReturn {
            inst_loc,
            inst_size: self.buf.len() as u64 - inst_loc,
            offset,
        });
    }
}

/// This impl block is for ir related instructions that need backend specific information.
/// For example, loading a symbol for doing a computation.
impl<
        'a,
        FloatReg: RegTrait,
        GeneralReg: RegTrait,
        ASM: Assembler<GeneralReg, FloatReg>,
        CC: CallConv<GeneralReg, FloatReg, ASM>,
    > Backend64Bit<'a, GeneralReg, FloatReg, ASM, CC>
{
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
}

#[macro_export]
macro_rules! sign_extended_int_builtins {
    () => {
        Builtin::Int(IntWidth::I8 | IntWidth::I16 | IntWidth::I32 | IntWidth::I64 | IntWidth::I128)
    };
}

#[macro_export]
macro_rules! zero_extended_int_builtins {
    () => {
        Builtin::Int(IntWidth::U8 | IntWidth::U16 | IntWidth::U32 | IntWidth::U64 | IntWidth::U128)
    };
}

#[macro_export]
macro_rules! single_register_int_builtins {
    () => {
        Builtin::Int(
            IntWidth::I8
                | IntWidth::I16
                | IntWidth::I32
                | IntWidth::I64
                | IntWidth::U8
                | IntWidth::U16
                | IntWidth::U32
                | IntWidth::U64,
        )
    };
}

#[macro_export]
macro_rules! single_register_integers {
    () => {
        Layout::Builtin(Builtin::Bool | single_register_int_builtins!()) | Layout::RecursivePointer
    };
}

#[macro_export]
macro_rules! single_register_floats {
    () => {
        Layout::Builtin(Builtin::Float(FloatWidth::F32 | FloatWidth::F64))
    };
}

#[macro_export]
macro_rules! single_register_layouts {
    () => {
        single_register_integers!() | single_register_floats!()
    };
}
