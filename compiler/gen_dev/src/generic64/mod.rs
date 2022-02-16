use crate::{Backend, Env, Relocation};
use bumpalo::collections::Vec;
use roc_builtins::bitcode::{FloatWidth, IntWidth};
use roc_collections::all::{MutMap, MutSet};
use roc_error_macros::internal_error;
use roc_module::symbol::{Interns, Symbol};
use roc_mono::code_gen_help::CodeGenHelp;
use roc_mono::ir::{BranchInfo, JoinPointId, Literal, Param, ProcLayout, SelfRecursive, Stmt};
use roc_mono::layout::{Builtin, Layout};
use roc_target::TargetInfo;
use std::marker::PhantomData;

pub mod aarch64;
pub mod x86_64;

const TARGET_INFO: TargetInfo = TargetInfo::default_x86_64();

pub trait CallConv<GeneralReg: RegTrait, FloatReg: RegTrait> {
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
        requested_stack_size: i32,
        fn_call_stack_size: i32,
    ) -> i32;
    fn cleanup_stack<'a>(
        buf: &mut Vec<'a, u8>,
        general_saved_regs: &[GeneralReg],
        aligned_stack_size: i32,
        fn_call_stack_size: i32,
    );

    // load_args updates the symbol map to know where every arg is stored.
    // It returns the total stack space after loading the args.
    fn load_args<'a>(
        buf: &mut Vec<'a, u8>,
        symbol_map: &mut MutMap<Symbol, SymbolStorage<GeneralReg, FloatReg>>,
        args: &'a [(Layout<'a>, Symbol)],
        // ret_layout is needed because if it is a complex type, we pass a pointer as the first arg.
        ret_layout: &Layout<'a>,
        stack_size: u32,
    ) -> u32;

    // store_args stores the args in registers and on the stack for function calling.
    // It returns the amount of stack space needed to temporarily store the args.
    fn store_args<'a>(
        buf: &mut Vec<'a, u8>,
        symbol_map: &MutMap<Symbol, SymbolStorage<GeneralReg, FloatReg>>,
        args: &'a [Symbol],
        arg_layouts: &[Layout<'a>],
        // ret_layout is needed because if it is a complex type, we pass a pointer as the first arg.
        ret_layout: &Layout<'a>,
    ) -> u32;

    // return_struct returns a struct currently on the stack at `struct_offset`.
    // It does so using registers and stack as necessary.
    fn return_struct<'a>(
        buf: &mut Vec<'a, u8>,
        struct_offset: i32,
        struct_size: u32,
        field_layouts: &[Layout<'a>],
        ret_reg: Option<GeneralReg>,
    );

    // returns true if the layout should be returned via an argument pointer.
    fn returns_via_arg_pointer(ret_layout: &Layout) -> bool;
}

/// Assembler contains calls to the backend assembly generator.
/// These calls do not necessarily map directly to a single assembly instruction.
/// They are higher level in cases where an instruction would not be common and shared between multiple architectures.
/// Thus, some backends will need to use mulitiple instructions to preform a single one of this calls.
/// Generally, I prefer explicit sources, as opposed to dst being one of the sources. Ex: `x = x + y` would be `add x, x, y` instead of `add x, y`.
/// dst should always come before sources.
pub trait Assembler<GeneralReg: RegTrait, FloatReg: RegTrait> {
    fn abs_reg64_reg64(buf: &mut Vec<'_, u8>, dst: GeneralReg, src: GeneralReg);
    fn abs_freg64_freg64(
        buf: &mut Vec<'_, u8>,
        relocs: &mut Vec<'_, Relocation>,
        dst: FloatReg,
        src: FloatReg,
    );

    fn add_reg64_reg64_imm32(buf: &mut Vec<'_, u8>, dst: GeneralReg, src1: GeneralReg, imm32: i32);
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

    // Jumps by an offset of offset bytes unconditionally.
    // It should always generate the same number of bytes to enable replacement if offset changes.
    // It returns the base offset to calculate the jump from (generally the instruction after the jump).
    fn jmp_imm32(buf: &mut Vec<'_, u8>, offset: i32) -> usize;

    fn tail_call(buf: &mut Vec<'_, u8>) -> u64;

    // Jumps by an offset of offset bytes if reg is not equal to imm.
    // It should always generate the same number of bytes to enable replacement if offset changes.
    // It returns the base offset to calculate the jump from (generally the instruction after the jump).
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

    fn mov_freg64_stack32(buf: &mut Vec<'_, u8>, dst: FloatReg, offset: i32);
    fn mov_reg64_stack32(buf: &mut Vec<'_, u8>, dst: GeneralReg, offset: i32);
    fn mov_stack32_freg64(buf: &mut Vec<'_, u8>, offset: i32, src: FloatReg);
    fn mov_stack32_reg64(buf: &mut Vec<'_, u8>, offset: i32, src: GeneralReg);

    fn neg_reg64_reg64(buf: &mut Vec<'_, u8>, dst: GeneralReg, src: GeneralReg);
    fn imul_reg64_reg64_reg64(
        buf: &mut Vec<'_, u8>,
        dst: GeneralReg,
        src1: GeneralReg,
        src2: GeneralReg,
    );

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

    fn gte_reg64_reg64_reg64(
        buf: &mut Vec<'_, u8>,
        dst: GeneralReg,
        src1: GeneralReg,
        src2: GeneralReg,
    );

    fn ret(buf: &mut Vec<'_, u8>);
}

#[derive(Clone, Debug, PartialEq)]
pub enum SymbolStorage<GeneralReg: RegTrait, FloatReg: RegTrait> {
    GeneralReg(GeneralReg),
    FloatReg(FloatReg),
    Base {
        offset: i32,
        size: u32,
        owned: bool,
    },
    BaseAndGeneralReg {
        reg: GeneralReg,
        offset: i32,
        size: u32,
        owned: bool,
    },
    BaseAndFloatReg {
        reg: FloatReg,
        offset: i32,
        size: u32,
        owned: bool,
    },
}

pub trait RegTrait: Copy + Eq + std::hash::Hash + std::fmt::Debug + 'static {
    fn value(&self) -> u8;
}

pub struct Backend64Bit<
    'a,
    GeneralReg: RegTrait,
    FloatReg: RegTrait,
    ASM: Assembler<GeneralReg, FloatReg>,
    CC: CallConv<GeneralReg, FloatReg>,
> {
    phantom_asm: PhantomData<ASM>,
    phantom_cc: PhantomData<CC>,
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

    symbol_storage_map: MutMap<Symbol, SymbolStorage<GeneralReg, FloatReg>>,
    literal_map: MutMap<Symbol, (*const Literal<'a>, *const Layout<'a>)>,
    join_map: MutMap<JoinPointId, u64>,

    // This should probably be smarter than a vec.
    // There are certain registers we should always use first. With pushing and popping, this could get mixed.
    general_free_regs: Vec<'a, GeneralReg>,
    float_free_regs: Vec<'a, FloatReg>,

    // The last major thing we need is a way to decide what reg to free when all of them are full.
    // Theoretically we want a basic lru cache for the currently loaded symbols.
    // For now just a vec of used registers and the symbols they contain.
    general_used_regs: Vec<'a, (GeneralReg, Symbol)>,
    float_used_regs: Vec<'a, (FloatReg, Symbol)>,

    // used callee saved regs must be tracked for pushing and popping at the beginning/end of the function.
    general_used_callee_saved_regs: MutSet<GeneralReg>,
    float_used_callee_saved_regs: MutSet<FloatReg>,

    free_stack_chunks: Vec<'a, (i32, u32)>,
    stack_size: u32,
    // The amount of stack space needed to pass args for function calling.
    fn_call_stack_size: u32,
}

/// new creates a new backend that will output to the specific Object.
pub fn new_backend_64bit<
    'a,
    GeneralReg: RegTrait,
    FloatReg: RegTrait,
    ASM: Assembler<GeneralReg, FloatReg>,
    CC: CallConv<GeneralReg, FloatReg>,
>(
    env: &'a Env,
    interns: &'a mut Interns,
) -> Backend64Bit<'a, GeneralReg, FloatReg, ASM, CC> {
    Backend64Bit {
        phantom_asm: PhantomData,
        phantom_cc: PhantomData,
        env,
        interns,
        helper_proc_gen: CodeGenHelp::new(env.arena, TARGET_INFO, env.module_id),
        helper_proc_symbols: bumpalo::vec![in env.arena],
        proc_name: None,
        is_self_recursive: None,
        buf: bumpalo::vec![in env.arena],
        relocs: bumpalo::vec![in env.arena],
        last_seen_map: MutMap::default(),
        layout_map: MutMap::default(),
        free_map: MutMap::default(),
        symbol_storage_map: MutMap::default(),
        literal_map: MutMap::default(),
        join_map: MutMap::default(),
        general_free_regs: bumpalo::vec![in env.arena],
        general_used_regs: bumpalo::vec![in env.arena],
        general_used_callee_saved_regs: MutSet::default(),
        float_free_regs: bumpalo::vec![in env.arena],
        float_used_regs: bumpalo::vec![in env.arena],
        float_used_callee_saved_regs: MutSet::default(),
        free_stack_chunks: bumpalo::vec![in env.arena],
        stack_size: 0,
        fn_call_stack_size: 0,
    }
}

impl<
        'a,
        GeneralReg: RegTrait,
        FloatReg: RegTrait,
        ASM: Assembler<GeneralReg, FloatReg>,
        CC: CallConv<GeneralReg, FloatReg>,
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
        self.stack_size = 0;
        self.free_stack_chunks.clear();
        self.fn_call_stack_size = 0;
        self.last_seen_map.clear();
        self.layout_map.clear();
        self.join_map.clear();
        self.free_map.clear();
        self.symbol_storage_map.clear();
        self.buf.clear();
        self.general_used_callee_saved_regs.clear();
        self.general_free_regs.clear();
        self.general_used_regs.clear();
        self.general_free_regs
            .extend_from_slice(CC::GENERAL_DEFAULT_FREE_REGS);
        self.float_used_callee_saved_regs.clear();
        self.float_free_regs.clear();
        self.float_used_regs.clear();
        self.float_free_regs
            .extend_from_slice(CC::FLOAT_DEFAULT_FREE_REGS);
        self.helper_proc_symbols.clear();
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
        let mut used_regs = bumpalo::vec![in self.env.arena];
        used_regs.extend(&self.general_used_callee_saved_regs);
        let aligned_stack_size = CC::setup_stack(
            &mut out,
            &used_regs,
            self.stack_size as i32,
            self.fn_call_stack_size as i32,
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
            &used_regs,
            aligned_stack_size,
            self.fn_call_stack_size as i32,
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
        self.stack_size = CC::load_args(
            &mut self.buf,
            &mut self.symbol_storage_map,
            args,
            ret_layout,
            self.stack_size,
        );
        // Update used and free regs.
        for (sym, storage) in &self.symbol_storage_map {
            match storage {
                SymbolStorage::GeneralReg(reg) | SymbolStorage::BaseAndGeneralReg { reg, .. } => {
                    self.general_free_regs.retain(|r| *r != *reg);
                    self.general_used_regs.push((*reg, *sym));
                }
                SymbolStorage::FloatReg(reg) | SymbolStorage::BaseAndFloatReg { reg, .. } => {
                    self.float_free_regs.retain(|r| *r != *reg);
                    self.float_used_regs.push((*reg, *sym));
                }
                SymbolStorage::Base { .. } => {}
            }
        }
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
        args: &'a [Symbol],
        arg_layouts: &[Layout<'a>],
        ret_layout: &Layout<'a>,
    ) {
        if let Some(SelfRecursive::SelfRecursive(id)) = self.is_self_recursive {
            if &fn_name == self.proc_name.as_ref().unwrap() && self.join_map.contains_key(&id) {
                return self.build_jump(&id, args, arg_layouts, ret_layout);
            }
        }
        // Save used caller saved regs.
        self.push_used_caller_saved_regs_to_stack();

        // Put values in param regs or on top of the stack.
        let tmp_stack_size = CC::store_args(
            &mut self.buf,
            &self.symbol_storage_map,
            args,
            arg_layouts,
            ret_layout,
        );
        self.fn_call_stack_size = std::cmp::max(self.fn_call_stack_size, tmp_stack_size);

        // Call function and generate reloc.
        ASM::call(&mut self.buf, &mut self.relocs, fn_name);

        // move return value to dst.
        match ret_layout {
            Layout::Builtin(Builtin::Int(IntWidth::I64 | IntWidth::U64) | Builtin::Bool) => {
                let dst_reg = self.claim_general_reg(dst);
                ASM::mov_reg64_reg64(&mut self.buf, dst_reg, CC::GENERAL_RETURN_REGS[0]);
            }
            Layout::Builtin(Builtin::Float(FloatWidth::F64)) => {
                let dst_reg = self.claim_float_reg(dst);
                ASM::mov_freg64_freg64(&mut self.buf, dst_reg, CC::FLOAT_RETURN_REGS[0]);
            }
            Layout::Builtin(Builtin::Str) => {
                if CC::returns_via_arg_pointer(ret_layout) {
                    // This will happen on windows, return via pointer here.
                    todo!("FnCall: Returning strings via pointer");
                } else {
                    let offset = self.claim_stack_size(16);
                    self.symbol_storage_map.insert(
                        *dst,
                        SymbolStorage::Base {
                            offset,
                            size: 16,
                            owned: true,
                        },
                    );
                    ASM::mov_base32_reg64(&mut self.buf, offset, CC::GENERAL_RETURN_REGS[0]);
                    ASM::mov_base32_reg64(&mut self.buf, offset + 8, CC::GENERAL_RETURN_REGS[1]);
                }
            }
            Layout::Struct([]) => {
                // Nothing needs to be done to load a returned empty struct.
            }
            x => todo!("FnCall: receiving return type, {:?}", x),
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
        let cond_reg = self.load_to_general_reg(cond_symbol);

        let mut ret_jumps = bumpalo::vec![in self.env.arena];
        let mut tmp = bumpalo::vec![in self.env.arena];
        for (val, branch_info, stmt) in branches.iter() {
            tmp.clear();
            if let BranchInfo::None = branch_info {
                // Create jump to next branch if not cond_sym not equal to value.
                // Since we don't know the offset yet, set it to 0 and overwrite later.
                let jne_location = self.buf.len();
                let start_offset = ASM::jne_reg64_imm64_imm32(&mut self.buf, cond_reg, *val, 0);

                // Build all statements in this branch.
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
            } else {
                todo!("Switch: branch info, {:?}", branch_info);
            }
        }
        let (branch_info, stmt) = default_branch;
        if let BranchInfo::None = branch_info {
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
        } else {
            todo!("Switch: branch info, {:?}", branch_info);
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
        // Create jump to remaining.
        let jmp_location = self.buf.len();
        let start_offset = ASM::jmp_imm32(&mut self.buf, 0x1234_5678);

        // This section can essentially be seen as a sub function within the main function.
        // Thus we build using a new backend with some minor extra synchronization.
        {
            let mut sub_backend =
                new_backend_64bit::<GeneralReg, FloatReg, ASM, CC>(self.env, self.interns);
            sub_backend.reset(
                self.proc_name.as_ref().unwrap().clone(),
                self.is_self_recursive.as_ref().unwrap().clone(),
            );
            // Sync static maps of important information.
            sub_backend.last_seen_map = self.last_seen_map.clone();
            sub_backend.layout_map = self.layout_map.clone();
            sub_backend.free_map = self.free_map.clone();

            // Setup join point.
            sub_backend.join_map.insert(*id, 0);
            self.join_map.insert(*id, self.buf.len() as u64);

            // Sync stack size so the "sub function" doesn't mess up our stack.
            sub_backend.stack_size = self.stack_size;
            sub_backend.fn_call_stack_size = self.fn_call_stack_size;

            // Load params as if they were args.
            let mut args = bumpalo::vec![in self.env.arena];
            for param in parameters {
                args.push((param.layout, param.symbol));
            }
            sub_backend.load_args(args.into_bump_slice(), ret_layout);

            // Build all statements in body.
            sub_backend.build_stmt(body, ret_layout);

            // Merge the "sub function" into the main function.
            let sub_func_offset = self.buf.len() as u64;
            self.buf.extend_from_slice(&sub_backend.buf);
            // Update stack based on how much was used by the sub function.
            self.stack_size = sub_backend.stack_size;
            self.fn_call_stack_size = sub_backend.fn_call_stack_size;
            // Relocations must be shifted to be merged correctly.
            self.relocs
                .extend(sub_backend.relocs.into_iter().map(|reloc| match reloc {
                    Relocation::LocalData { offset, data } => Relocation::LocalData {
                        offset: offset + sub_func_offset,
                        data,
                    },
                    Relocation::LinkedData { offset, name } => Relocation::LinkedData {
                        offset: offset + sub_func_offset,
                        name,
                    },
                    Relocation::LinkedFunction { offset, name } => Relocation::LinkedFunction {
                        offset: offset + sub_func_offset,
                        name,
                    },
                    Relocation::JmpToReturn {
                        inst_loc,
                        inst_size,
                        offset,
                    } => Relocation::JmpToReturn {
                        inst_loc: inst_loc + sub_func_offset,
                        inst_size,
                        offset: offset + sub_func_offset,
                    },
                }));
        }
        // Overwrite the original jump with the correct offset.
        let mut tmp = bumpalo::vec![in self.env.arena];
        self.update_jmp_imm32_offset(
            &mut tmp,
            jmp_location as u64,
            start_offset as u64,
            self.buf.len() as u64,
        );

        // Build remainder of function.
        self.build_stmt(remainder, ret_layout)
    }

    fn build_jump(
        &mut self,
        id: &JoinPointId,
        args: &'a [Symbol],
        arg_layouts: &[Layout<'a>],
        ret_layout: &Layout<'a>,
    ) {
        // Treat this like a function call, but with a jump instead of a call instruction at the end.

        self.push_used_caller_saved_regs_to_stack();

        let tmp_stack_size = CC::store_args(
            &mut self.buf,
            &self.symbol_storage_map,
            args,
            arg_layouts,
            ret_layout,
        );
        self.fn_call_stack_size = std::cmp::max(self.fn_call_stack_size, tmp_stack_size);

        let jmp_location = self.buf.len();
        let start_offset = ASM::jmp_imm32(&mut self.buf, 0x1234_5678);

        if let Some(offset) = self.join_map.get(id) {
            let offset = *offset;
            let mut tmp = bumpalo::vec![in self.env.arena];
            self.update_jmp_imm32_offset(
                &mut tmp,
                jmp_location as u64,
                start_offset as u64,
                offset,
            );
        } else {
            internal_error!("Jump: unknown point specified to jump to: {:?}", id);
        }
    }

    fn build_num_abs(&mut self, dst: &Symbol, src: &Symbol, layout: &Layout<'a>) {
        match layout {
            Layout::Builtin(Builtin::Int(IntWidth::I64 | IntWidth::U64)) => {
                let dst_reg = self.claim_general_reg(dst);
                let src_reg = self.load_to_general_reg(src);
                ASM::abs_reg64_reg64(&mut self.buf, dst_reg, src_reg);
            }
            Layout::Builtin(Builtin::Float(FloatWidth::F64)) => {
                let dst_reg = self.claim_float_reg(dst);
                let src_reg = self.load_to_float_reg(src);
                ASM::abs_freg64_freg64(&mut self.buf, &mut self.relocs, dst_reg, src_reg);
            }
            x => todo!("NumAbs: layout, {:?}", x),
        }
    }

    fn build_num_add(&mut self, dst: &Symbol, src1: &Symbol, src2: &Symbol, layout: &Layout<'a>) {
        match layout {
            Layout::Builtin(Builtin::Int(IntWidth::I64 | IntWidth::U64)) => {
                let dst_reg = self.claim_general_reg(dst);
                let src1_reg = self.load_to_general_reg(src1);
                let src2_reg = self.load_to_general_reg(src2);
                ASM::add_reg64_reg64_reg64(&mut self.buf, dst_reg, src1_reg, src2_reg);
            }
            Layout::Builtin(Builtin::Float(FloatWidth::F64)) => {
                let dst_reg = self.claim_float_reg(dst);
                let src1_reg = self.load_to_float_reg(src1);
                let src2_reg = self.load_to_float_reg(src2);
                ASM::add_freg64_freg64_freg64(&mut self.buf, dst_reg, src1_reg, src2_reg);
            }
            x => todo!("NumAdd: layout, {:?}", x),
        }
    }

    fn build_num_mul(&mut self, dst: &Symbol, src1: &Symbol, src2: &Symbol, layout: &Layout<'a>) {
        match layout {
            Layout::Builtin(Builtin::Int(IntWidth::I64 | IntWidth::U64)) => {
                let dst_reg = self.claim_general_reg(dst);
                let src1_reg = self.load_to_general_reg(src1);
                let src2_reg = self.load_to_general_reg(src2);
                ASM::imul_reg64_reg64_reg64(&mut self.buf, dst_reg, src1_reg, src2_reg);
            }
            x => todo!("NumMul: layout, {:?}", x),
        }
    }

    fn build_num_neg(&mut self, dst: &Symbol, src: &Symbol, layout: &Layout<'a>) {
        match layout {
            Layout::Builtin(Builtin::Int(IntWidth::I64 | IntWidth::U64)) => {
                let dst_reg = self.claim_general_reg(dst);
                let src_reg = self.load_to_general_reg(src);
                ASM::neg_reg64_reg64(&mut self.buf, dst_reg, src_reg);
            }
            x => todo!("NumNeg: layout, {:?}", x),
        }
    }

    fn build_num_sub(&mut self, dst: &Symbol, src1: &Symbol, src2: &Symbol, layout: &Layout<'a>) {
        match layout {
            Layout::Builtin(Builtin::Int(IntWidth::I64 | IntWidth::U64)) => {
                let dst_reg = self.claim_general_reg(dst);
                let src1_reg = self.load_to_general_reg(src1);
                let src2_reg = self.load_to_general_reg(src2);
                ASM::sub_reg64_reg64_reg64(&mut self.buf, dst_reg, src1_reg, src2_reg);
            }
            x => todo!("NumSub: layout, {:?}", x),
        }
    }

    fn build_eq(&mut self, dst: &Symbol, src1: &Symbol, src2: &Symbol, arg_layout: &Layout<'a>) {
        match arg_layout {
            Layout::Builtin(Builtin::Int(IntWidth::I64 | IntWidth::U64)) => {
                let dst_reg = self.claim_general_reg(dst);
                let src1_reg = self.load_to_general_reg(src1);
                let src2_reg = self.load_to_general_reg(src2);
                ASM::eq_reg64_reg64_reg64(&mut self.buf, dst_reg, src1_reg, src2_reg);
            }
            x => todo!("NumEq: layout, {:?}", x),
        }
    }

    fn build_neq(&mut self, dst: &Symbol, src1: &Symbol, src2: &Symbol, arg_layout: &Layout<'a>) {
        match arg_layout {
            Layout::Builtin(Builtin::Int(IntWidth::I64 | IntWidth::U64)) => {
                let dst_reg = self.claim_general_reg(dst);
                let src1_reg = self.load_to_general_reg(src1);
                let src2_reg = self.load_to_general_reg(src2);
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
                let dst_reg = self.claim_general_reg(dst);
                let src1_reg = self.load_to_general_reg(src1);
                let src2_reg = self.load_to_general_reg(src2);
                ASM::lt_reg64_reg64_reg64(&mut self.buf, dst_reg, src1_reg, src2_reg);
            }
            x => todo!("NumLt: layout, {:?}", x),
        }
    }

    fn build_num_to_float(
        &mut self,
        dst: &Symbol,
        src: &Symbol,
        arg_layout: &Layout<'a>,
        ret_layout: &Layout<'a>,
    ) {
        let dst_reg = self.claim_float_reg(dst);
        match (arg_layout, ret_layout) {
            (
                Layout::Builtin(Builtin::Int(IntWidth::I32 | IntWidth::I64)),
                Layout::Builtin(Builtin::Float(FloatWidth::F64)),
            ) => {
                let src_reg = self.load_to_general_reg(src);
                ASM::to_float_freg64_reg64(&mut self.buf, dst_reg, src_reg);
            }
            (
                Layout::Builtin(Builtin::Int(IntWidth::I32 | IntWidth::I64)),
                Layout::Builtin(Builtin::Float(FloatWidth::F32)),
            ) => {
                let src_reg = self.load_to_general_reg(src);
                ASM::to_float_freg32_reg64(&mut self.buf, dst_reg, src_reg);
            }
            (
                Layout::Builtin(Builtin::Float(FloatWidth::F64)),
                Layout::Builtin(Builtin::Float(FloatWidth::F32)),
            ) => {
                let src_reg = self.load_to_float_reg(src);
                ASM::to_float_freg32_freg64(&mut self.buf, dst_reg, src_reg);
            }
            (
                Layout::Builtin(Builtin::Float(FloatWidth::F32)),
                Layout::Builtin(Builtin::Float(FloatWidth::F64)),
            ) => {
                let src_reg = self.load_to_float_reg(src);
                ASM::to_float_freg64_freg32(&mut self.buf, dst_reg, src_reg);
            }
            (
                Layout::Builtin(Builtin::Float(FloatWidth::F64)),
                Layout::Builtin(Builtin::Float(FloatWidth::F64)),
            ) => {
                let src_reg = self.load_to_float_reg(src);
                ASM::mov_freg64_freg64(&mut self.buf, dst_reg, src_reg);
            }
            (
                Layout::Builtin(Builtin::Float(FloatWidth::F32)),
                Layout::Builtin(Builtin::Float(FloatWidth::F32)),
            ) => {
                let src_reg = self.load_to_float_reg(src);
                ASM::mov_freg64_freg64(&mut self.buf, dst_reg, src_reg);
            }
            (a, r) => todo!("NumToFloat: layout, arg {:?}, ret {:?}", a, r),
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
            Layout::Builtin(Builtin::Int(IntWidth::I64 | IntWidth::U64)) => {
                let dst_reg = self.claim_general_reg(dst);
                let src1_reg = self.load_to_general_reg(src1);
                let src2_reg = self.load_to_general_reg(src2);
                ASM::gte_reg64_reg64_reg64(&mut self.buf, dst_reg, src1_reg, src2_reg);
            }
            x => todo!("NumGte: layout, {:?}", x),
        }
    }

    fn build_ptr_cast(&mut self, dst: &Symbol, src: &Symbol) {
        // We may not strictly need an instruction here.
        // What's important is to load the value, and for src and dest to have different Layouts.
        // This is used for pointer math in refcounting and for pointer equality
        let dst_reg = self.claim_general_reg(dst);
        let src_reg = self.load_to_general_reg(src);
        ASM::mov_reg64_reg64(&mut self.buf, dst_reg, src_reg);
    }

    fn create_struct(&mut self, sym: &Symbol, layout: &Layout<'a>, fields: &'a [Symbol]) {
        let struct_size = layout.stack_size(TARGET_INFO);

        if let Layout::Struct(field_layouts) = layout {
            if struct_size > 0 {
                let offset = self.claim_stack_size(struct_size);
                self.symbol_storage_map.insert(
                    *sym,
                    SymbolStorage::Base {
                        offset,
                        size: struct_size,
                        owned: true,
                    },
                );

                let mut current_offset = offset;
                for (field, field_layout) in fields.iter().zip(field_layouts.iter()) {
                    self.copy_symbol_to_stack_offset(current_offset, field, field_layout);
                    let field_size = field_layout.stack_size(TARGET_INFO);
                    current_offset += field_size as i32;
                }
            } else {
                self.symbol_storage_map.insert(
                    *sym,
                    SymbolStorage::Base {
                        offset: 0,
                        size: 0,
                        owned: false,
                    },
                );
            }
        } else {
            // This is a single element struct. Just copy the single field to the stack.
            let offset = self.claim_stack_size(struct_size);
            self.symbol_storage_map.insert(
                *sym,
                SymbolStorage::Base {
                    offset,
                    size: struct_size,
                    owned: true,
                },
            );
            self.copy_symbol_to_stack_offset(offset, &fields[0], layout);
        }
    }

    fn load_struct_at_index(
        &mut self,
        sym: &Symbol,
        structure: &Symbol,
        index: u64,
        field_layouts: &'a [Layout<'a>],
    ) {
        if let Some(SymbolStorage::Base { offset, .. }) = self.symbol_storage_map.get(structure) {
            let mut data_offset = *offset;
            for i in 0..index {
                let field_size = field_layouts[i as usize].stack_size(TARGET_INFO);
                data_offset += field_size as i32;
            }
            self.symbol_storage_map.insert(
                *sym,
                SymbolStorage::Base {
                    offset: data_offset,
                    size: field_layouts[index as usize].stack_size(TARGET_INFO),
                    owned: false,
                },
            );
        } else {
            internal_error!("unknown struct: {:?}", structure);
        }
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
                let reg = self.claim_general_reg(sym);
                let val = *x;
                ASM::mov_reg64_imm64(&mut self.buf, reg, val as i64);
            }
            (Literal::Float(x), Layout::Builtin(Builtin::Float(FloatWidth::F64))) => {
                let reg = self.claim_float_reg(sym);
                let val = *x;
                ASM::mov_freg64_imm64(&mut self.buf, &mut self.relocs, reg, val);
            }
            (Literal::Float(x), Layout::Builtin(Builtin::Float(FloatWidth::F32))) => {
                let reg = self.claim_float_reg(sym);
                let val = *x as f32;
                ASM::mov_freg32_imm32(&mut self.buf, &mut self.relocs, reg, val);
            }
            (Literal::Str(x), Layout::Builtin(Builtin::Str)) if x.len() < 16 => {
                // Load small string.
                let reg = self.get_tmp_general_reg();

                let offset = self.claim_stack_size(16);
                self.symbol_storage_map.insert(
                    *sym,
                    SymbolStorage::Base {
                        offset,
                        size: 16,
                        owned: true,
                    },
                );
                let mut bytes = [0; 16];
                bytes[..x.len()].copy_from_slice(x.as_bytes());
                bytes[15] = (x.len() as u8) | 0b1000_0000;

                let mut num_bytes = [0; 8];
                num_bytes.copy_from_slice(&bytes[..8]);
                let num = i64::from_ne_bytes(num_bytes);
                ASM::mov_reg64_imm64(&mut self.buf, reg, num);
                ASM::mov_base32_reg64(&mut self.buf, offset, reg);

                num_bytes.copy_from_slice(&bytes[8..]);
                let num = i64::from_ne_bytes(num_bytes);
                ASM::mov_reg64_imm64(&mut self.buf, reg, num);
                ASM::mov_base32_reg64(&mut self.buf, offset + 8, reg);
            }
            x => todo!("loading literal, {:?}", x),
        }
    }

    fn free_symbol(&mut self, sym: &Symbol) {
        match self.symbol_storage_map.remove(sym) {
            Some(
                SymbolStorage::Base {
                    offset,
                    size,
                    owned: true,
                }
                | SymbolStorage::BaseAndGeneralReg {
                    offset,
                    size,
                    owned: true,
                    ..
                }
                | SymbolStorage::BaseAndFloatReg {
                    offset,
                    size,
                    owned: true,
                    ..
                },
            ) => {
                let loc = (offset, size);
                // Note: this position current points to the offset following the specified location.
                // If loc was inserted at this position, it would shift the data at this position over by 1.
                let pos = self
                    .free_stack_chunks
                    .binary_search(&loc)
                    .unwrap_or_else(|e| e);

                // Check for overlap with previous and next free chunk.
                let merge_with_prev = if pos > 0 {
                    if let Some((prev_offset, prev_size)) = self.free_stack_chunks.get(pos - 1) {
                        let prev_end = *prev_offset + *prev_size as i32;
                        if prev_end > offset {
                            internal_error!("Double free? A previously freed stack location overlaps with the currently freed stack location.");
                        }
                        prev_end == offset
                    } else {
                        false
                    }
                } else {
                    false
                };
                let merge_with_next = if let Some((next_offset, _)) =
                    self.free_stack_chunks.get(pos)
                {
                    let current_end = offset + size as i32;
                    if current_end > *next_offset {
                        internal_error!("Double free? A previously freed stack location overlaps with the currently freed stack location.");
                    }
                    current_end == *next_offset
                } else {
                    false
                };

                match (merge_with_prev, merge_with_next) {
                    (true, true) => {
                        let (prev_offset, prev_size) = self.free_stack_chunks[pos - 1];
                        let (_, next_size) = self.free_stack_chunks[pos];
                        self.free_stack_chunks[pos - 1] =
                            (prev_offset, prev_size + size + next_size);
                        self.free_stack_chunks.remove(pos);
                    }
                    (true, false) => {
                        let (prev_offset, prev_size) = self.free_stack_chunks[pos - 1];
                        self.free_stack_chunks[pos - 1] = (prev_offset, prev_size + size);
                    }
                    (false, true) => {
                        let (_, next_size) = self.free_stack_chunks[pos];
                        self.free_stack_chunks[pos] = (offset, next_size + size);
                    }
                    (false, false) => self.free_stack_chunks.insert(pos, loc),
                }
            }
            Some(_) | None => {}
        }
        for i in 0..self.general_used_regs.len() {
            let (reg, saved_sym) = self.general_used_regs[i];
            if saved_sym == *sym {
                self.general_free_regs.push(reg);
                self.general_used_regs.remove(i);
                break;
            }
        }
        for i in 0..self.float_used_regs.len() {
            let (reg, saved_sym) = self.float_used_regs[i];
            if saved_sym == *sym {
                self.float_free_regs.push(reg);
                self.float_used_regs.remove(i);
                break;
            }
        }
    }

    fn return_symbol(&mut self, sym: &Symbol, layout: &Layout<'a>) {
        let val = self.symbol_storage_map.get(sym);
        match val {
            Some(SymbolStorage::GeneralReg(reg)) if *reg == CC::GENERAL_RETURN_REGS[0] => {}
            Some(SymbolStorage::GeneralReg(reg)) => {
                // If it fits in a general purpose register, just copy it over to.
                // Technically this can be optimized to produce shorter instructions if less than 64bits.
                ASM::mov_reg64_reg64(&mut self.buf, CC::GENERAL_RETURN_REGS[0], *reg);
            }
            Some(SymbolStorage::FloatReg(reg)) if *reg == CC::FLOAT_RETURN_REGS[0] => {}
            Some(SymbolStorage::FloatReg(reg)) => {
                ASM::mov_freg64_freg64(&mut self.buf, CC::FLOAT_RETURN_REGS[0], *reg);
            }
            Some(SymbolStorage::Base { offset, size, .. }) => match layout {
                Layout::Builtin(Builtin::Int(IntWidth::I64 | IntWidth::U64)) => {
                    ASM::mov_reg64_base32(&mut self.buf, CC::GENERAL_RETURN_REGS[0], *offset);
                }
                Layout::Builtin(Builtin::Float(FloatWidth::F64)) => {
                    ASM::mov_freg64_base32(&mut self.buf, CC::FLOAT_RETURN_REGS[0], *offset);
                }
                Layout::Builtin(Builtin::Str) => {
                    if self.symbol_storage_map.contains_key(&Symbol::RET_POINTER) {
                        // This will happen on windows, return via pointer here.
                        todo!("Returning strings via pointer");
                    } else {
                        ASM::mov_reg64_base32(&mut self.buf, CC::GENERAL_RETURN_REGS[0], *offset);
                        ASM::mov_reg64_base32(
                            &mut self.buf,
                            CC::GENERAL_RETURN_REGS[1],
                            *offset + 8,
                        );
                    }
                }
                Layout::Struct(field_layouts) => {
                    let (offset, size) = (*offset, *size);
                    // Nothing to do for empty struct
                    if size > 0 {
                        let ret_reg = if self.symbol_storage_map.contains_key(&Symbol::RET_POINTER)
                        {
                            Some(self.load_to_general_reg(&Symbol::RET_POINTER))
                        } else {
                            None
                        };
                        CC::return_struct(&mut self.buf, offset, size, field_layouts, ret_reg);
                    }
                }
                x => todo!("returning symbol with layout, {:?}", x),
            },
            Some(x) => todo!("returning symbol storage, {:?}", x),
            None if layout == &Layout::Struct(&[]) => {
                // Empty struct is not defined and does nothing.
            }
            None => {
                internal_error!("Unknown return symbol: {:?}", sym);
            }
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
        CC: CallConv<GeneralReg, FloatReg>,
    > Backend64Bit<'a, GeneralReg, FloatReg, ASM, CC>
{
    fn get_tmp_general_reg(&mut self) -> GeneralReg {
        if !self.general_free_regs.is_empty() {
            let free_reg = *self
                .general_free_regs
                .get(self.general_free_regs.len() - 1)
                .unwrap();
            if CC::general_callee_saved(&free_reg) {
                self.general_used_callee_saved_regs.insert(free_reg);
            }
            free_reg
        } else if !self.general_used_regs.is_empty() {
            let (reg, sym) = self.general_used_regs.remove(0);
            self.free_to_stack(&sym);
            reg
        } else {
            internal_error!("completely out of general purpose registers");
        }
    }

    fn claim_general_reg(&mut self, sym: &Symbol) -> GeneralReg {
        let reg = if !self.general_free_regs.is_empty() {
            let free_reg = self.general_free_regs.pop().unwrap();
            if CC::general_callee_saved(&free_reg) {
                self.general_used_callee_saved_regs.insert(free_reg);
            }
            free_reg
        } else if !self.general_used_regs.is_empty() {
            let (reg, sym) = self.general_used_regs.remove(0);
            self.free_to_stack(&sym);
            reg
        } else {
            internal_error!("completely out of general purpose registers");
        };

        self.general_used_regs.push((reg, *sym));
        self.symbol_storage_map
            .insert(*sym, SymbolStorage::GeneralReg(reg));
        reg
    }

    fn claim_float_reg(&mut self, sym: &Symbol) -> FloatReg {
        let reg = if !self.float_free_regs.is_empty() {
            let free_reg = self.float_free_regs.pop().unwrap();
            if CC::float_callee_saved(&free_reg) {
                self.float_used_callee_saved_regs.insert(free_reg);
            }
            free_reg
        } else if !self.float_used_regs.is_empty() {
            let (reg, sym) = self.float_used_regs.remove(0);
            self.free_to_stack(&sym);
            reg
        } else {
            internal_error!("completely out of floating point registers");
        };

        self.float_used_regs.push((reg, *sym));
        self.symbol_storage_map
            .insert(*sym, SymbolStorage::FloatReg(reg));
        reg
    }

    fn load_to_general_reg(&mut self, sym: &Symbol) -> GeneralReg {
        let val = self.symbol_storage_map.remove(sym);
        match val {
            Some(SymbolStorage::GeneralReg(reg)) => {
                self.symbol_storage_map
                    .insert(*sym, SymbolStorage::GeneralReg(reg));
                reg
            }
            Some(SymbolStorage::Base {
                offset,
                size,
                owned,
            }) => {
                let reg = self.claim_general_reg(sym);
                self.symbol_storage_map.insert(
                    *sym,
                    SymbolStorage::BaseAndGeneralReg {
                        reg,
                        offset,
                        size,
                        owned,
                    },
                );
                ASM::mov_reg64_base32(&mut self.buf, reg, offset as i32);
                reg
            }
            Some(SymbolStorage::BaseAndGeneralReg {
                reg,
                offset,
                size,
                owned,
            }) => {
                self.symbol_storage_map.insert(
                    *sym,
                    SymbolStorage::BaseAndGeneralReg {
                        reg,
                        offset,
                        size,
                        owned,
                    },
                );
                reg
            }
            Some(SymbolStorage::FloatReg(_)) | Some(SymbolStorage::BaseAndFloatReg { .. }) => {
                internal_error!("Cannot load floating point symbol into GeneralReg")
            }
            None => internal_error!("Unknown symbol: {}", sym),
        }
    }

    fn load_to_float_reg(&mut self, sym: &Symbol) -> FloatReg {
        let val = self.symbol_storage_map.remove(sym);
        match val {
            Some(SymbolStorage::FloatReg(reg)) => {
                self.symbol_storage_map
                    .insert(*sym, SymbolStorage::FloatReg(reg));
                reg
            }
            Some(SymbolStorage::Base {
                offset,
                size,
                owned,
            }) => {
                let reg = self.claim_float_reg(sym);
                self.symbol_storage_map.insert(
                    *sym,
                    SymbolStorage::BaseAndFloatReg {
                        reg,
                        offset,
                        size,
                        owned,
                    },
                );
                ASM::mov_freg64_base32(&mut self.buf, reg, offset as i32);
                reg
            }
            Some(SymbolStorage::BaseAndFloatReg {
                reg,
                offset,
                size,
                owned,
            }) => {
                self.symbol_storage_map.insert(
                    *sym,
                    SymbolStorage::BaseAndFloatReg {
                        reg,
                        offset,
                        size,
                        owned,
                    },
                );
                reg
            }
            Some(SymbolStorage::GeneralReg(_)) | Some(SymbolStorage::BaseAndGeneralReg { .. }) => {
                internal_error!("Cannot load integer symbol into FloatReg")
            }
            None => internal_error!("Unknown symbol: {}", sym),
        }
    }

    fn free_to_stack(&mut self, sym: &Symbol) {
        let val = self.symbol_storage_map.remove(sym);
        match val {
            Some(SymbolStorage::GeneralReg(reg)) => {
                let offset = self.claim_stack_size(8);
                // For base addressing, use the negative offset - 8.
                ASM::mov_base32_reg64(&mut self.buf, offset, reg);
                self.symbol_storage_map.insert(
                    *sym,
                    SymbolStorage::Base {
                        offset,
                        size: 8,
                        owned: true,
                    },
                );
            }
            Some(SymbolStorage::FloatReg(reg)) => {
                let offset = self.claim_stack_size(8);
                // For base addressing, use the negative offset.
                ASM::mov_base32_freg64(&mut self.buf, offset, reg);
                self.symbol_storage_map.insert(
                    *sym,
                    SymbolStorage::Base {
                        offset,
                        size: 8,
                        owned: true,
                    },
                );
            }
            Some(SymbolStorage::Base {
                offset,
                size,
                owned,
            }) => {
                self.symbol_storage_map.insert(
                    *sym,
                    SymbolStorage::Base {
                        offset,
                        size,
                        owned,
                    },
                );
            }
            Some(SymbolStorage::BaseAndGeneralReg {
                offset,
                size,
                owned,
                ..
            }) => {
                self.symbol_storage_map.insert(
                    *sym,
                    SymbolStorage::Base {
                        offset,
                        size,
                        owned,
                    },
                );
            }
            Some(SymbolStorage::BaseAndFloatReg {
                offset,
                size,
                owned,
                ..
            }) => {
                self.symbol_storage_map.insert(
                    *sym,
                    SymbolStorage::Base {
                        offset,
                        size,
                        owned,
                    },
                );
            }
            None => internal_error!("Unknown symbol: {}", sym),
        }
    }

    /// claim_stack_size claims `amount` bytes from the stack.
    /// This may be free space in the stack or result in increasing the stack size.
    /// It returns base pointer relative offset of the new data.
    fn claim_stack_size(&mut self, amount: u32) -> i32 {
        debug_assert!(amount > 0);
        if let Some(fitting_chunk) = self
            .free_stack_chunks
            .iter()
            .enumerate()
            .filter(|(_, (_, size))| *size >= amount)
            .min_by_key(|(_, (_, size))| size)
        {
            let (pos, (offset, size)) = fitting_chunk;
            let (offset, size) = (*offset, *size);
            if size == amount {
                self.free_stack_chunks.remove(pos);
                offset
            } else {
                let (prev_offset, prev_size) = self.free_stack_chunks[pos];
                self.free_stack_chunks[pos] = (prev_offset + amount as i32, prev_size - amount);
                prev_offset
            }
        } else if let Some(new_size) = self.stack_size.checked_add(amount) {
            // Since stack size is u32, but the max offset is i32, if we pass i32 max, we have overflowed.
            if new_size > i32::MAX as u32 {
                internal_error!("Ran out of stack space");
            } else {
                self.stack_size = new_size;
                -(self.stack_size as i32)
            }
        } else {
            internal_error!("Ran out of stack space");
        }
    }

    fn copy_symbol_to_stack_offset(&mut self, to_offset: i32, sym: &Symbol, layout: &Layout<'a>) {
        match layout {
            Layout::Builtin(Builtin::Int(IntWidth::I64 | IntWidth::U64)) => {
                let reg = self.load_to_general_reg(sym);
                ASM::mov_base32_reg64(&mut self.buf, to_offset, reg);
            }
            Layout::Builtin(Builtin::Float(FloatWidth::F64)) => {
                let reg = self.load_to_float_reg(sym);
                ASM::mov_base32_freg64(&mut self.buf, to_offset, reg);
            }
            Layout::Struct(_) if layout.safe_to_memcpy() => {
                let tmp_reg = self.get_tmp_general_reg();
                if let Some(SymbolStorage::Base {
                    offset: from_offset,
                    size,
                    ..
                }) = self.symbol_storage_map.get(sym)
                {
                    debug_assert_eq!(
                        *size,
                        layout.stack_size(TARGET_INFO),
                        "expected struct to have same size as data being stored in it"
                    );
                    for i in 0..layout.stack_size(TARGET_INFO) as i32 {
                        ASM::mov_reg64_base32(&mut self.buf, tmp_reg, from_offset + i);
                        ASM::mov_base32_reg64(&mut self.buf, to_offset + i, tmp_reg);
                    }
                } else {
                    internal_error!("unknown struct: {:?}", sym);
                }
            }
            x => todo!("copying data to the stack with layout, {:?}", x),
        }
    }

    fn push_used_caller_saved_regs_to_stack(&mut self) {
        let old_general_used_regs = std::mem::replace(
            &mut self.general_used_regs,
            bumpalo::vec![in self.env.arena],
        );
        for (reg, saved_sym) in old_general_used_regs.into_iter() {
            if CC::general_caller_saved(&reg) {
                self.general_free_regs.push(reg);
                self.free_to_stack(&saved_sym);
            } else {
                self.general_used_regs.push((reg, saved_sym));
            }
        }
        let old_float_used_regs =
            std::mem::replace(&mut self.float_used_regs, bumpalo::vec![in self.env.arena]);
        for (reg, saved_sym) in old_float_used_regs.into_iter() {
            if CC::float_caller_saved(&reg) {
                self.float_free_regs.push(reg);
                self.free_to_stack(&saved_sym);
            } else {
                self.float_used_regs.push((reg, saved_sym));
            }
        }
    }

    // Updates a jump instruction to a new offset and returns the number of bytes written.
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
macro_rules! single_register_integers {
    () => {
        Layout::Builtin(
            Builtin::Bool
                | Builtin::Int(
                    IntWidth::I8
                        | IntWidth::I16
                        | IntWidth::I32
                        | IntWidth::I64
                        | IntWidth::U8
                        | IntWidth::U16
                        | IntWidth::U32
                        | IntWidth::U64,
                ),
        ) | Layout::RecursivePointer
    };
}

#[macro_export]
macro_rules! single_register_floats {
    () => {
        Layout::Builtin(Builtin::Float(FloatWidth::F32 | FloatWidth::F64))
    };
}

#[macro_export]
macro_rules! single_register_builtins {
    () => {
        single_register_integers!() | single_register_floats!()
    };
}
