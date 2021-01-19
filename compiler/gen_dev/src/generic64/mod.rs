use crate::{Backend, Env, Relocation};
use bumpalo::collections::Vec;
use roc_collections::all::{MutMap, MutSet};
use roc_module::symbol::Symbol;
use roc_mono::ir::{Literal, Stmt};
use std::marker::PhantomData;
use target_lexicon::Triple;

pub mod aarch64;
pub mod x86_64;

pub trait CallConv<GPReg: RegTrait, FPReg: RegTrait> {
    const GP_PARAM_REGS: &'static [GPReg];
    const GP_RETURN_REGS: &'static [GPReg];
    const GP_DEFAULT_FREE_REGS: &'static [GPReg];

    const FP_PARAM_REGS: &'static [FPReg];
    const FP_RETURN_REGS: &'static [FPReg];
    const FP_DEFAULT_FREE_REGS: &'static [FPReg];

    const SHADOW_SPACE_SIZE: u8;

    fn gp_callee_saved(reg: &GPReg) -> bool;
    #[inline(always)]
    fn gp_caller_saved(reg: &GPReg) -> bool {
        !Self::gp_callee_saved(reg)
    }
    fn fp_callee_saved(reg: &FPReg) -> bool;
    #[inline(always)]
    fn fp_caller_saved(reg: &FPReg) -> bool {
        !Self::fp_callee_saved(reg)
    }

    fn setup_stack<'a>(
        buf: &mut Vec<'a, u8>,
        leaf_function: bool,
        gp_saved_regs: &[GPReg],
        requested_stack_size: i32,
    ) -> Result<i32, String>;
    fn cleanup_stack<'a>(
        buf: &mut Vec<'a, u8>,
        leaf_function: bool,
        gp_saved_regs: &[GPReg],
        aligned_stack_size: i32,
    ) -> Result<(), String>;
}

/// Assembler contains calls to the backend assembly generator.
/// These calls do not necessarily map directly to a single assembly instruction.
/// They are higher level in cases where an instruction would not be common and shared between multiple architectures.
/// Thus, some backends will need to use mulitiple instructions to preform a single one of this calls.
/// Generally, I prefer explicit sources, as opposed to dst being one of the sources. Ex: `x = x + y` would be `add x, x, y` instead of `add x, y`.
/// dst should always come before sources.
pub trait Assembler<GPReg: RegTrait, FPReg: RegTrait> {
    fn abs_reg64_reg64(buf: &mut Vec<'_, u8>, dst: GPReg, src: GPReg);
    fn add_reg64_reg64_imm32(buf: &mut Vec<'_, u8>, dst: GPReg, src1: GPReg, imm32: i32);
    fn add_freg64_freg64_freg64(buf: &mut Vec<'_, u8>, dst: FPReg, src1: FPReg, src2: FPReg);
    fn add_reg64_reg64_reg64(buf: &mut Vec<'_, u8>, dst: GPReg, src1: GPReg, src2: GPReg);
    fn mov_freg64_imm64(
        buf: &mut Vec<'_, u8>,
        relocs: &mut Vec<'_, Relocation>,
        dst: FPReg,
        imm: f64,
    );
    fn mov_reg64_imm64(buf: &mut Vec<'_, u8>, dst: GPReg, imm: i64);
    fn mov_freg64_freg64(buf: &mut Vec<'_, u8>, dst: FPReg, src: FPReg);
    fn mov_reg64_reg64(buf: &mut Vec<'_, u8>, dst: GPReg, src: GPReg);
    fn mov_freg64_stack32(buf: &mut Vec<'_, u8>, dst: FPReg, offset: i32);
    fn mov_reg64_stack32(buf: &mut Vec<'_, u8>, dst: GPReg, offset: i32);
    fn mov_stack32_freg64(buf: &mut Vec<'_, u8>, offset: i32, src: FPReg);
    fn mov_stack32_reg64(buf: &mut Vec<'_, u8>, offset: i32, src: GPReg);
    fn sub_reg64_reg64_imm32(buf: &mut Vec<'_, u8>, dst: GPReg, src1: GPReg, imm32: i32);
    fn sub_reg64_reg64_reg64(buf: &mut Vec<'_, u8>, dst: GPReg, src1: GPReg, src2: GPReg);
    fn ret(buf: &mut Vec<'_, u8>);
}

#[derive(Clone, Debug, PartialEq)]
#[allow(dead_code)]
enum SymbolStorage<GPReg: RegTrait, FPReg: RegTrait> {
    // These may need layout, but I am not sure.
    // I think whenever a symbol would be used, we specify layout anyways.
    GPReg(GPReg),
    FPReg(FPReg),
    Stack(i32),
    StackAndGPReg(GPReg, i32),
    StackAndFPReg(FPReg, i32),
}

pub trait RegTrait: Copy + Eq + std::hash::Hash + std::fmt::Debug + 'static {}

pub struct Backend64Bit<
    'a,
    GPReg: RegTrait,
    FPReg: RegTrait,
    ASM: Assembler<GPReg, FPReg>,
    CC: CallConv<GPReg, FPReg>,
> {
    phantom_asm: PhantomData<ASM>,
    phantom_cc: PhantomData<CC>,
    env: &'a Env<'a>,
    buf: Vec<'a, u8>,
    relocs: Vec<'a, Relocation<'a>>,

    /// leaf_function is true if the only calls this function makes are tail calls.
    /// If that is the case, we can skip emitting the frame pointer and updating the stack.
    leaf_function: bool,

    last_seen_map: MutMap<Symbol, *const Stmt<'a>>,
    free_map: MutMap<*const Stmt<'a>, Vec<'a, Symbol>>,
    symbols_map: MutMap<Symbol, SymbolStorage<GPReg, FPReg>>,
    literal_map: MutMap<Symbol, Literal<'a>>,

    // This should probably be smarter than a vec.
    // There are certain registers we should always use first. With pushing and popping, this could get mixed.
    gp_free_regs: Vec<'a, GPReg>,
    fp_free_regs: Vec<'a, FPReg>,

    // The last major thing we need is a way to decide what reg to free when all of them are full.
    // Theoretically we want a basic lru cache for the currently loaded symbols.
    // For now just a vec of used registers and the symbols they contain.
    gp_used_regs: Vec<'a, (GPReg, Symbol)>,
    fp_used_regs: Vec<'a, (FPReg, Symbol)>,

    // used callee saved regs must be tracked for pushing and popping at the beginning/end of the function.
    gp_used_callee_saved_regs: MutSet<GPReg>,
    fp_used_callee_saved_regs: MutSet<FPReg>,

    stack_size: i32,
}

impl<
        'a,
        GPReg: RegTrait,
        FPReg: RegTrait,
        ASM: Assembler<GPReg, FPReg>,
        CC: CallConv<GPReg, FPReg>,
    > Backend<'a> for Backend64Bit<'a, GPReg, FPReg, ASM, CC>
{
    fn new(env: &'a Env, _target: &Triple) -> Result<Self, String> {
        Ok(Backend64Bit {
            phantom_asm: PhantomData,
            phantom_cc: PhantomData,
            env,
            leaf_function: true,
            buf: bumpalo::vec!(in env.arena),
            relocs: bumpalo::vec!(in env.arena),
            last_seen_map: MutMap::default(),
            free_map: MutMap::default(),
            symbols_map: MutMap::default(),
            literal_map: MutMap::default(),
            gp_free_regs: bumpalo::vec![in env.arena],
            gp_used_regs: bumpalo::vec![in env.arena],
            gp_used_callee_saved_regs: MutSet::default(),
            fp_free_regs: bumpalo::vec![in env.arena],
            fp_used_regs: bumpalo::vec![in env.arena],
            fp_used_callee_saved_regs: MutSet::default(),
            stack_size: 0,
        })
    }

    fn env(&self) -> &'a Env<'a> {
        self.env
    }

    fn reset(&mut self) {
        self.stack_size = 0;
        self.leaf_function = true;
        self.last_seen_map.clear();
        self.free_map.clear();
        self.symbols_map.clear();
        self.buf.clear();
        self.gp_used_callee_saved_regs.clear();
        self.gp_free_regs.clear();
        self.gp_used_regs.clear();
        self.gp_free_regs
            .extend_from_slice(CC::GP_DEFAULT_FREE_REGS);
        self.fp_used_callee_saved_regs.clear();
        self.fp_free_regs.clear();
        self.fp_used_regs.clear();
        self.fp_free_regs
            .extend_from_slice(CC::FP_DEFAULT_FREE_REGS);
    }

    fn set_not_leaf_function(&mut self) {
        self.leaf_function = false;
        self.stack_size = CC::SHADOW_SPACE_SIZE as i32;
    }

    fn literal_map(&mut self) -> &mut MutMap<Symbol, Literal<'a>> {
        &mut self.literal_map
    }

    fn last_seen_map(&mut self) -> &mut MutMap<Symbol, *const Stmt<'a>> {
        &mut self.last_seen_map
    }

    fn set_free_map(&mut self, map: MutMap<*const Stmt<'a>, Vec<'a, Symbol>>) {
        self.free_map = map;
    }

    fn free_map(&mut self) -> &mut MutMap<*const Stmt<'a>, Vec<'a, Symbol>> {
        &mut self.free_map
    }

    fn finalize(&mut self) -> Result<(&'a [u8], &[&Relocation]), String> {
        let mut out = bumpalo::vec![in self.env.arena];

        // Setup stack.
        let mut used_regs = bumpalo::vec![in self.env.arena];
        used_regs.extend(&self.gp_used_callee_saved_regs);
        let aligned_stack_size =
            CC::setup_stack(&mut out, self.leaf_function, &used_regs, self.stack_size)?;

        // Add function body.
        out.extend(&self.buf);

        // Cleanup stack.
        CC::cleanup_stack(&mut out, self.leaf_function, &used_regs, aligned_stack_size)?;
        ASM::ret(&mut out);

        let mut out_relocs = bumpalo::vec![in self.env.arena];
        out_relocs.extend(&self.relocs);
        Ok((out.into_bump_slice(), out_relocs.into_bump_slice()))
    }

    fn build_num_abs_i64(&mut self, dst: &Symbol, src: &Symbol) -> Result<(), String> {
        let dst_reg = self.claim_gpreg(dst)?;
        let src_reg = self.load_to_gpreg(src)?;
        ASM::abs_reg64_reg64(&mut self.buf, dst_reg, src_reg);
        Ok(())
    }

    fn build_num_add_i64(
        &mut self,
        dst: &Symbol,
        src1: &Symbol,
        src2: &Symbol,
    ) -> Result<(), String> {
        let dst_reg = self.claim_gpreg(dst)?;
        let src1_reg = self.load_to_gpreg(src1)?;
        let src2_reg = self.load_to_gpreg(src2)?;
        ASM::add_reg64_reg64_reg64(&mut self.buf, dst_reg, src1_reg, src2_reg);
        Ok(())
    }

    fn build_num_add_f64(
        &mut self,
        dst: &Symbol,
        src1: &Symbol,
        src2: &Symbol,
    ) -> Result<(), String> {
        let dst_reg = self.claim_fpreg(dst)?;
        let src1_reg = self.load_to_fpreg(src1)?;
        let src2_reg = self.load_to_fpreg(src2)?;
        ASM::add_freg64_freg64_freg64(&mut self.buf, dst_reg, src1_reg, src2_reg);
        Ok(())
    }

    fn build_num_sub_i64(
        &mut self,
        dst: &Symbol,
        src1: &Symbol,
        src2: &Symbol,
    ) -> Result<(), String> {
        let dst_reg = self.claim_gpreg(dst)?;
        let src1_reg = self.load_to_gpreg(src1)?;
        let src2_reg = self.load_to_gpreg(src2)?;
        ASM::sub_reg64_reg64_reg64(&mut self.buf, dst_reg, src1_reg, src2_reg);
        Ok(())
    }

    fn load_literal(&mut self, sym: &Symbol, lit: &Literal<'a>) -> Result<(), String> {
        match lit {
            Literal::Int(x) => {
                let reg = self.claim_gpreg(sym)?;
                let val = *x;
                ASM::mov_reg64_imm64(&mut self.buf, reg, val);
                Ok(())
            }
            Literal::Float(x) => {
                let reg = self.claim_fpreg(sym)?;
                let val = *x;
                ASM::mov_freg64_imm64(&mut self.buf, &mut self.relocs, reg, val);
                Ok(())
            }
            x => Err(format!("loading literal, {:?}, is not yet implemented", x)),
        }
    }

    fn free_symbol(&mut self, sym: &Symbol) {
        self.symbols_map.remove(sym);
        for i in 0..self.gp_used_regs.len() {
            let (reg, saved_sym) = self.gp_used_regs[i];
            if saved_sym == *sym {
                self.gp_free_regs.push(reg);
                self.gp_used_regs.remove(i);
                break;
            }
        }
    }

    fn return_symbol(&mut self, sym: &Symbol) -> Result<(), String> {
        let val = self.symbols_map.get(sym);
        match val {
            Some(SymbolStorage::GPReg(reg)) if *reg == CC::GP_RETURN_REGS[0] => Ok(()),
            Some(SymbolStorage::GPReg(reg)) => {
                // If it fits in a general purpose register, just copy it over to.
                // Technically this can be optimized to produce shorter instructions if less than 64bits.
                ASM::mov_reg64_reg64(&mut self.buf, CC::GP_RETURN_REGS[0], *reg);
                Ok(())
            }
            Some(SymbolStorage::FPReg(reg)) if *reg == CC::FP_RETURN_REGS[0] => Ok(()),
            Some(SymbolStorage::FPReg(reg)) => {
                ASM::mov_freg64_freg64(&mut self.buf, CC::FP_RETURN_REGS[0], *reg);
                Ok(())
            }
            Some(x) => Err(format!(
                "returning symbol storage, {:?}, is not yet implemented",
                x
            )),
            None => Err(format!("Unknown return symbol: {}", sym)),
        }
    }
}

/// This impl block is for ir related instructions that need backend specific information.
/// For example, loading a symbol for doing a computation.
impl<
        'a,
        FPReg: RegTrait,
        GPReg: RegTrait,
        ASM: Assembler<GPReg, FPReg>,
        CC: CallConv<GPReg, FPReg>,
    > Backend64Bit<'a, GPReg, FPReg, ASM, CC>
{
    fn claim_gpreg(&mut self, sym: &Symbol) -> Result<GPReg, String> {
        let reg = if !self.gp_free_regs.is_empty() {
            let free_reg = self.gp_free_regs.pop().unwrap();
            if CC::gp_callee_saved(&free_reg) {
                self.gp_used_callee_saved_regs.insert(free_reg);
            }
            Ok(free_reg)
        } else if !self.gp_used_regs.is_empty() {
            let (reg, sym) = self.gp_used_regs.remove(0);
            self.free_to_stack(&sym)?;
            Ok(reg)
        } else {
            Err("completely out of general purpose registers".to_string())
        }?;

        self.gp_used_regs.push((reg, *sym));
        self.symbols_map.insert(*sym, SymbolStorage::GPReg(reg));
        Ok(reg)
    }

    fn claim_fpreg(&mut self, sym: &Symbol) -> Result<FPReg, String> {
        let reg = if !self.fp_free_regs.is_empty() {
            let free_reg = self.fp_free_regs.pop().unwrap();
            if CC::fp_callee_saved(&free_reg) {
                self.fp_used_callee_saved_regs.insert(free_reg);
            }
            Ok(free_reg)
        } else if !self.fp_used_regs.is_empty() {
            let (reg, sym) = self.fp_used_regs.remove(0);
            self.free_to_stack(&sym)?;
            Ok(reg)
        } else {
            Err("completely out of floating point registers".to_string())
        }?;

        self.fp_used_regs.push((reg, *sym));
        self.symbols_map.insert(*sym, SymbolStorage::FPReg(reg));
        Ok(reg)
    }

    fn load_to_gpreg(&mut self, sym: &Symbol) -> Result<GPReg, String> {
        let val = self.symbols_map.remove(sym);
        match val {
            Some(SymbolStorage::GPReg(reg)) => {
                self.symbols_map.insert(*sym, SymbolStorage::GPReg(reg));
                Ok(reg)
            }
            Some(SymbolStorage::FPReg(_reg)) => {
                Err("Cannot load floating point symbol into GPReg".to_string())
            }
            Some(SymbolStorage::StackAndGPReg(reg, offset)) => {
                self.symbols_map
                    .insert(*sym, SymbolStorage::StackAndGPReg(reg, offset));
                Ok(reg)
            }
            Some(SymbolStorage::StackAndFPReg(_reg, _offset)) => {
                Err("Cannot load floating point symbol into GPReg".to_string())
            }
            Some(SymbolStorage::Stack(offset)) => {
                let reg = self.claim_gpreg(sym)?;
                self.symbols_map
                    .insert(*sym, SymbolStorage::StackAndGPReg(reg, offset));
                ASM::mov_reg64_stack32(&mut self.buf, reg, offset as i32);
                Ok(reg)
            }
            None => Err(format!("Unknown symbol: {}", sym)),
        }
    }

    fn load_to_fpreg(&mut self, sym: &Symbol) -> Result<FPReg, String> {
        let val = self.symbols_map.remove(sym);
        match val {
            Some(SymbolStorage::GPReg(_reg)) => {
                Err("Cannot load integer point symbol into FPReg".to_string())
            }
            Some(SymbolStorage::FPReg(reg)) => {
                self.symbols_map.insert(*sym, SymbolStorage::FPReg(reg));
                Ok(reg)
            }
            Some(SymbolStorage::StackAndGPReg(_reg, _offset)) => {
                Err("Cannot load integer point symbol into FPReg".to_string())
            }
            Some(SymbolStorage::StackAndFPReg(reg, offset)) => {
                self.symbols_map
                    .insert(*sym, SymbolStorage::StackAndFPReg(reg, offset));
                Ok(reg)
            }
            Some(SymbolStorage::Stack(offset)) => {
                let reg = self.claim_fpreg(sym)?;
                self.symbols_map
                    .insert(*sym, SymbolStorage::StackAndFPReg(reg, offset));
                ASM::mov_freg64_stack32(&mut self.buf, reg, offset as i32);
                Ok(reg)
            }
            None => Err(format!("Unknown symbol: {}", sym)),
        }
    }

    fn free_to_stack(&mut self, sym: &Symbol) -> Result<(), String> {
        let val = self.symbols_map.remove(sym);
        match val {
            Some(SymbolStorage::GPReg(reg)) => {
                let offset = self.increase_stack_size(8)?;
                ASM::mov_stack32_reg64(&mut self.buf, offset as i32, reg);
                self.symbols_map.insert(*sym, SymbolStorage::Stack(offset));
                Ok(())
            }
            Some(SymbolStorage::FPReg(reg)) => {
                let offset = self.increase_stack_size(8)?;
                ASM::mov_stack32_freg64(&mut self.buf, offset as i32, reg);
                self.symbols_map.insert(*sym, SymbolStorage::Stack(offset));
                Ok(())
            }
            Some(SymbolStorage::StackAndGPReg(_, offset)) => {
                self.symbols_map.insert(*sym, SymbolStorage::Stack(offset));
                Ok(())
            }
            Some(SymbolStorage::StackAndFPReg(_, offset)) => {
                self.symbols_map.insert(*sym, SymbolStorage::Stack(offset));
                Ok(())
            }
            Some(SymbolStorage::Stack(offset)) => {
                self.symbols_map.insert(*sym, SymbolStorage::Stack(offset));
                Ok(())
            }
            None => Err(format!("Unknown symbol: {}", sym)),
        }
    }

    /// increase_stack_size increase the current stack size and returns the offset of the stack.
    fn increase_stack_size(&mut self, amount: i32) -> Result<i32, String> {
        debug_assert!(amount > 0);
        let offset = self.stack_size;
        if let Some(new_size) = self.stack_size.checked_add(amount) {
            self.stack_size = new_size;
            Ok(offset)
        } else {
            Err("Ran out of stack space".to_string())
        }
    }
}
