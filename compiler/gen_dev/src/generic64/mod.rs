use crate::{Backend, Env, Relocation};
use bumpalo::collections::Vec;
use roc_collections::all::{ImSet, MutMap, MutSet};
use roc_module::symbol::Symbol;
use roc_mono::ir::{Literal, Stmt};
use std::marker::PhantomData;
use target_lexicon::Triple;

pub mod x86_64;

pub trait CallConv<GPReg> {
    fn gp_param_regs() -> &'static [GPReg];
    fn gp_return_regs() -> &'static [GPReg];
    fn gp_default_free_regs() -> &'static [GPReg];

    // A linear scan of an array may be faster than a set technically.
    // That being said, fastest would likely be a trait based on calling convention/register.
    fn caller_saved_regs() -> ImSet<GPReg>;
    fn callee_saved_regs() -> ImSet<GPReg>;

    fn stack_pointer() -> GPReg;
    fn frame_pointer() -> GPReg;

    fn shadow_space_size() -> u8;
    // It may be worth ignoring the red zone and keeping things simpler.
    fn red_zone_size() -> u8;
}

pub trait Assembler<GPReg> {
    fn add_register64bit_immediate32bit<'a>(buf: &mut Vec<'a, u8>, dst: GPReg, imm: i32);
    fn add_register64bit_register64bit<'a>(buf: &mut Vec<'a, u8>, dst: GPReg, src: GPReg);
    fn cmovl_register64bit_register64bit<'a>(buf: &mut Vec<'a, u8>, dst: GPReg, src: GPReg);
    fn mov_register64bit_immediate32bit<'a>(buf: &mut Vec<'a, u8>, dst: GPReg, imm: i32);
    fn mov_register64bit_immediate64bit<'a>(buf: &mut Vec<'a, u8>, dst: GPReg, imm: i64);
    fn mov_register64bit_register64bit<'a>(buf: &mut Vec<'a, u8>, dst: GPReg, src: GPReg);
    fn mov_register64bit_stackoffset32bit<'a>(buf: &mut Vec<'a, u8>, dst: GPReg, offset: i32);
    fn mov_stackoffset32bit_register64bit<'a>(buf: &mut Vec<'a, u8>, offset: i32, src: GPReg);
    fn neg_register64bit<'a>(buf: &mut Vec<'a, u8>, reg: GPReg);
    fn ret<'a>(buf: &mut Vec<'a, u8>);
    fn sub_register64bit_immediate32bit<'a>(buf: &mut Vec<'a, u8>, dst: GPReg, imm: i32);
    fn pop_register64bit<'a>(buf: &mut Vec<'a, u8>, reg: GPReg);
    fn push_register64bit<'a>(buf: &mut Vec<'a, u8>, reg: GPReg);
}

#[derive(Clone, Debug, PartialEq)]
enum SymbolStorage<GPReg> {
    // These may need layout, but I am not sure.
    // I think whenever a symbol would be used, we specify layout anyways.
    GPRegeg(GPReg),
    Stack(i32),
    StackAndGPRegeg(GPReg, i32),
}

pub trait GPRegTrait: Copy + Eq + std::hash::Hash + std::fmt::Debug + 'static {}

pub struct Backend64Bit<'a, GPReg: GPRegTrait, ASM: Assembler<GPReg>, CC: CallConv<GPReg>> {
    phantom_asm: PhantomData<ASM>,
    phantom_cc: PhantomData<CC>,
    env: &'a Env<'a>,
    buf: Vec<'a, u8>,

    /// leaf_function is true if the only calls this function makes are tail calls.
    /// If that is the case, we can skip emitting the frame pointer and updating the stack.
    leaf_function: bool,

    last_seen_map: MutMap<Symbol, *const Stmt<'a>>,
    free_map: MutMap<*const Stmt<'a>, Vec<'a, Symbol>>,
    symbols_map: MutMap<Symbol, SymbolStorage<GPReg>>,
    literal_map: MutMap<Symbol, Literal<'a>>,

    // This should probably be smarter than a vec.
    // There are certain registers we should always use first. With pushing and popping, this could get mixed.
    gp_free_regs: Vec<'a, GPReg>,

    // The last major thing we need is a way to decide what reg to free when all of them are full.
    // Theoretically we want a basic lru cache for the currently loaded symbols.
    // For now just a vec of used registers and the symbols they contain.
    gp_used_regs: Vec<'a, (GPReg, Symbol)>,

    stack_size: i32,

    // used callee saved regs must be tracked for pushing and popping at the beginning/end of the function.
    used_callee_saved_regs: MutSet<GPReg>,
}

impl<'a, GPReg: GPRegTrait, ASM: Assembler<GPReg>, CC: CallConv<GPReg>> Backend<'a>
    for Backend64Bit<'a, GPReg, ASM, CC>
{
    fn new(env: &'a Env, _target: &Triple) -> Result<Self, String> {
        Ok(Backend64Bit {
            phantom_asm: PhantomData,
            phantom_cc: PhantomData,
            env,
            leaf_function: true,
            buf: bumpalo::vec!(in env.arena),
            last_seen_map: MutMap::default(),
            free_map: MutMap::default(),
            symbols_map: MutMap::default(),
            literal_map: MutMap::default(),
            gp_free_regs: bumpalo::vec![in env.arena],
            gp_used_regs: bumpalo::vec![in env.arena],
            stack_size: 0,
            used_callee_saved_regs: MutSet::default(),
        })
    }

    fn env(&self) -> &'a Env<'a> {
        self.env
    }

    fn reset(&mut self) {
        self.stack_size = -(CC::red_zone_size() as i32);
        self.leaf_function = true;
        self.last_seen_map.clear();
        self.free_map.clear();
        self.symbols_map.clear();
        self.buf.clear();
        self.used_callee_saved_regs.clear();
        self.gp_free_regs.clear();
        self.gp_used_regs.clear();
        self.gp_free_regs
            .extend_from_slice(CC::gp_default_free_regs());
    }

    fn set_not_leaf_function(&mut self) {
        self.leaf_function = false;
        // If this is not a leaf function, it can't use the shadow space.
        self.stack_size = CC::shadow_space_size() as i32 - CC::red_zone_size() as i32;
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

    fn finalize(&mut self) -> Result<(&'a [u8], &[Relocation]), String> {
        let mut out = bumpalo::vec![in self.env.arena];

        if !self.leaf_function {
            // I believe that this will have to move away from push and to mov to be generic across backends.
            ASM::push_register64bit(&mut out, CC::frame_pointer());
            ASM::mov_register64bit_register64bit(
                &mut out,
                CC::frame_pointer(),
                CC::stack_pointer(),
            );
        }
        // Save data in all callee saved regs.
        let mut pop_order = bumpalo::vec![in self.env.arena];
        for reg in &self.used_callee_saved_regs {
            ASM::push_register64bit(&mut out, *reg);
            pop_order.push(*reg);
        }
        if self.stack_size > 0 {
            ASM::sub_register64bit_immediate32bit(&mut out, CC::stack_pointer(), self.stack_size);
        }

        // Add function body.
        out.extend(&self.buf);

        if self.stack_size > 0 {
            ASM::add_register64bit_immediate32bit(&mut out, CC::stack_pointer(), self.stack_size);
        }
        // Restore data in callee saved regs.
        while let Some(reg) = pop_order.pop() {
            ASM::pop_register64bit(&mut out, reg);
        }
        if !self.leaf_function {
            ASM::pop_register64bit(&mut out, CC::frame_pointer());
        }
        ASM::ret(&mut out);

        Ok((out.into_bump_slice(), &[]))
    }

    fn build_num_abs_i64(&mut self, dst: &Symbol, src: &Symbol) -> Result<(), String> {
        let dst_reg = self.claim_gp_reg(dst)?;
        let src_reg = self.load_to_reg(src)?;
        ASM::mov_register64bit_register64bit(&mut self.buf, dst_reg, src_reg);
        ASM::neg_register64bit(&mut self.buf, dst_reg);
        ASM::cmovl_register64bit_register64bit(&mut self.buf, dst_reg, src_reg);
        Ok(())
    }

    fn build_num_add_i64(
        &mut self,
        dst: &Symbol,
        src1: &Symbol,
        src2: &Symbol,
    ) -> Result<(), String> {
        let dst_reg = self.claim_gp_reg(dst)?;
        let src1_reg = self.load_to_reg(src1)?;
        ASM::mov_register64bit_register64bit(&mut self.buf, dst_reg, src1_reg);
        let src2_reg = self.load_to_reg(src2)?;
        ASM::add_register64bit_register64bit(&mut self.buf, dst_reg, src2_reg);
        Ok(())
    }

    fn load_literal(&mut self, sym: &Symbol, lit: &Literal<'a>) -> Result<(), String> {
        match lit {
            Literal::Int(x) => {
                let reg = self.claim_gp_reg(sym)?;
                let val = *x;
                ASM::mov_register64bit_immediate64bit(&mut self.buf, reg, val);
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
            Some(SymbolStorage::GPRegeg(reg)) if *reg == CC::gp_return_regs()[0] => Ok(()),
            Some(SymbolStorage::GPRegeg(reg)) => {
                // If it fits in a general purpose register, just copy it over to.
                // Technically this can be optimized to produce shorter instructions if less than 64bits.
                ASM::mov_register64bit_register64bit(&mut self.buf, CC::gp_return_regs()[0], *reg);
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
impl<'a, GPReg: GPRegTrait, ASM: Assembler<GPReg>, CC: CallConv<GPReg>>
    Backend64Bit<'a, GPReg, ASM, CC>
{
    fn claim_gp_reg(&mut self, sym: &Symbol) -> Result<GPReg, String> {
        let reg = if !self.gp_free_regs.is_empty() {
            let free_reg = self.gp_free_regs.pop().unwrap();
            if CC::callee_saved_regs().contains(&free_reg) {
                self.used_callee_saved_regs.insert(free_reg);
            }
            Ok(free_reg)
        } else if !self.gp_used_regs.is_empty() {
            let (reg, sym) = self.gp_used_regs.remove(0);
            self.free_to_stack(&sym)?;
            Ok(reg)
        } else {
            Err("completely out of registers".to_string())
        }?;

        self.gp_used_regs.push((reg, *sym));
        self.symbols_map.insert(*sym, SymbolStorage::GPRegeg(reg));
        Ok(reg)
    }

    fn load_to_reg(&mut self, sym: &Symbol) -> Result<GPReg, String> {
        let val = self.symbols_map.remove(sym);
        match val {
            Some(SymbolStorage::GPRegeg(reg)) => {
                self.symbols_map.insert(*sym, SymbolStorage::GPRegeg(reg));
                Ok(reg)
            }
            Some(SymbolStorage::StackAndGPRegeg(reg, offset)) => {
                self.symbols_map
                    .insert(*sym, SymbolStorage::StackAndGPRegeg(reg, offset));
                Ok(reg)
            }
            Some(SymbolStorage::Stack(offset)) => {
                let reg = self.claim_gp_reg(sym)?;
                self.symbols_map
                    .insert(*sym, SymbolStorage::StackAndGPRegeg(reg, offset));
                ASM::mov_register64bit_stackoffset32bit(&mut self.buf, reg, offset as i32);
                Ok(reg)
            }
            None => Err(format!("Unknown symbol: {}", sym)),
        }
    }

    fn free_to_stack(&mut self, sym: &Symbol) -> Result<(), String> {
        let val = self.symbols_map.remove(sym);
        match val {
            Some(SymbolStorage::GPRegeg(reg)) => {
                let offset = self.stack_size;
                self.stack_size += 8;
                if let Some(size) = self.stack_size.checked_add(8) {
                    self.stack_size = size;
                } else {
                    return Err(format!(
                        "Ran out of stack space while saving symbol: {}",
                        sym
                    ));
                }
                ASM::mov_stackoffset32bit_register64bit(&mut self.buf, offset as i32, reg);
                self.symbols_map
                    .insert(*sym, SymbolStorage::Stack(offset as i32));
                Ok(())
            }
            Some(SymbolStorage::StackAndGPRegeg(_, offset)) => {
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
}
