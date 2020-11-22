use crate::{Backend, Env, Relocation};
use bumpalo::collections::Vec;
use roc_collections::all::{ImSet, MutMap, MutSet};
use roc_module::symbol::Symbol;
use roc_mono::ir::{Literal, Stmt};
use target_lexicon::{CallingConvention, Triple};

mod asm;
use asm::GPReg;

#[derive(Clone, Debug, PartialEq)]
enum SymbolStorage {
    // These may need layout, but I am not sure.
    // I think whenever a symbol would be used, we specify layout anyways.
    GPReg(GPReg),
    Stack(i32),
    StackAndGPReg(GPReg, i32),
}

pub struct X86_64Backend<'a> {
    env: &'a Env<'a>,
    buf: Vec<'a, u8>,

    /// leaf_function is true if the only calls this function makes are tail calls.
    /// If that is the case, we can skip emitting the frame pointer and updating the stack.
    leaf_function: bool,

    last_seen_map: MutMap<Symbol, *const Stmt<'a>>,
    free_map: MutMap<*const Stmt<'a>, Vec<'a, Symbol>>,
    symbols_map: MutMap<Symbol, SymbolStorage>,
    literal_map: MutMap<Symbol, Literal<'a>>,

    gp_param_regs: &'static [GPReg],
    gp_return_regs: &'static [GPReg],

    // This should probably be smarter than a vec.
    // There are certain registers we should always use first. With pushing and poping, this could get mixed.
    gp_default_free_regs: &'static [GPReg],
    gp_free_regs: Vec<'a, GPReg>,

    // The last major thing we need is a way to decide what reg to free when all of them are full.
    // Theoretically we want a basic lru cache for the currently loaded symbols.
    // For now just a vec of used registers and the symbols they contain.
    gp_used_regs: Vec<'a, (GPReg, Symbol)>,

    stack_size: i32,
    shadow_space_size: u8,
    red_zone_size: u8,

    // A linear scan of an array may be faster than a set technically.
    // That being said, fastest would likely be a trait based on calling convention/register.
    caller_saved_regs: ImSet<GPReg>,
    callee_saved_regs: ImSet<GPReg>,

    // used callee saved regs must be tracked for pushing and popping at the beginning/end of the function.
    used_callee_saved_regs: MutSet<GPReg>,
}

impl<'a> Backend<'a> for X86_64Backend<'a> {
    fn new(env: &'a Env, target: &Triple) -> Result<Self, String> {
        match target.default_calling_convention() {
            Ok(CallingConvention::SystemV) => Ok(X86_64Backend {
                env,
                leaf_function: true,
                buf: bumpalo::vec!(in env.arena),
                last_seen_map: MutMap::default(),
                free_map: MutMap::default(),
                symbols_map: MutMap::default(),
                literal_map: MutMap::default(),
                gp_param_regs: &[
                    GPReg::RDI,
                    GPReg::RSI,
                    GPReg::RDX,
                    GPReg::RCX,
                    GPReg::R8,
                    GPReg::R9,
                ],
                gp_return_regs: &[GPReg::RAX, GPReg::RDX],
                gp_default_free_regs: &[
                    // The regs we want to use first should be at the end of this vec.
                    // We will use pop to get which reg to use next
                    // Use callee saved regs last.
                    GPReg::RBX,
                    // Don't use frame pointer: GPReg::RBP,
                    GPReg::R12,
                    GPReg::R13,
                    GPReg::R14,
                    GPReg::R15,
                    // Use caller saved regs first.
                    GPReg::RAX,
                    GPReg::RCX,
                    GPReg::RDX,
                    // Don't use stack pionter: GPReg::RSP,
                    GPReg::RSI,
                    GPReg::RDI,
                    GPReg::R8,
                    GPReg::R9,
                    GPReg::R10,
                    GPReg::R11,
                ],
                gp_free_regs: bumpalo::vec![in env.arena],
                gp_used_regs: bumpalo::vec![in env.arena],
                stack_size: 0,
                shadow_space_size: 0,
                red_zone_size: 128,
                // TODO: stop using vec! here. I was just have trouble with some errors, but it shouldn't be needed.
                caller_saved_regs: ImSet::from(vec![
                    GPReg::RAX,
                    GPReg::RCX,
                    GPReg::RDX,
                    GPReg::RSP,
                    GPReg::RSI,
                    GPReg::RDI,
                    GPReg::R8,
                    GPReg::R9,
                    GPReg::R10,
                    GPReg::R11,
                ]),
                callee_saved_regs: ImSet::from(vec![
                    GPReg::RBX,
                    GPReg::RBP,
                    GPReg::R12,
                    GPReg::R13,
                    GPReg::R14,
                    GPReg::R15,
                ]),
                used_callee_saved_regs: MutSet::default(),
            }),
            Ok(CallingConvention::WindowsFastcall) => Ok(X86_64Backend {
                env,
                leaf_function: true,
                buf: bumpalo::vec!(in env.arena),
                last_seen_map: MutMap::default(),
                free_map: MutMap::default(),
                symbols_map: MutMap::default(),
                literal_map: MutMap::default(),
                gp_param_regs: &[GPReg::RCX, GPReg::RDX, GPReg::R8, GPReg::R9],
                gp_return_regs: &[GPReg::RAX],
                gp_default_free_regs: &[
                    // The regs we want to use first should be at the end of this vec.
                    // We will use pop to get which reg to use next
                    // Use callee saved regs last.
                    GPReg::RBX,
                    // Don't use frame pointer: GPReg::RBP,
                    GPReg::RSI,
                    // Don't use stack pionter: GPReg::RSP,
                    GPReg::RDI,
                    GPReg::R12,
                    GPReg::R13,
                    GPReg::R14,
                    GPReg::R15,
                    // Use caller saved regs first.
                    GPReg::RAX,
                    GPReg::RCX,
                    GPReg::RDX,
                    GPReg::R8,
                    GPReg::R9,
                    GPReg::R10,
                    GPReg::R11,
                ],
                gp_free_regs: bumpalo::vec![in env.arena],
                gp_used_regs: bumpalo::vec![in env.arena],
                stack_size: 0,
                shadow_space_size: 32,
                red_zone_size: 0,
                caller_saved_regs: ImSet::from(vec![
                    GPReg::RAX,
                    GPReg::RCX,
                    GPReg::RDX,
                    GPReg::R8,
                    GPReg::R9,
                    GPReg::R10,
                    GPReg::R11,
                ]),
                callee_saved_regs: ImSet::from(vec![
                    GPReg::RBX,
                    GPReg::RBP,
                    GPReg::RSI,
                    GPReg::RSP,
                    GPReg::RDI,
                    GPReg::R12,
                    GPReg::R13,
                    GPReg::R14,
                    GPReg::R15,
                ]),
                used_callee_saved_regs: MutSet::default(),
            }),
            x => Err(format!("unsupported backend: {:?}", x)),
        }
    }

    fn env(&self) -> &'a Env<'a> {
        self.env
    }

    fn reset(&mut self) {
        self.stack_size = -(self.red_zone_size as i32);
        self.leaf_function = true;
        self.last_seen_map.clear();
        self.free_map.clear();
        self.symbols_map.clear();
        self.buf.clear();
        self.used_callee_saved_regs.clear();
        self.gp_free_regs.clear();
        self.gp_used_regs.clear();
        self.gp_free_regs
            .extend_from_slice(self.gp_default_free_regs);
    }

    fn set_not_leaf_function(&mut self) {
        self.leaf_function = true;
        // If this is not a leaf function, it can't use the shadow space.
        self.stack_size = self.shadow_space_size as i32 - self.red_zone_size as i32;
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
            asm::push_register64bit(&mut out, GPReg::RBP);
            asm::mov_register64bit_register64bit(&mut out, GPReg::RBP, GPReg::RSP);
        }
        // Save data in all callee saved regs.
        let mut pop_order = bumpalo::vec![in self.env.arena];
        for reg in &self.used_callee_saved_regs {
            asm::push_register64bit(&mut out, *reg);
            pop_order.push(*reg);
        }
        if self.stack_size > 0 {
            asm::sub_register64bit_immediate32bit(&mut out, GPReg::RSP, self.stack_size);
        }

        // Add function body.
        out.extend(&self.buf);

        if self.stack_size > 0 {
            asm::add_register64bit_immediate32bit(&mut out, GPReg::RSP, self.stack_size);
        }
        // Restore data in callee saved regs.
        while let Some(reg) = pop_order.pop() {
            asm::pop_register64bit(&mut out, reg);
        }
        if !self.leaf_function {
            asm::pop_register64bit(&mut out, GPReg::RBP);
        }
        asm::ret_near(&mut out);

        Ok((out.into_bump_slice(), &[]))
    }

    fn build_num_abs_i64(&mut self, dst: &Symbol, src: &Symbol) -> Result<(), String> {
        let dst_reg = self.claim_gp_reg(dst)?;
        let src_reg = self.load_to_reg(src)?;
        asm::mov_register64bit_register64bit(&mut self.buf, dst_reg, src_reg);
        asm::neg_register64bit(&mut self.buf, dst_reg);
        asm::cmovl_register64bit_register64bit(&mut self.buf, dst_reg, src_reg);
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
        asm::mov_register64bit_register64bit(&mut self.buf, dst_reg, src1_reg);
        let src2_reg = self.load_to_reg(src2)?;
        asm::add_register64bit_register64bit(&mut self.buf, dst_reg, src2_reg);
        Ok(())
    }

    fn load_literal(&mut self, sym: &Symbol, lit: &Literal<'a>) -> Result<(), String> {
        match lit {
            Literal::Int(x) => {
                let reg = self.claim_gp_reg(sym)?;
                let val = *x;
                asm::mov_register64bit_immediate64bit(&mut self.buf, reg, val);
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
            Some(SymbolStorage::GPReg(reg)) if *reg == self.gp_return_regs[0] => Ok(()),
            Some(SymbolStorage::GPReg(reg)) => {
                // If it fits in a general purpose register, just copy it over to.
                // Technically this can be optimized to produce shorter instructions if less than 64bits.
                asm::mov_register64bit_register64bit(&mut self.buf, self.gp_return_regs[0], *reg);
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
impl<'a> X86_64Backend<'a> {
    fn claim_gp_reg(&mut self, sym: &Symbol) -> Result<GPReg, String> {
        let reg = if !self.gp_free_regs.is_empty() {
            let free_reg = self.gp_free_regs.pop().unwrap();
            if self.callee_saved_regs.contains(&free_reg) {
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
        self.symbols_map.insert(*sym, SymbolStorage::GPReg(reg));
        Ok(reg)
    }

    fn load_to_reg(&mut self, sym: &Symbol) -> Result<GPReg, String> {
        let val = self.symbols_map.remove(sym);
        match val {
            Some(SymbolStorage::GPReg(reg)) => {
                self.symbols_map.insert(*sym, SymbolStorage::GPReg(reg));
                Ok(reg)
            }
            Some(SymbolStorage::StackAndGPReg(reg, offset)) => {
                self.symbols_map
                    .insert(*sym, SymbolStorage::StackAndGPReg(reg, offset));
                Ok(reg)
            }
            Some(SymbolStorage::Stack(offset)) => {
                let reg = self.claim_gp_reg(sym)?;
                self.symbols_map
                    .insert(*sym, SymbolStorage::StackAndGPReg(reg, offset));
                asm::mov_register64bit_stackoffset32bit(&mut self.buf, reg, offset as i32);
                Ok(reg)
            }
            None => Err(format!("Unknown symbol: {}", sym)),
        }
    }

    fn free_to_stack(&mut self, sym: &Symbol) -> Result<(), String> {
        let val = self.symbols_map.remove(sym);
        match val {
            Some(SymbolStorage::GPReg(reg)) => {
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
                asm::mov_stackoffset32bit_register64bit(&mut self.buf, offset as i32, reg);
                self.symbols_map
                    .insert(*sym, SymbolStorage::Stack(offset as i32));
                Ok(())
            }
            Some(SymbolStorage::StackAndGPReg(_, offset)) => {
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
