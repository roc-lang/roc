use crate::{Backend, Env, Relocation};
use bumpalo::collections::Vec;
use roc_collections::all::{ImSet, MutMap};
use roc_module::symbol::Symbol;
use roc_mono::ir::{Literal, Stmt};
use roc_mono::layout::Layout;
use target_lexicon::{CallingConvention, Triple};

mod asm;
use asm::GPReg;

#[derive(Clone, Debug, PartialEq)]
enum SymbolStorage<'a> {
    Literal(Literal<'a>),
    GPReg(GPReg, Layout<'a>),
    Stack(u32, Layout<'a>),
}

pub struct X86_64Backend<'a> {
    env: &'a Env<'a>,
    buf: Vec<'a, u8>,

    /// leaf_proc is true if the only calls this function makes are tail calls.
    /// If that is the case, we can skip emitting the frame pointer and updating the stack.
    leaf_proc: bool,

    last_seen_map: MutMap<Symbol, *const Stmt<'a>>,
    symbols_map: MutMap<Symbol, SymbolStorage<'a>>,

    gp_param_regs: &'static [GPReg],
    gp_return_regs: &'static [GPReg],

    // This should probably be smarter than a vec.
    // There are certain registers we should always use first. With pushing and poping, this could get mixed.
    gp_free_regs: Vec<'a, GPReg>,

    // The last major thing we need is a way to decide what reg to free when all of them are full.
    // Theoretically we want a basic lru cache for the currently loaded symbols.
    // For now just a vec of used registers and the symbols they contain.
    gp_used_regs: Vec<'a, (GPReg, Symbol)>,

    // not sure how big this should be u16 is 64k. I hope no function uses that much stack.
    stack_size: u16,
    shadow_space_size: u8,
    red_zone_size: u8,

    // A linear scan of an array may be faster than a set technically.
    // That being said, fastest would likely be a trait based on calling convention/register.
    caller_saved_regs: ImSet<GPReg>,
    callee_saved_regs: ImSet<GPReg>,
}

impl<'a> Backend<'a> for X86_64Backend<'a> {
    fn new(env: &'a Env, target: &Triple) -> Result<Self, String> {
        match target.default_calling_convention() {
            Ok(CallingConvention::SystemV) => Ok(X86_64Backend {
                env,
                leaf_proc: true,
                buf: bumpalo::vec!(in env.arena),
                last_seen_map: MutMap::default(),
                symbols_map: MutMap::default(),
                gp_param_regs: &[
                    GPReg::RDI,
                    GPReg::RSI,
                    GPReg::RDX,
                    GPReg::RCX,
                    GPReg::R8,
                    GPReg::R9,
                ],
                gp_return_regs: &[GPReg::RAX, GPReg::RDX],
                gp_free_regs: bumpalo::vec![in env.arena;
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
            }),
            Ok(CallingConvention::WindowsFastcall) => Ok(X86_64Backend {
                env,
                leaf_proc: true,
                buf: bumpalo::vec!(in env.arena),
                last_seen_map: MutMap::default(),
                symbols_map: MutMap::default(),
                gp_param_regs: &[GPReg::RCX, GPReg::RDX, GPReg::R8, GPReg::R9],
                gp_return_regs: &[GPReg::RAX],
                gp_free_regs: bumpalo::vec![in env.arena;
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
            }),
            x => Err(format!("unsupported backend: {:?}", x)),
        }
    }

    fn env(&self) -> &'a Env<'a> {
        self.env
    }

    fn reset(&mut self) {
        self.symbols_map.clear();
        self.buf.clear();
    }

    fn last_seen_map(&mut self) -> &mut MutMap<Symbol, *const Stmt<'a>> {
        &mut self.last_seen_map
    }

    fn set_symbol_to_lit(&mut self, sym: &Symbol, lit: &Literal<'a>) {
        self.symbols_map
            .insert(*sym, SymbolStorage::Literal(lit.clone()));
    }

    fn free_symbol(&mut self, sym: &Symbol) {
        self.symbols_map.remove(sym);
    }

    fn return_symbol(&mut self, sym: &Symbol) -> Result<(), String> {
        let val = self.symbols_map.get(sym);
        match val {
            Some(SymbolStorage::Literal(Literal::Int(x))) => {
                let val = *x;
                if val <= i32::MAX as i64 && val >= i32::MIN as i64 {
                    asm::mov_register64bit_immediate32bit(
                        &mut self.buf,
                        self.gp_return_regs[0],
                        val as i32,
                    );
                } else {
                    asm::mov_register64bit_immediate64bit(
                        &mut self.buf,
                        self.gp_return_regs[0],
                        val,
                    );
                }
                Ok(())
            }
            Some(SymbolStorage::GPReg(reg, _)) if *reg == self.gp_return_regs[0] => Ok(()),
            Some(SymbolStorage::GPReg(reg, _)) => {
                // If it fits in a general purpose register, just copy it over to.
                // Technically this can be optimized to produce shorter instructions if less than 64bits.
                asm::mov_register64bit_register64bit(&mut self.buf, self.gp_return_regs[0], *reg);
                Ok(())
            }
            Some(x) => Err(format!("symbol, {:?}, is not yet implemented", x)),
            None => Err(format!("Unknown return symbol: {}", sym)),
        }
    }

    fn finalize(&mut self) -> Result<(&'a [u8], &[Relocation]), String> {
        // TODO: handle allocating and cleaning up data on the stack.
        let mut out = bumpalo::vec![in self.env.arena];
        if self.requires_stack_modification() {
            asm::push_register64bit(&mut out, GPReg::RBP);
            asm::mov_register64bit_register64bit(&mut out, GPReg::RBP, GPReg::RSP);
        }
        out.extend(&self.buf);

        if self.requires_stack_modification() {
            asm::pop_register64bit(&mut out, GPReg::RBP);
        }
        asm::ret_near(&mut out);

        Ok((out.into_bump_slice(), &[]))
    }
}

/// This impl block is for ir related instructions that need backend specific information.
/// For example, loading a symbol for doing a computation.
impl<'a> X86_64Backend<'a> {
    fn requires_stack_modification(&self) -> bool {
        !self.leaf_proc
            || self.stack_size < self.shadow_space_size as u16 + self.red_zone_size as u16
    }
}
