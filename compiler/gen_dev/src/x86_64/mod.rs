use crate::{Backend, Env};
use bumpalo::collections::Vec;
use roc_collections::all::MutMap;
use roc_module::symbol::Symbol;
use roc_mono::ir::Literal;
use roc_mono::layout::Layout;

mod asm;

use asm::Register;

const RETURN_REG: Register = Register::RAX;

pub struct X86_64Backend<'a> {
    env: &'a Env<'a>,
    buf: Vec<'a, u8>,

    /// leaf_proc is true if the only calls this function makes are tail calls.
    /// If that is the case, we can skip emitting the frame pointer and updating the stack.
    leaf_proc: bool,

    // This will need to hold info a symbol is held in a register or on the stack as well.
    symbols_map: MutMap<Symbol, (Literal<'a>, Layout<'a>)>,
    // This is gonna need to include a lot of data. Right now I can think of quite a few.
    // Registers order by priority with info of what data is stored in them.
    // Scope with knows were all variables are currently stored.X86_64Backend

    // Since this is x86_64 the calling convetions is really just windows or linux/macos.
    // Hopefully this will be easy to extract into a trait somehow. Cause probably don't want if's everywhere.
    // Also, don't really want to build an x86_64-win backend specifically for it.

    // function parameter registers listed by order. Need to know the float equivalent registers as well.
    // Probably need to encode stack parameter knowledge here too.
    // return parameter register. This includes dealing with multiple value returns.
}

impl<'a> Backend<'a> for X86_64Backend<'a> {
    fn new(env: &'a Env) -> Self {
        X86_64Backend {
            env,
            leaf_proc: true,
            buf: bumpalo::vec!(in env.arena),
            symbols_map: MutMap::default(),
        }
    }

    fn env(&self) -> &'a Env<'a> {
        self.env
    }

    fn reset(&mut self) {
        self.symbols_map.clear();
        self.buf.clear();
    }

    fn finalize(&mut self) -> &'a [u8] {
        // TODO: handle allocating and cleaning up data on the stack.
        let mut out = bumpalo::vec![in self.env.arena];
        if !self.leaf_proc {
            asm::push_register64bit(&mut out, Register::RBP);
            asm::mov_register64bit_register64bit(&mut out, Register::RBP, Register::RSP);
        }
        out.extend(&self.buf);

        if !self.leaf_proc {
            asm::pop_register64bit(&mut out, Register::RBP);
        }
        asm::ret_near(&mut out);

        out.into_bump_slice()
    }

    fn set_symbol_to_lit(&mut self, sym: &Symbol, lit: &Literal<'a>, layout: &Layout<'a>) {
        self.symbols_map.insert(*sym, (lit.clone(), layout.clone()));
    }

    fn return_symbol(&mut self, sym: &Symbol) {
        self.load_symbol(RETURN_REG, sym);
    }
}

/// This impl block is for ir related instructions that need backend specific information.
/// For example, loading a symbol for doing a computation.
impl<'a> X86_64Backend<'a> {
    fn load_symbol(&mut self, dst: Register, sym: &Symbol) {
        let val = self.symbols_map.get(sym);
        match val {
            Some((Literal::Int(x), _)) => {
                let val = *x;
                if val <= i32::MAX as i64 && val >= i32::MIN as i64 {
                    asm::mov_register64bit_immediate32bit(&mut self.buf, dst, val as i32);
                } else {
                    asm::mov_register64bit_immediate64bit(&mut self.buf, dst, val);
                }
            }
            Some(x) => unimplemented!("symbol, {:?}, is not yet implemented", x),
            None => panic!("Unknown return symbol: {}", sym),
        }
    }
}
