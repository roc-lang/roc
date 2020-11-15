use crate::{Backend, Env};
use bumpalo::collections::Vec;
use roc_collections::all::MutMap;
use roc_module::symbol::Symbol;
use roc_mono::ir::Literal;
use roc_mono::layout::Layout;

pub struct X86_64Backend<'a> {
    env: &'a Env<'a>,

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
            symbols_map: MutMap::default(),
        }
    }

    fn env(&self) -> &'a Env<'a> {
        self.env
    }

    fn reset(&mut self) {
        self.symbols_map.clear();
    }

    fn wrap_proc(&mut self, body: Vec<'a, u8>) -> Vec<'a, u8> {
        // push rbp     (0x55)
        // mov rbp, rsp (0x48, 0x89, 0xE5)
        let mut out = bumpalo::vec![in self.env.arena; 0x55, 0x48, 0x89, 0xE5];
        out.reserve(body.len() + 2);

        // TODO: handle allocating and cleaning up data on the stack.

        out.extend(body);

        // pop rbp
        out.push(0x5D);
        // ret
        out.push(0xC3);

        out
    }

    fn set_symbol_to_lit(&mut self, sym: &Symbol, lit: &Literal<'a>, layout: &Layout<'a>) {
        self.symbols_map
            .insert(sym.clone(), (lit.clone(), layout.clone()));
    }

    fn return_symbol(&mut self, body: &mut Vec<'a, u8>, sym: &Symbol) {
        //let body = bumpalo::vec![in env.arena; 0xb8, 0x06, 0x00, 0x00, 0x00];
        match self.symbols_map.get(sym) {
            Some((Literal::Int(x), _)) => {
                // movabs rax, ...
                body.extend(&[0x48, 0xB8]);
                body.extend(&x.to_le_bytes());
            }
            Some(x) => unimplemented!("return value, {:?}, is not yet implemented", x),
            None => panic!("Unknown return symbol: {}", sym),
        }
    }
}
