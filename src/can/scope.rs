use crate::can::ident::ModuleName;
use crate::can::ident::{Lowercase, Uppercase};
use crate::can::symbol::Symbol;
use crate::collections::ImMap;
use crate::ident::Ident;
use crate::region::{Located, Region};
use crate::types::Type;

#[derive(Clone, Debug, PartialEq)]
pub struct Scope {
    pub idents: ImMap<Ident, (Symbol, Region)>,
    pub aliases: ImMap<Uppercase, (Region, Vec<Located<Lowercase>>, Type)>,
    pub module_name: ModuleName,
    symbol_prefix: Box<str>,
    next_unique_id: u64,
}

impl Scope {
    pub fn new(
        module_name: ModuleName,
        symbol_prefix: Box<str>,
        declared_idents: ImMap<Ident, (Symbol, Region)>,
    ) -> Scope {
        Scope {
            symbol_prefix,
            module_name,

            // This is used to generate unique names for anonymous closures.
            // It always begins at 0.
            next_unique_id: 0,

            aliases: ImMap::default(),
            idents: declared_idents,
        }
    }

    pub fn symbol(&self, name: &str) -> Symbol {
        Symbol::new(&self.symbol_prefix, name)
    }

    pub fn gen_unique_symbol(&mut self) -> Symbol {
        self.next_unique_id += 1;

        Symbol::new(&self.symbol_prefix, &self.next_unique_id.to_string())
    }

    pub fn add_alias(
        &mut self,
        name: Uppercase,
        region: Region,
        vars: Vec<Located<Lowercase>>,
        typ: Type,
    ) {
        self.aliases.insert(name, (region, vars, typ));
    }
}
