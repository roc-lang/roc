use can::symbol::Symbol;
use collections::ImMap;
use ident::Ident;
use region::Region;

#[derive(Clone, Debug, PartialEq)]
pub struct Scope<'a> {
    pub idents: ImMap<Ident, (Symbol<'a>, Region)>,
    symbol_prefix: &'a str,
    next_unique_id: u64,
}

impl<'a> Scope<'a> {
    pub fn new(
        symbol_prefix: &'a str,
        declared_idents: ImMap<Ident, (Symbol<'a>, Region)>,
    ) -> Scope<'a> {
        Scope {
            symbol_prefix,

            // This is used to generate unique names for anonymous closures.
            // It always begins at 0.
            next_unique_id: 0,

            idents: declared_idents,
        }
    }

    pub fn symbol(&'a self, name: &'a str) -> Symbol<'a> {
        Symbol::new(&self.symbol_prefix, name)
    }

    pub fn gen_unique_symbol(&mut self) -> Symbol<'a> {
        self.next_unique_id = self.next_unique_id + 1;

        Symbol::new(&self.symbol_prefix, &self.next_unique_id.to_string())
    }
}
