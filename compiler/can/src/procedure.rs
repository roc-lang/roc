use crate::expr::Expr;
use crate::pattern::Pattern;
use roc_collections::VecSet;
use roc_module::symbol::Symbol;
use roc_region::all::{Loc, Region};
use roc_types::subs::Variable;

#[derive(Clone, Debug)]
pub struct Procedure {
    pub name: Option<Box<str>>,
    pub is_self_tail_recursive: bool,
    pub definition: Region,
    pub args: Vec<Loc<Pattern>>,
    pub body: Loc<Expr>,
    pub references: References,
    pub var: Variable,
    pub ret_var: Variable,
}

impl Procedure {
    pub fn new(
        definition: Region,
        args: Vec<Loc<Pattern>>,
        body: Loc<Expr>,
        references: References,
        var: Variable,
        ret_var: Variable,
    ) -> Procedure {
        Procedure {
            name: None,
            is_self_tail_recursive: false,
            definition,
            args,
            body,
            references,
            var,
            ret_var,
        }
    }
}

/// These are all ordered sets because they end up getting traversed in a graph search
/// to determine how defs should be ordered. We want builds to be reproducible,
/// so it's important that building the same code gives the same order every time!
#[derive(Clone, Debug, Default, PartialEq)]
pub struct References {
    bound_symbols: VecSet<Symbol>,
    type_lookups: VecSet<Symbol>,
    value_lookups: VecSet<Symbol>,
    /// Aliases or opaque types referenced
    referenced_type_defs: VecSet<Symbol>,
    pub calls: VecSet<Symbol>,
}

impl References {
    pub fn new() -> References {
        Self::default()
    }

    pub fn union_mut(&mut self, other: &References) {
        self.value_lookups
            .extend(other.value_lookups.iter().copied());
        self.type_lookups.extend(other.type_lookups.iter().copied());
        self.calls.extend(other.calls.iter().copied());
        self.bound_symbols
            .extend(other.bound_symbols.iter().copied());
        self.referenced_type_defs
            .extend(other.referenced_type_defs.iter().copied());
    }

    pub fn has_value_lookup(&self, symbol: Symbol) -> bool {
        self.value_lookups.contains(&symbol)
    }

    pub fn has_type_lookup(&self, symbol: Symbol) -> bool {
        self.type_lookups.contains(&symbol)
    }

    pub fn references_type_def(&self, symbol: Symbol) -> bool {
        self.referenced_type_defs.contains(&symbol)
    }

    pub fn insert_value_lookup(&mut self, symbol: Symbol) {
        self.value_lookups.insert(symbol);
    }

    pub fn insert_bound(&mut self, symbol: Symbol) {
        self.bound_symbols.insert(symbol);
    }

    pub fn remove_value_lookup(&mut self, symbol: &Symbol) {
        self.value_lookups.remove(symbol);
    }

    pub fn insert_type_lookup(&mut self, symbol: Symbol) {
        self.type_lookups.insert(symbol);
        self.referenced_type_defs.insert(symbol);
    }

    pub fn value_lookups(&self) -> impl Iterator<Item = &Symbol> {
        self.value_lookups.iter()
    }

    pub fn type_lookups(&self) -> impl Iterator<Item = &Symbol> {
        self.type_lookups.iter()
    }

    pub fn bound_symbols(&self) -> impl Iterator<Item = &Symbol> {
        self.bound_symbols.iter()
    }
}
