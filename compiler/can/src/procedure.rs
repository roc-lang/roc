use crate::expr::Expr;
use crate::pattern::Pattern;
use roc_collections::all::VecSet;
use roc_module::symbol::Symbol;
use roc_region::all::{Loc, Region};
use roc_types::subs::Variable;

#[derive(Clone, Debug, PartialEq)]
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
    pub bound_symbols: VecSet<Symbol>,
    pub type_lookups: VecSet<Symbol>,
    pub value_lookups: VecSet<Symbol>,
    /// Aliases or opaque types referenced
    pub referenced_type_defs: VecSet<Symbol>,
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
}
