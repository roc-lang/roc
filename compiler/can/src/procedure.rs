use crate::expr::Expr;
use crate::pattern::Pattern;
use roc_collections::all::ImSet;
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
    pub bound_symbols: ImSet<Symbol>,
    pub lookups: ImSet<Symbol>,
    /// Aliases or opaque types referenced
    pub referenced_type_defs: ImSet<Symbol>,
    pub calls: ImSet<Symbol>,
}

impl References {
    pub fn new() -> References {
        Self::default()
    }

    pub fn union(mut self, other: References) -> Self {
        self.lookups = self.lookups.union(other.lookups);
        self.calls = self.calls.union(other.calls);
        self.bound_symbols = self.bound_symbols.union(other.bound_symbols);
        self.referenced_type_defs = self.referenced_type_defs.union(other.referenced_type_defs);

        self
    }

    pub fn union_mut(&mut self, other: References) {
        self.lookups.extend(other.lookups);
        self.calls.extend(other.calls);
        self.bound_symbols.extend(other.bound_symbols);
        self.referenced_type_defs.extend(other.referenced_type_defs);
    }

    pub fn has_lookup(&self, symbol: Symbol) -> bool {
        self.lookups.contains(&symbol)
    }
}
