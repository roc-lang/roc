use crate::expr::Expr;
use crate::pattern::Pattern;
use roc_collections::all::MutSet;
use roc_module::symbol::Symbol;
use roc_region::all::{Located, Region};
use roc_types::subs::Variable;

#[derive(Clone, Debug, PartialEq)]
pub struct Procedure {
    pub name: Option<Box<str>>,
    pub is_self_tail_recursive: bool,
    pub definition: Region,
    pub args: Vec<Located<Pattern>>,
    pub body: Located<Expr>,
    pub references: References,
    pub var: Variable,
    pub ret_var: Variable,
}

impl Procedure {
    pub fn new(
        definition: Region,
        args: Vec<Located<Pattern>>,
        body: Located<Expr>,
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
/// to determine how defs shuold be ordered. We want builds to be reproducible,
/// so it's important that building the same code gives the same order every time!
#[derive(Clone, Debug, Default, PartialEq)]
pub struct References {
    pub lookups: MutSet<Symbol>,
    pub calls: MutSet<Symbol>,
}

impl References {
    pub fn new() -> References {
        References {
            lookups: MutSet::default(),
            calls: MutSet::default(),
        }
    }

    pub fn union(mut self, other: References) -> Self {
        self.lookups.extend(other.lookups);
        self.calls.extend(other.calls);

        self
    }

    pub fn union_mut(&mut self, other: References) {
        self.lookups.extend(other.lookups);
        self.calls.extend(other.calls);
    }

    pub fn has_lookup(&self, symbol: Symbol) -> bool {
        self.lookups.contains(&symbol)
    }
}
