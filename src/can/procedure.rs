use can::expr::Expr;
use can::pattern::Pattern;
use can::symbol::Symbol;
use collections::ImSet;
use region::{Located, Region};
use subs::Variable;

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
/// to determine how assignments shuold be ordered. We want builds to be reproducible,
/// so it's important that building the same code gives the same order every time!
#[derive(Clone, Debug, Default, PartialEq)]
pub struct References {
    pub locals: ImSet<Symbol>,
    pub globals: ImSet<Symbol>,
    pub variants: ImSet<Symbol>,
    pub calls: ImSet<Symbol>,
}

impl References {
    pub fn new() -> References {
        References {
            locals: ImSet::default(),
            globals: ImSet::default(),
            variants: ImSet::default(),
            calls: ImSet::default(),
        }
    }

    pub fn union(mut self, other: References) -> Self {
        self.locals = self.locals.union(other.locals);
        self.globals = self.globals.union(other.globals);
        self.variants = self.variants.union(other.variants);
        self.calls = self.calls.union(other.calls);

        self
    }

    pub fn has_local(&self, symbol: &Symbol) -> bool {
        self.locals.contains(symbol)
    }

    pub fn has_variant(&self, symbol: &Symbol) -> bool {
        self.variants.contains(symbol)
    }
}
