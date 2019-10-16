use can::expr::Expr;
use can::pattern::Pattern;
use can::symbol::Symbol;
use collections::ImSet;
use region::{Located, Region};

#[derive(Clone, Debug, PartialEq)]
pub struct Procedure<'a> {
    pub name: Option<&'a str>,
    pub is_self_tail_recursive: bool,
    pub definition: Region,
    pub args: &'a [Located<Pattern<'a>>],
    pub body: Located<Expr<'a>>,
    pub references: References<'a>,
}

impl<'a> Procedure<'a> {
    pub fn new(
        definition: Region,
        args: &'a [Located<Pattern<'a>>],
        body: Located<Expr<'a>>,
        references: References<'a>,
    ) -> Procedure<'a> {
        Procedure {
            name: None,
            is_self_tail_recursive: false,
            definition,
            args,
            body,
            references,
        }
    }
}

/// These are all ordered sets because they end up getting traversed in a graph search
/// to determine how assignments shuold be ordered. We want builds to be reproducible,
/// so it's important that building the same code gives the same order every time!
#[derive(Clone, Debug, PartialEq)]
pub struct References<'a> {
    pub locals: ImSet<Symbol<'a>>,
    pub globals: ImSet<Symbol<'a>>,
    pub variants: ImSet<Symbol<'a>>,
    pub calls: ImSet<Symbol<'a>>,
}

impl<'a> References<'a> {
    pub fn new() -> References<'a> {
        References {
            locals: ImSet::default(),
            globals: ImSet::default(),
            variants: ImSet::default(),
            calls: ImSet::default(),
        }
    }

    pub fn union(mut self, other: References<'a>) -> Self {
        self.locals = self.locals.union(other.locals);
        self.globals = self.globals.union(other.globals);
        self.variants = self.variants.union(other.variants);
        self.calls = self.calls.union(other.calls);

        self
    }

    pub fn has_local(&self, symbol: &Symbol<'a>) -> bool {
        self.locals.contains(symbol)
    }

    pub fn has_variant(&self, symbol: &Symbol<'a>) -> bool {
        self.variants.contains(symbol)
    }
}
