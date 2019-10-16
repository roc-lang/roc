use bumpalo::collections::Vec;
use bumpalo::Bump;
use can::expr::Expr;
use can::pattern::Pattern;
use can::problem::Problem;
use can::procedure::{Procedure, References};
use can::symbol::Symbol;
use collections::{ImMap, MutMap};
use region::{Located, Region};
use subs::Subs;

/// The canonicalization environment for a particular module.
pub struct Env<'a> {
    /// The module's path. Unqualified references to identifiers and variant names are assumed
    /// to be relative to this path.
    pub home: &'a str,

    /// Problems we've encountered along the way, which will be reported to the user at the end.
    pub problems: Vec<'a, Problem<'a>>,

    /// Variants either declared in this module, or imported.
    pub variants: ImMap<Symbol<'a>, Located<&'a str>>,

    /// Former closures converted to top-level procedures.
    pub procedures: MutMap<Symbol<'a>, Procedure<'a>>,

    pub arena: &'a Bump,

    pub subs: Subs<'a>,
}

impl<'a> Env<'a> {
    pub fn new(
        arena: &'a Bump,
        home: &'a str,
        declared_variants: ImMap<Symbol<'a>, Located<&'a str>>,
    ) -> Env<'a> {
        Env {
            home,
            variants: declared_variants,
            problems: Vec::new_in(arena),
            procedures: MutMap::default(),
            arena,
            subs: Subs::new(arena),
        }
    }

    pub fn problem(&mut self, problem: Problem<'a>) -> () {
        self.problems.push(problem)
    }

    pub fn register_closure(
        &mut self,
        symbol: Symbol<'a>,
        args: &'a [Located<Pattern<'a>>],
        body: Located<Expr<'a>>,
        definition: Region,
        references: References<'a>,
    ) -> () {
        // We can't if the closure is self tail recursive yet, because it doesn't know its final name yet.
        // (Assign sets that.) Assume this is false, and let Assign change it to true after it sets final name.
        let is_self_tail_recursive = false;
        let name = None; // The Assign logic is also responsible for setting names after the fact.
        let procedure = Procedure {
            args,
            name,
            body,
            is_self_tail_recursive,
            definition,
            references,
        };

        self.procedures.insert(symbol, procedure);
    }
}
