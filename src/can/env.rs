use can::expr::Expr;
use can::pattern::Pattern;
use can::problem::Problem;
use can::procedure::{Procedure, References};
use can::symbol::Symbol;
use collections::{ImMap, MutMap};
use region::{Located, Region};
use subs::Variable;

/// The canonicalization environment for a particular module.
pub struct Env {
    /// The module's path. Unqualified references to identifiers and variant names are assumed
    /// to be relative to this path.
    pub home: Box<str>,

    /// Problems we've encountered along the way, which will be reported to the user at the end.
    pub problems: Vec<Problem>,

    /// Variants either declared in this module, or imported.
    pub variants: ImMap<Symbol, Located<Box<str>>>,

    /// Former closures converted to top-level procedures.
    pub procedures: MutMap<Symbol, Procedure>,
}

impl Env {
    pub fn new(home: Box<str>, declared_variants: ImMap<Symbol, Located<Box<str>>>) -> Env {
        Env {
            home,
            variants: declared_variants,
            problems: Vec::new(),
            procedures: MutMap::default(),
        }
    }

    pub fn problem(&mut self, problem: Problem) {
        self.problems.push(problem)
    }

    pub fn register_closure(
        &mut self,
        symbol: Symbol,
        args: Vec<Located<Pattern>>,
        body: Located<Expr>,
        definition: Region,
        references: References,
        var: Variable,
        ret_var: Variable,
    ) {
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
            var,
            ret_var,
        };

        self.procedures.insert(symbol, procedure);
    }
}
