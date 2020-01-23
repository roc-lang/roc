use crate::can::ident::ModuleName;
use crate::can::problem::Problem;
use crate::can::procedure::References;
use crate::module::symbol::Symbol;
use crate::collections::MutMap;

/// The canonicalization environment for a particular module.
pub struct Env {
    /// The module's path. Private tags and unqualified references to identifiers
    /// are assumed to be relative to this path.
    pub home: ModuleName,

    /// Problems we've encountered along the way, which will be reported to the user at the end.
    pub problems: Vec<Problem>,

    /// Closures
    pub closures: MutMap<Symbol, References>,

    /// current tail-callable symbol
    pub tailcallable_symbol: Option<Symbol>,
}

impl Env {
    pub fn new(home: ModuleName) -> Env {
        Env {
            home,
            problems: Vec::new(),
            closures: MutMap::default(),
            tailcallable_symbol: None,
        }
    }

    pub fn problem(&mut self, problem: Problem) {
        self.problems.push(problem)
    }

    pub fn register_closure(&mut self, symbol: Symbol, references: References) {
        self.closures.insert(symbol, references);
    }
}
