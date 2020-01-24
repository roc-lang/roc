use crate::can::problem::{Problem, RuntimeError};
use crate::can::procedure::References;
use crate::collections::MutMap;
use crate::module::symbol::{IdentIds, ModuleId, ModuleIds, Symbol};
use inlinable_string::InlinableString;
use std::sync::Arc;

/// The canonicalization environment for a particular module.
pub struct Env {
    /// The module's path. Private tags and unqualified references to identifiers
    /// are assumed to be relative to this path.
    pub home: ModuleId,

    pub dep_idents: MutMap<ModuleId, Arc<IdentIds>>,

    pub module_ids: ModuleIds,

    /// Problems we've encountered along the way, which will be reported to the user at the end.
    pub problems: Vec<Problem>,

    /// Closures
    pub closures: MutMap<Symbol, References>,

    /// current tail-callable symbol
    pub tailcallable_symbol: Option<Symbol>,

    pub ident_ids: IdentIds,
}

impl Env {
    pub fn new(
        home: ModuleId,
        dep_idents: MutMap<ModuleId, Arc<IdentIds>>,
        module_ids: ModuleIds,
        home_ident_ids: IdentIds,
    ) -> Env {
        Env {
            home,
            dep_idents,
            module_ids,
            ident_ids: home_ident_ids,
            problems: Vec::new(),
            closures: MutMap::default(),
            tailcallable_symbol: None,
        }
    }

    /// Returns Err if the symbol resolved, but it was not exposed by the given module
    pub fn qualified_lookup(&self, module_name: &str, ident: &str) -> Result<Symbol, RuntimeError> {
        let module_name: InlinableString = module_name.into();

        match self.module_ids.get_id(&module_name) {
            Some(module_id) => {
                let ident: InlinableString = ident.into();

                match self
                    .dep_idents
                    .get(&module_id)
                    .and_then(|exposed_ids| exposed_ids.get_id(&ident))
                {
                    Some(ident_id) => Ok(Symbol::new(*module_id, *ident_id)),
                    None => Err(RuntimeError::ValueNotExposed { module_name, ident }),
                }
            }
            None => Err(RuntimeError::ModuleNotImported(module_name)),
        }
    }

    /// Generates a unique, new symbol like "$1" or "$5",
    /// using the home module as the module_id.
    ///
    /// This is used, for example, during canonicalization of an Expr::Closure
    /// to generate a unique symbol to refer to that closure.
    pub fn gen_unique_symbol(&mut self) -> Symbol {
        let ident_id = self.ident_ids.gen_unique();

        Symbol::new(self.home, ident_id)
    }

    pub fn problem(&mut self, problem: Problem) {
        self.problems.push(problem)
    }

    pub fn register_closure(&mut self, symbol: Symbol, references: References) {
        self.closures.insert(symbol, references);
    }
}
