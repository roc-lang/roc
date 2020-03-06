use crate::problem::{Problem, RuntimeError};
use crate::procedure::References;
use inlinable_string::InlinableString;
use roc_collections::all::{MutMap, MutSet};
use roc_module::symbol::{IdentIds, ModuleId, ModuleIds, Symbol};
use roc_region::all::Region;

/// The canonicalization environment for a particular module.
pub struct Env<'a> {
    /// The module's path. Private tags and unqualified references to identifiers
    /// are assumed to be relative to this path.
    pub home: ModuleId,

    pub dep_idents: MutMap<ModuleId, IdentIds>,

    pub module_ids: &'a ModuleIds,

    /// Problems we've encountered along the way, which will be reported to the user at the end.
    pub problems: Vec<Problem>,

    /// Closures
    pub closures: MutMap<Symbol, References>,

    /// current tail-callable symbol
    pub tailcallable_symbol: Option<Symbol>,

    /// Symbols which were referenced by qualified lookups.
    pub referenced_symbols: MutSet<Symbol>,

    pub ident_ids: IdentIds,
    pub exposed_ident_ids: IdentIds,
}

impl<'a> Env<'a> {
    pub fn new(
        home: ModuleId,
        dep_idents: MutMap<ModuleId, IdentIds>,
        module_ids: &'a ModuleIds,
        exposed_ident_ids: IdentIds,
    ) -> Env<'a> {
        Env {
            home,
            dep_idents,
            module_ids,
            ident_ids: exposed_ident_ids.clone(), // we start with these, but will add more later
            exposed_ident_ids,
            problems: Vec::new(),
            closures: MutMap::default(),
            referenced_symbols: MutSet::default(),
            tailcallable_symbol: None,
        }
    }

    /// Returns Err if the symbol resolved, but it was not exposed by the given module
    pub fn qualified_lookup(
        &mut self,
        module_name: &str,
        ident: &str,
        region: Region,
    ) -> Result<Symbol, RuntimeError> {
        debug_assert!(
            !module_name.is_empty(),
            "Called env.qualified_lookup with an unqualified ident: {:?}",
            ident
        );

        let module_name: InlinableString = module_name.into();

        match self.module_ids.get_id(&module_name) {
            Some(&module_id) => {
                let ident: InlinableString = ident.into();

                match self
                    .dep_idents
                    .get(&module_id)
                    .and_then(|exposed_ids| exposed_ids.get_id(&ident))
                {
                    Some(ident_id) => {
                        let symbol = Symbol::new(module_id, *ident_id);

                        self.referenced_symbols.insert(symbol);

                        Ok(symbol)
                    }
                    None => Err(RuntimeError::ValueNotExposed {
                        module_name,
                        ident,
                        region,
                    }),
                }
            }
            None => Err(RuntimeError::ModuleNotImported {
                module_name,
                ident: ident.into(),
                region,
            }),
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
