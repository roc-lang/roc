use crate::procedure::References;
use roc_collections::all::{MutMap, MutSet};
use roc_module::ident::{Ident, Lowercase, ModuleName};
use roc_module::symbol::{IdentIds, ModuleId, ModuleIds, Symbol};
use roc_problem::can::{Problem, RuntimeError};
use roc_region::all::{Loc, Region};

/// The canonicalization environment for a particular module.
pub struct Env<'a> {
    /// The module's path. Private tags and unqualified references to identifiers
    /// are assumed to be relative to this path.
    pub home: ModuleId,

    pub dep_idents: &'a MutMap<ModuleId, IdentIds>,

    pub module_ids: &'a ModuleIds,

    /// Problems we've encountered along the way, which will be reported to the user at the end.
    pub problems: Vec<Problem>,

    /// Closures
    pub closures: MutMap<Symbol, References>,

    /// current tail-callable symbol
    pub tailcallable_symbol: Option<Symbol>,

    /// current closure name (if any)
    pub closure_name_symbol: Option<Symbol>,

    /// Symbols which were referenced by qualified lookups.
    pub qualified_lookups: MutSet<Symbol>,

    pub top_level_symbols: MutSet<Symbol>,

    pub ident_ids: IdentIds,
    pub exposed_ident_ids: IdentIds,
}

impl<'a> Env<'a> {
    pub fn new(
        home: ModuleId,
        dep_idents: &'a MutMap<ModuleId, IdentIds>,
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
            qualified_lookups: MutSet::default(),
            tailcallable_symbol: None,
            closure_name_symbol: None,
            top_level_symbols: MutSet::default(),
        }
    }

    /// Returns Err if the symbol resolved, but it was not exposed by the given module
    pub fn qualified_lookup(
        &mut self,
        module_name_str: &str,
        ident: &str,
        region: Region,
    ) -> Result<Symbol, RuntimeError> {
        debug_assert!(
            !module_name_str.is_empty(),
            "Called env.qualified_lookup with an unqualified ident: {:?}",
            ident
        );

        let module_name = ModuleName::from(module_name_str);
        let ident = Ident::from(ident);

        match self.module_ids.get_id(&module_name) {
            Some(&module_id) => {
                // You can do qualified lookups on your own module, e.g.
                // if I'm in the Foo module, I can do a `Foo.bar` lookup.
                if module_id == self.home {
                    match self.ident_ids.get_id(&ident) {
                        Some(ident_id) => {
                            let symbol = Symbol::new(module_id, *ident_id);

                            self.qualified_lookups.insert(symbol);

                            Ok(symbol)
                        }
                        None => Err(RuntimeError::LookupNotInScope(
                            Loc {
                                value: ident,
                                region,
                            },
                            self.ident_ids
                                .idents()
                                .map(|(_, string)| string.as_ref().into())
                                .collect(),
                        )),
                    }
                } else {
                    match self.dep_idents.get(&module_id) {
                        Some(exposed_ids) => match exposed_ids.get_id(&ident) {
                            Some(ident_id) => {
                                let symbol = Symbol::new(module_id, *ident_id);

                                self.qualified_lookups.insert(symbol);

                                Ok(symbol)
                            }
                            None => {
                                let exposed_values = exposed_ids
                                    .idents()
                                    .filter(|(_, ident)| {
                                        ident.as_ref().starts_with(|c: char| c.is_lowercase())
                                    })
                                    .map(|(_, ident)| Lowercase::from(ident.as_ref()))
                                    .collect();
                                Err(RuntimeError::ValueNotExposed {
                                    module_name,
                                    ident,
                                    region,
                                    exposed_values,
                                })
                            }
                        },
                        None => Err(RuntimeError::ModuleNotImported {
                            module_name,
                            imported_modules: self
                                .dep_idents
                                .keys()
                                .filter_map(|module_id| self.module_ids.get_name(*module_id))
                                .map(|module_name| module_name.as_ref().into())
                                .collect(),
                            region,
                            module_exists: true,
                        }),
                    }
                }
            }
            None => Err(RuntimeError::ModuleNotImported {
                module_name,
                imported_modules: self
                    .module_ids
                    .available_modules()
                    .map(|string| string.as_ref().into())
                    .collect(),
                region,
                module_exists: false,
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
