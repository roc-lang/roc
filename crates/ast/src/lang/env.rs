use crate::mem_pool::pool::{NodeId, Pool};
use bumpalo::{collections::Vec as BumpVec, Bump};
use roc_collections::all::{MutMap, MutSet};
use roc_module::ident::{Ident, Lowercase, ModuleName};
use roc_module::symbol::{IdentIds, IdentIdsByModule, ModuleId, ModuleIds, Symbol};
use roc_problem::can::{Problem, RuntimeError};
use roc_region::all::{Loc, Region};
use roc_types::subs::VarStore;

use super::core::def::def::References;

/// TODO document
#[derive(Debug)]
pub struct Env<'a> {
    pub home: ModuleId,
    pub var_store: &'a mut VarStore,
    pub pool: &'a mut Pool,
    pub arena: &'a Bump,

    pub problems: BumpVec<'a, Problem>,

    pub dep_idents: IdentIdsByModule,
    pub module_ids: &'a ModuleIds,
    pub ident_ids: IdentIds,
    pub exposed_ident_ids: IdentIds,

    pub closures: MutMap<Symbol, References>,
    /// Symbols which were referenced by qualified lookups.
    pub qualified_lookups: MutSet<Symbol>,

    pub top_level_symbols: MutSet<Symbol>,

    pub closure_name_symbol: Option<Symbol>,
    pub tailcallable_symbol: Option<Symbol>,
}

impl<'a> Env<'a> {
    #[allow(clippy::too_many_arguments)]
    pub fn new(
        home: ModuleId,
        arena: &'a Bump,
        pool: &'a mut Pool,
        var_store: &'a mut VarStore,
        dep_idents: IdentIdsByModule,
        module_ids: &'a ModuleIds,
        exposed_ident_ids: IdentIds,
    ) -> Env<'a> {
        Env {
            home,
            arena,
            pool,
            problems: BumpVec::new_in(arena),
            var_store,
            dep_idents,
            module_ids,
            ident_ids: exposed_ident_ids.clone(), // we start with these, but will add more later using Scope.introduce
            exposed_ident_ids,
            closures: MutMap::default(),
            qualified_lookups: MutSet::default(),
            tailcallable_symbol: None,
            closure_name_symbol: None,
            top_level_symbols: MutSet::default(),
        }
    }

    pub fn add<T>(&mut self, item: T, region: Region) -> NodeId<T> {
        let id = self.pool.add(item);
        self.set_region(id, region);

        id
    }

    pub fn problem(&mut self, problem: Problem) {
        self.problems.push(problem);
    }

    pub fn set_region<T>(&mut self, _node_id: NodeId<T>, _region: Region) {
        dbg!("Don't Forget to set the region eventually");
    }

    pub fn register_closure(&mut self, symbol: Symbol, references: References) {
        self.closures.insert(symbol, references);
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

    /// Returns Err if the symbol resolved, but it was not exposed by the given module
    pub fn qualified_lookup(
        &mut self,
        module_name: &str,
        ident: &str,
        region: Region,
    ) -> Result<Symbol, RuntimeError> {
        debug_assert!(
            !module_name.is_empty(),
            "Called env.qualified_lookup with an unqualified ident: {ident:?}"
        );

        let module_name: ModuleName = module_name.into();

        match self.module_ids.get_id(&module_name) {
            Some(module_id) => {
                // You can do qualified lookups on your own module, e.g.
                // if I'm in the Foo module, I can do a `Foo.bar` lookup.
                if module_id == self.home {
                    match self.ident_ids.get_id(ident) {
                        Some(ident_id) => {
                            let symbol = Symbol::new(module_id, ident_id);

                            self.qualified_lookups.insert(symbol);

                            Ok(symbol)
                        }
                        None => Err(RuntimeError::LookupNotInScope {
                            loc_name: Loc {
                                value: Ident::from(ident),
                                region,
                            },
                            suggestion_options: self
                                .ident_ids
                                .ident_strs()
                                .map(|(_, string)| string.into())
                                .collect(),
                            underscored_suggestion_region: None,
                        }),
                    }
                } else {
                    match self.dep_idents.get(&module_id) {
                        Some(exposed_ids) => match exposed_ids.get_id(ident) {
                            Some(ident_id) => {
                                let symbol = Symbol::new(module_id, ident_id);

                                self.qualified_lookups.insert(symbol);

                                Ok(symbol)
                            }
                            None => {
                                let exposed_values = exposed_ids
                                    .ident_strs()
                                    .filter(|(_, ident)| {
                                        ident.starts_with(|c: char| c.is_lowercase())
                                    })
                                    .map(|(_, ident)| Lowercase::from(ident))
                                    .collect();
                                Err(RuntimeError::ValueNotExposed {
                                    module_name,
                                    ident: Ident::from(ident),
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
}
