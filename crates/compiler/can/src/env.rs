use crate::procedure::References;
use crate::scope::Scope;
use bumpalo::Bump;
use roc_collections::{MutMap, VecSet};
use roc_module::ident::{Ident, Lowercase, ModuleName};
use roc_module::symbol::{IdentIdsByModule, ModuleId, ModuleIds, Symbol};
use roc_problem::can::{Problem, RuntimeError};
use roc_region::all::{Loc, Region};

/// The canonicalization environment for a particular module.
pub struct Env<'a> {
    /// The module's path. Opaques and unqualified references to identifiers
    /// are assumed to be relative to this path.
    pub home: ModuleId,

    pub dep_idents: &'a IdentIdsByModule,

    pub module_ids: &'a ModuleIds,

    /// Problems we've encountered along the way, which will be reported to the user at the end.
    pub problems: Vec<Problem>,

    /// Closures
    pub closures: MutMap<Symbol, References>,

    /// current tail-callable symbol
    pub tailcallable_symbol: Option<Symbol>,

    /// Symbols of values/functions which were referenced by qualified lookups.
    pub qualified_value_lookups: VecSet<Symbol>,

    /// Symbols of types which were referenced by qualified lookups.
    pub qualified_type_lookups: VecSet<Symbol>,

    pub top_level_symbols: VecSet<Symbol>,

    pub arena: &'a Bump,
}

impl<'a> Env<'a> {
    pub fn new(
        arena: &'a Bump,
        home: ModuleId,
        dep_idents: &'a IdentIdsByModule,
        module_ids: &'a ModuleIds,
    ) -> Env<'a> {
        Env {
            arena,
            home,
            dep_idents,
            module_ids,
            problems: Vec::new(),
            closures: MutMap::default(),
            qualified_value_lookups: VecSet::default(),
            qualified_type_lookups: VecSet::default(),
            tailcallable_symbol: None,
            top_level_symbols: VecSet::default(),
        }
    }

    pub fn qualified_lookup(
        &mut self,
        scope: &Scope,
        module_name_str: &str,
        ident: &str,
        region: Region,
    ) -> Result<Symbol, RuntimeError> {
        debug_assert!(
            !module_name_str.is_empty(),
            "Called env.qualified_lookup with an unqualified ident: {ident:?}"
        );

        let module_name = ModuleName::from(module_name_str);

        match self.module_ids.get_id(&module_name) {
            Some(module_id) => self.qualified_lookup_help(scope, module_id, ident, region),
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

    pub fn qualified_lookup_with_module_id(
        &mut self,
        scope: &Scope,
        module_id: ModuleId,
        ident: &str,
        region: Region,
    ) -> Result<Symbol, RuntimeError> {
        self.qualified_lookup_help(scope, module_id, ident, region)
    }

    /// Returns Err if the symbol resolved, but it was not exposed by the given module
    fn qualified_lookup_help(
        &mut self,
        scope: &Scope,
        module_id: ModuleId,
        ident: &str,
        region: Region,
    ) -> Result<Symbol, RuntimeError> {
        let is_type_name = ident.starts_with(|c: char| c.is_uppercase());

        // You can do qualified lookups on your own module, e.g.
        // if I'm in the Foo module, I can do a `Foo.bar` lookup.
        if module_id == self.home {
            match scope.locals.ident_ids.get_id(ident) {
                Some(ident_id) => {
                    let symbol = Symbol::new(module_id, ident_id);

                    if is_type_name {
                        self.qualified_type_lookups.insert(symbol);
                    } else {
                        self.qualified_value_lookups.insert(symbol);
                    }

                    Ok(symbol)
                }
                None => {
                    let error = RuntimeError::LookupNotInScope {
                        loc_name: Loc {
                            value: Ident::from(ident),
                            region,
                        },
                        suggestion_options: scope
                            .locals
                            .ident_ids
                            .ident_strs()
                            .map(|(_, string)| string.into())
                            .collect(),
                        underscored_suggestion_region: None,
                    };
                    Err(error)
                }
            }
        } else {
            match self.dep_idents.get(&module_id) {
                Some(exposed_ids) => match exposed_ids.get_id(ident) {
                    Some(ident_id) => {
                        let symbol = Symbol::new(module_id, ident_id);

                        if is_type_name {
                            self.qualified_type_lookups.insert(symbol);
                        } else {
                            self.qualified_value_lookups.insert(symbol);
                        }

                        Ok(symbol)
                    }
                    None => {
                        let exposed_values = exposed_ids
                            .ident_strs()
                            .filter(|(_, ident)| ident.starts_with(|c: char| c.is_lowercase()))
                            .map(|(_, ident)| Lowercase::from(ident))
                            .collect();
                        Err(RuntimeError::ValueNotExposed {
                            module_name: self
                                .module_ids
                                .get_name(module_id)
                                .expect("Module ID known, but not in the module IDs somehow")
                                .clone(),
                            ident: Ident::from(ident),
                            region,
                            exposed_values,
                        })
                    }
                },
                None => Err(RuntimeError::ModuleNotImported {
                    module_name: self
                        .module_ids
                        .get_name(module_id)
                        .expect("Module ID known, but not in the module IDs somehow")
                        .clone(),
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

    pub fn problem(&mut self, problem: Problem) {
        self.problems.push(problem)
    }
}
