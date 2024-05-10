use std::path::Path;

use crate::procedure::References;
use crate::scope::Scope;
use bumpalo::Bump;
use roc_collections::{MutMap, VecSet};
use roc_module::ident::{Ident, ModuleName};
use roc_module::symbol::{
    IdentIdsByModule, LookedupModule, LookedupSymbol, ModuleId, PQModuleName, PackageModuleIds,
    Symbol,
};
use roc_problem::can::{Problem, RuntimeError};
use roc_region::all::{Loc, Region};

/// The canonicalization environment for a particular module.
pub struct Env<'a> {
    /// The module's path. Opaques and unqualified references to identifiers
    /// are assumed to be relative to this path.
    pub home: ModuleId,

    pub module_path: &'a Path,

    pub dep_idents: &'a IdentIdsByModule,

    pub qualified_module_ids: &'a PackageModuleIds<'a>,

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

    pub opt_shorthand: Option<&'a str>,
}

impl<'a> Env<'a> {
    pub fn new(
        arena: &'a Bump,
        home: ModuleId,
        module_path: &'a Path,
        dep_idents: &'a IdentIdsByModule,
        qualified_module_ids: &'a PackageModuleIds<'a>,
        opt_shorthand: Option<&'a str>,
    ) -> Env<'a> {
        Env {
            arena,
            home,
            module_path,
            dep_idents,
            qualified_module_ids,
            problems: Vec::new(),
            closures: MutMap::default(),
            qualified_value_lookups: VecSet::default(),
            qualified_type_lookups: VecSet::default(),
            tailcallable_symbol: None,
            top_level_symbols: VecSet::default(),
            opt_shorthand,
        }
    }

    pub fn qualified_lookup(
        &mut self,
        scope: &Scope,
        module_name_str: &str,
        ident: &str,
        region: Region,
    ) -> Result<LookedupSymbol, RuntimeError> {
        debug_assert!(
            !module_name_str.is_empty(),
            "Called env.qualified_lookup with an unqualified ident: {ident:?}"
        );

        let module_name = ModuleName::from(module_name_str);

        match scope.modules.lookup(&module_name) {
            Some(lookedup_module) => {
                self.qualified_lookup_help(scope, lookedup_module, ident, region)
            }
            None => Err(RuntimeError::ModuleNotImported {
                module_name: module_name.clone(),
                imported_modules: scope
                    .modules
                    .available_names()
                    .map(|string| string.as_ref().into())
                    .collect(),
                region,
                module_exists: self
                    .qualified_module_ids
                    .get_id(&PQModuleName::Unqualified(module_name))
                    .is_some(),
            }),
        }
    }

    pub fn qualified_lookup_with_module_id(
        &mut self,
        scope: &Scope,
        module_id: ModuleId,
        ident: &str,
        region: Region,
    ) -> Result<LookedupSymbol, RuntimeError> {
        if let Some(module) = scope.modules.lookup_by_id(&module_id) {
            self.qualified_lookup_help(scope, module, ident, region)
        } else {
            Err(self.module_exists_but_not_imported(scope, module_id, region))
        }
    }

    /// Returns Err if the symbol resolved, but it was not exposed by the given module
    fn qualified_lookup_help(
        &mut self,
        scope: &Scope,
        module: LookedupModule,
        ident: &str,
        region: Region,
    ) -> Result<LookedupSymbol, RuntimeError> {
        let is_type_name = ident.starts_with(|c: char| c.is_uppercase());

        // You can do qualified lookups on your own module, e.g.
        // if I'm in the Foo module, I can do a `Foo.bar` lookup.
        if module.id == self.home {
            match scope.locals.ident_ids.get_id(ident) {
                Some(ident_id) => {
                    let symbol = Symbol::new(module.id, ident_id);

                    if is_type_name {
                        self.qualified_type_lookups.insert(symbol);
                    } else {
                        self.qualified_value_lookups.insert(symbol);
                    }

                    Ok(LookedupSymbol::no_params(symbol))
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
            match self.dep_idents.get(&module.id) {
                Some(exposed_ids) => match exposed_ids.get_id(ident) {
                    Some(ident_id) => {
                        let symbol = Symbol::new(module.id, ident_id);

                        if is_type_name {
                            self.qualified_type_lookups.insert(symbol);
                        } else {
                            self.qualified_value_lookups.insert(symbol);
                        }

                        Ok(module.into_symbol(symbol))
                    }
                    None => Err(RuntimeError::ValueNotExposed {
                        module_name: self
                            .qualified_module_ids
                            .get_name(module.id)
                            .expect("Module ID known, but not in the module IDs somehow")
                            .as_inner()
                            .clone(),
                        ident: Ident::from(ident),
                        region,
                        exposed_values: exposed_ids.exposed_values(),
                    }),
                },
                _ => Err(self.module_exists_but_not_imported(scope, module.id, region)),
            }
        }
    }

    fn module_exists_but_not_imported(
        &self,
        scope: &Scope,
        module_id: ModuleId,
        region: Region,
    ) -> RuntimeError {
        RuntimeError::ModuleNotImported {
            module_name: self
                .qualified_module_ids
                .get_name(module_id)
                .expect("Module ID known, but not in the module IDs somehow")
                .as_inner()
                .clone(),
            imported_modules: scope
                .modules
                .available_names()
                .map(|string| string.as_ref().into())
                .collect(),
            region,
            module_exists: true,
        }
    }

    pub fn problem(&mut self, problem: Problem) {
        self.problems.push(problem)
    }
}
