use roc_builtins::std::StdLib;
use roc_can::constraint::{Constraint, Constraints};
use roc_can::def::Declaration;
use roc_collections::all::{MutMap, MutSet};
use roc_error_macros::internal_error;
use roc_module::symbol::{ModuleId, Symbol};
use roc_region::all::{Loc, Region};
use roc_types::solved_types::{FreeVars, SolvedType};
use roc_types::subs::{VarStore, Variable};
use roc_types::types::{Alias, Problem};

#[derive(Clone, Debug, Default)]
pub struct ExposedByModule {
    exposed: MutMap<ModuleId, ExposedModuleTypes>,
}

impl ExposedByModule {
    pub fn insert(&mut self, module_id: ModuleId, exposed: ExposedModuleTypes) {
        self.exposed.insert(module_id, exposed);
    }

    pub fn get(&self, module_id: &ModuleId) -> Option<&ExposedModuleTypes> {
        self.exposed.get(module_id)
    }

    pub fn get_mut(&mut self, module_id: &ModuleId) -> Option<&mut ExposedModuleTypes> {
        self.exposed.get_mut(module_id)
    }

    pub fn retain_modules<'a>(&self, it: impl Iterator<Item = &'a ModuleId>) -> Self {
        let mut output = Self::default();

        for module_id in it {
            match self.exposed.get(module_id) {
                None => {
                    internal_error!("Module {:?} did not register its exposed values", module_id)
                }
                Some(exposed_types) => {
                    output.exposed.insert(*module_id, exposed_types.clone());
                }
            }
        }

        output
    }
}

#[derive(Clone, Debug, Default)]
pub struct ExposedForModule {
    pub exposed_by_module: ExposedByModule,
    pub imported_symbols: Vec<Symbol>,
}

impl ExposedForModule {
    pub fn new<'a>(
        it: impl Iterator<Item = &'a Symbol>,
        exposed_by_module: ExposedByModule,
    ) -> Self {
        let mut imported_symbols = Vec::new();

        for symbol in it {
            if symbol.is_builtin() {
                continue;
            }

            if let Some(ExposedModuleTypes::Valid { .. }) =
                exposed_by_module.exposed.get(&symbol.module_id())
            {
                imported_symbols.push(*symbol);
            } else {
                continue;
            }
        }

        Self {
            imported_symbols,
            exposed_by_module,
        }
    }
}

#[derive(Clone, Debug)]
pub enum ExposedModuleTypes {
    Invalid,
    Valid {
        stored_vars_by_symbol: Vec<(Symbol, Variable)>,
        storage_subs: roc_types::subs::StorageSubs,
    },
}

pub fn constrain_module(
    constraints: &mut Constraints,
    declarations: &[Declaration],
    home: ModuleId,
) -> Constraint {
    crate::expr::constrain_decls(constraints, home, declarations)
}

#[derive(Debug, Clone)]
pub struct Import {
    pub loc_symbol: Loc<Symbol>,
    pub solved_type: SolvedType,
}

pub fn introduce_builtin_imports(
    constraints: &mut Constraints,
    imports: Vec<Import>,
    body_con: Constraint,
    var_store: &mut VarStore,
) -> Constraint {
    let (rigid_vars, def_types) = constrain_imports(imports, var_store);
    constraints.let_import_constraint(rigid_vars, def_types, body_con, &[])
}

pub fn constrain_imports(
    imports: Vec<Import>,
    var_store: &mut VarStore,
) -> (Vec<Variable>, Vec<(Symbol, Loc<roc_types::types::Type>)>) {
    let mut def_types = Vec::new();
    let mut rigid_vars = Vec::new();

    for import in imports {
        let mut free_vars = FreeVars::default();
        let loc_symbol = import.loc_symbol;

        // an imported symbol can be either an alias or a value
        match import.solved_type {
            SolvedType::Alias(symbol, _, _, _, _) if symbol == loc_symbol.value => {
                // do nothing, in the future the alias definitions should not be in the list of imported values
            }
            _ => {
                let typ = roc_types::solved_types::to_type(
                    &import.solved_type,
                    &mut free_vars,
                    var_store,
                );

                def_types.push((
                    loc_symbol.value,
                    Loc {
                        region: loc_symbol.region,
                        value: typ,
                    },
                ));

                for (_, var) in free_vars.named_vars {
                    rigid_vars.push(var);
                }

                for var in free_vars.wildcards {
                    rigid_vars.push(var);
                }

                // Variables can lose their name during type inference. But the unnamed
                // variables are still part of a signature, and thus must be treated as rigids here!
                for (_, var) in free_vars.unnamed_vars {
                    rigid_vars.push(var);
                }
            }
        }
    }

    (rigid_vars, def_types)
}

pub struct ConstrainableImports {
    pub imported_symbols: Vec<Import>,
    pub imported_aliases: MutMap<Symbol, Alias>,
    pub unused_imports: MutMap<ModuleId, Region>,
}

/// Run this before constraining imports.
///
/// Constraining imports is split into two different functions, because this
/// part of the work needs to be done on the main thread, whereas the rest of it
/// can be done on a different thread.
pub fn pre_constrain_imports(
    home: ModuleId,
    references: &MutSet<Symbol>,
    imported_modules: MutMap<ModuleId, Region>,
    exposed_types: &mut ExposedByModule,
    stdlib: &StdLib,
) -> ConstrainableImports {
    let mut imported_symbols = Vec::with_capacity(references.len());
    let mut imported_aliases = MutMap::default();
    let mut unused_imports = imported_modules; // We'll remove these as we encounter them.

    // Translate referenced symbols into constraints. We do this on the main
    // thread because we need exclusive access to the exposed_types map, in order
    // to get the necessary constraint info for any aliases we imported. We also
    // resolve builtin types now, so we can use a reference to stdlib instead of
    // having to either clone it or recreate it from scratch on the other thread.
    for &symbol in references.iter() {
        let module_id = symbol.module_id();

        // We used this module, so clearly it is not unused!
        unused_imports.remove(&module_id);

        if module_id.is_builtin() {
            // For builtin modules, we create imports from the
            // hardcoded builtin map.
            match stdlib.types.get(&symbol) {
                Some((solved_type, region)) => {
                    let loc_symbol = Loc {
                        value: symbol,
                        region: *region,
                    };

                    imported_symbols.push(Import {
                        loc_symbol,
                        solved_type: solved_type.clone(),
                    });
                }
                None => {
                    let is_valid_alias = stdlib.applies.contains(&symbol)
                        // This wasn't a builtin value or Apply; maybe it was a builtin alias.
                        || roc_types::builtin_aliases::aliases().contains_key(&symbol);

                    if !is_valid_alias {
                        panic!(
                            "Could not find {:?} in builtin types {:?} or builtin aliases",
                            symbol, stdlib.types,
                        );
                    }
                }
            }
        } else if module_id != home {
            // We already have constraints for our own symbols.
            let region = Region::zero(); // TODO this should be the region where this symbol was declared in its home module. Look that up!
            let loc_symbol = Loc {
                value: symbol,
                region,
            };

            match exposed_types.get(&module_id) {
                Some(ExposedModuleTypes::Valid {
                    storage_subs,
                    stored_vars_by_symbol,
                }) => {
                    // do nothing, basically
                }
                Some(ExposedModuleTypes::Invalid) => {
                    // If that module was invalid, use True constraints
                    // for everything imported from it.
                    imported_symbols.push(Import {
                        loc_symbol,
                        solved_type: SolvedType::Erroneous(Problem::InvalidModule),
                    });

                    // TODO what about storage subs here?
                }
                None => {
                    panic!(
                        "Could not find module {:?} in exposed_types {:?}",
                        module_id, exposed_types
                    );
                }
            }
        }
    }

    ConstrainableImports {
        imported_symbols,
        imported_aliases,
        unused_imports,
    }
}
