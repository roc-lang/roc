use roc_builtins::std::StdLib;
use roc_can::abilities::AbilitiesStore;
use roc_can::constraint::{Constraint, Constraints};
use roc_can::def::Declaration;
use roc_collections::all::MutMap;
use roc_error_macros::internal_error;
use roc_module::symbol::{ModuleId, Symbol};
use roc_region::all::Loc;
use roc_types::solved_types::{FreeVars, SolvedType};
use roc_types::subs::{VarStore, Variable};

/// The types of all exposed values/functions of a collection of modules
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

    /// Convenient when you need mutable access to the StorageSubs in the ExposedModuleTypes
    pub fn get_mut(&mut self, module_id: &ModuleId) -> Option<&mut ExposedModuleTypes> {
        self.exposed.get_mut(module_id)
    }

    /// Create a clone of `self` that has just a subset of the modules
    ///
    /// Useful when we know what modules a particular module imports, and want just
    /// the exposed types for those exposed modules.
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
    pub imported_values: Vec<Symbol>,
}

impl ExposedForModule {
    pub fn new<'a>(
        it: impl Iterator<Item = &'a Symbol>,
        exposed_by_module: ExposedByModule,
    ) -> Self {
        let mut imported_values = Vec::new();

        for symbol in it {
            // Today, builtins are not actually imported,
            // but generated in each module that uses them
            //
            // This will change when we write builtins in roc
            if symbol.is_builtin() {
                continue;
            }

            if let Some(ExposedModuleTypes::Valid { .. }) =
                exposed_by_module.exposed.get(&symbol.module_id())
            {
                imported_values.push(*symbol);
            } else {
                continue;
            }
        }

        Self {
            imported_values,
            exposed_by_module,
        }
    }
}

/// The types of all exposed values/functions of a module
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
    abilities_store: &AbilitiesStore,
    declarations: &[Declaration],
    home: ModuleId,
) -> Constraint {
    let mut constraint = crate::expr::constrain_decls(constraints, home, declarations);

    for (member_name, member_data) in abilities_store.root_ability_members().iter() {
        constraint = constraints.let_constraint(
            [],
            [],
            [(*member_name, Loc::at_zero(member_data.signature.clone()))],
            Constraint::True,
            constraint,
        );
    }

    // The module constraint should always save the environment at the end.
    debug_assert!(constraints.contains_save_the_environment(&constraint));

    constraint
}

#[derive(Debug, Clone)]
pub struct Import {
    pub loc_symbol: Loc<Symbol>,
    pub solved_type: SolvedType,
}

pub fn introduce_builtin_imports(
    constraints: &mut Constraints,
    imports: Vec<Symbol>,
    body_con: Constraint,
    var_store: &mut VarStore,
) -> Constraint {
    let stdlib = roc_builtins::std::borrow_stdlib();
    let (rigid_vars, def_types) = constrain_builtin_imports(stdlib, imports, var_store);
    constraints.let_import_constraint(rigid_vars, def_types, body_con, &[])
}

pub fn constrain_builtin_imports(
    stdlib: &StdLib,
    imports: Vec<Symbol>,
    var_store: &mut VarStore,
) -> (Vec<Variable>, Vec<(Symbol, Loc<roc_types::types::Type>)>) {
    let mut def_types = Vec::new();
    let mut rigid_vars = Vec::new();

    for symbol in imports {
        let mut free_vars = FreeVars::default();

        let import = match stdlib.types.get(&symbol) {
            Some((solved_type, region)) => {
                let loc_symbol = Loc {
                    value: symbol,
                    region: *region,
                };

                Import {
                    loc_symbol,
                    solved_type: solved_type.clone(),
                }
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

                continue;
            }
        };

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
