use crate::expr::{constrain_def_make_constraint, constrain_def_pattern, Env};
use roc_builtins::std::StdLib;
use roc_can::abilities::{AbilitiesStore, MemberTypeInfo, SolvedSpecializations};
use roc_can::constraint::{Constraint, Constraints};
use roc_can::def::Declaration;
use roc_can::expected::Expected;
use roc_can::pattern::Pattern;
use roc_collections::all::MutMap;
use roc_error_macros::internal_error;
use roc_module::symbol::{ModuleId, Symbol};
use roc_region::all::{Loc, Region};
use roc_types::solved_types::{FreeVars, SolvedType};
use roc_types::subs::{VarStore, Variable};
use roc_types::types::{AnnotationSource, Category, Type};

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
            let module = exposed_by_module.exposed.get(&symbol.module_id());
            if let Some(ExposedModuleTypes { .. }) = module {
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
pub struct ExposedModuleTypes {
    pub stored_vars_by_symbol: Vec<(Symbol, Variable)>,
    pub storage_subs: roc_types::subs::StorageSubs,
    pub solved_specializations: SolvedSpecializations,
}

pub fn constrain_module(
    constraints: &mut Constraints,
    symbols_from_requires: Vec<(Loc<Symbol>, Loc<Type>)>,
    abilities_store: &AbilitiesStore,
    declarations: &[Declaration],
    home: ModuleId,
) -> Constraint {
    let constraint = crate::expr::constrain_decls(constraints, home, declarations);
    let constraint =
        constrain_symbols_from_requires(constraints, symbols_from_requires, home, constraint);
    let constraint = frontload_ability_constraints(constraints, abilities_store, home, constraint);

    // The module constraint should always save the environment at the end.
    debug_assert!(constraints.contains_save_the_environment(&constraint));

    constraint
}

fn constrain_symbols_from_requires(
    constraints: &mut Constraints,
    symbols_from_requires: Vec<(Loc<Symbol>, Loc<Type>)>,
    home: ModuleId,
    constraint: Constraint,
) -> Constraint {
    symbols_from_requires
        .into_iter()
        .fold(constraint, |constraint, (loc_symbol, loc_type)| {
            if loc_symbol.value.module_id() == home {
                // 1. Required symbols can only be specified in package modules
                // 2. Required symbols come from app modules
                // But, if we are running e.g. `roc check` on a package module, there is no app
                // module, and we will have instead put the required symbols in the package module
                // namespace. If this is the case, we want to introduce the symbols as if they had
                // the types they are annotated with.
                let rigids = Default::default();
                let mut env = Env {
                    home,
                    rigids,
                    resolutions_to_make: vec![],
                };
                let pattern = Loc::at_zero(roc_can::pattern::Pattern::Identifier(loc_symbol.value));

                let def_pattern_state =
                    constrain_def_pattern(constraints, &mut env, &pattern, loc_type.value);

                debug_assert!(env.resolutions_to_make.is_empty());

                constrain_def_make_constraint(
                    constraints,
                    // No new rigids or flex vars because they are represented in the type
                    // annotation.
                    std::iter::empty(),
                    std::iter::empty(),
                    Constraint::True,
                    constraint,
                    def_pattern_state,
                )
            } else {
                // Otherwise, this symbol comes from an app module - we want to check that the type
                // provided by the app is in fact what the package module requires.
                let arity = loc_type.value.arity();
                let provided_eq_requires_constr = constraints.lookup(
                    loc_symbol.value,
                    Expected::FromAnnotation(
                        loc_symbol.map(|&s| Pattern::Identifier(s)),
                        arity,
                        AnnotationSource::RequiredSymbol {
                            region: loc_type.region,
                        },
                        loc_type.value,
                    ),
                    loc_type.region,
                );
                constraints.and_constraint([provided_eq_requires_constr, constraint])
            }
        })
}

pub fn frontload_ability_constraints(
    constraints: &mut Constraints,
    abilities_store: &AbilitiesStore,
    home: ModuleId,
    mut constraint: Constraint,
) -> Constraint {
    for (member_name, member_data) in abilities_store.root_ability_members().iter() {
        if let MemberTypeInfo::Local {
            signature_var,
            variables: vars,
            signature,
        } = &member_data.typ
        {
            let rigids = Default::default();
            let mut env = Env {
                home,
                rigids,
                resolutions_to_make: vec![],
            };
            let pattern = Loc::at_zero(roc_can::pattern::Pattern::Identifier(*member_name));

            let mut def_pattern_state = constrain_def_pattern(
                constraints,
                &mut env,
                &pattern,
                Type::Variable(member_data.signature_var),
            );

            debug_assert!(env.resolutions_to_make.is_empty());

            def_pattern_state.vars.push(member_data.signature_var);

            let vars = &member_data.variables;
            let rigid_variables = vars.rigid_vars.iter().chain(vars.able_vars.iter()).copied();
            let infer_variables = vars.flex_vars.iter().copied();

            def_pattern_state
                .constraints
                .push(constraints.equal_types_var(
                    member_data.signature_var,
                    Expected::NoExpectation(member_data.signature.clone()),
                    Category::Storage(file!(), line!()),
                    Region::zero(),
                ));

            constraint = constrain_def_make_constraint(
                constraints,
                rigid_variables,
                infer_variables,
                Constraint::True,
                constraint,
                def_pattern_state,
            );
        }
    }
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
