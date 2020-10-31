use crate::expr::constrain_decls;
use roc_builtins::std::{Mode, StdLib};
use roc_can::constraint::{Constraint, LetConstraint};
use roc_can::module::ModuleOutput;
use roc_collections::all::{ImMap, MutMap, MutSet, SendMap};
use roc_module::symbol::{ModuleId, Symbol};
use roc_region::all::{Located, Region};
use roc_types::solved_types::{BuiltinAlias, FreeVars, SolvedType};
use roc_types::subs::{VarId, VarStore, Variable};
use roc_types::types::{Alias, Problem, Type};

pub type SubsByModule = MutMap<ModuleId, ExposedModuleTypes>;

#[derive(Clone, Debug)]
pub enum ExposedModuleTypes {
    Invalid,
    Valid(MutMap<Symbol, SolvedType>, MutMap<Symbol, Alias>),
}

pub struct ConstrainedModule {
    pub unused_imports: MutSet<ModuleId>,
    pub constraint: Constraint,
}

pub fn constrain_module(
    module: &ModuleOutput,
    home: ModuleId,
    mode: Mode,
    var_store: &mut VarStore,
) -> Constraint {
    use Mode::*;

    let mut send_aliases = SendMap::default();

    for (symbol, alias) in module.aliases.iter() {
        send_aliases.insert(*symbol, alias.clone());
    }

    let decls = &module.declarations;

    match mode {
        Standard => constrain_decls(home, decls),
        Uniqueness => crate::uniq::constrain_decls(home, decls, var_store),
    }
}

#[derive(Debug, Clone)]
pub struct Import {
    pub loc_symbol: Located<Symbol>,
    pub solved_type: SolvedType,
}

pub fn constrain_imported_values(
    imports: Vec<Import>,
    body_con: Constraint,
    var_store: &mut VarStore,
) -> (Vec<Variable>, Constraint) {
    use Constraint::*;
    let mut def_types = SendMap::default();
    let mut rigid_vars = Vec::new();

    for import in imports {
        let mut free_vars = FreeVars::default();
        let loc_symbol = import.loc_symbol;

        // an imported symbol can be either an alias or a value
        match import.solved_type {
            SolvedType::Alias(symbol, _, _) if symbol == loc_symbol.value => {
                // do nothing, in the future the alias definitions should not be in the list of imported values
            }
            _ => {
                let typ = roc_types::solved_types::to_type(
                    &import.solved_type,
                    &mut free_vars,
                    var_store,
                );

                def_types.insert(
                    loc_symbol.value,
                    Located {
                        region: loc_symbol.region,
                        value: typ,
                    },
                );

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

    (
        rigid_vars.clone(),
        Let(Box::new(LetConstraint {
            rigid_vars,
            flex_vars: Vec::new(),
            def_types,
            defs_constraint: True,
            ret_constraint: body_con,
        })),
    )
}

pub fn constrain_imported_aliases(
    aliases: MutMap<Symbol, Alias>,
    body_con: Constraint,
    var_store: &mut VarStore,
) -> Constraint {
    use Constraint::*;

    for (symbol, imported_alias) in aliases {
        let mut vars = Vec::with_capacity(imported_alias.vars.len());
        let mut substitution = ImMap::default();

        for Located {
            region,
            value: (lowercase, old_var),
        } in &imported_alias.vars
        {
            let new_var = var_store.fresh();
            vars.push(Located::at(*region, (lowercase.clone(), new_var)));
            substitution.insert(*old_var, Type::Variable(new_var));
        }

        let mut actual = imported_alias.typ.clone();

        actual.substitute(&substitution);

        let mut hidden_variables = MutSet::default();
        hidden_variables.extend(actual.variables());

        for loc_var in vars.iter() {
            hidden_variables.remove(&loc_var.value.1);
        }

        let alias = Alias {
            vars,
            hidden_variables,
            region: imported_alias.region,
            uniqueness: imported_alias.uniqueness,
            typ: actual,
        };
    }

    Let(Box::new(LetConstraint {
        rigid_vars: Vec::new(),
        flex_vars: Vec::new(),
        def_types: SendMap::default(),
        defs_constraint: True,
        ret_constraint: body_con,
    }))
}

/// Run pre_constrain_imports to get imported_symbols and imported_aliases.
pub fn constrain_imports(
    imported_symbols: Vec<Import>,
    imported_aliases: MutMap<Symbol, Alias>,
    constraint: Constraint,
    var_store: &mut VarStore,
) -> Constraint {
    let (_introduced_rigids, constraint) =
        constrain_imported_values(imported_symbols, constraint, var_store);

    // TODO determine what to do with those rigids
    //    for var in introduced_rigids {
    //        output.ftv.insert(var, format!("internal_{:?}", var).into());
    //    }

    constrain_imported_aliases(imported_aliases, constraint, var_store)
}

pub struct ConstrainableImports {
    pub imported_symbols: Vec<Import>,
    pub imported_aliases: MutMap<Symbol, Alias>,
    pub unused_imports: MutSet<ModuleId>,
}

/// Run this before constraining imports.
///
/// Constraining imports is split into two different functions, because this
/// part of the work needs to be done on the main thread, whereas the rest of it
/// can be done on a different thread.
pub fn pre_constrain_imports(
    home: ModuleId,
    references: &MutSet<Symbol>,
    imported_modules: MutSet<ModuleId>,
    exposed_types: &mut SubsByModule,
    stdlib: &StdLib,
) -> ConstrainableImports {
    let mut imported_symbols = Vec::with_capacity(references.len());
    let mut imported_aliases = MutMap::default();
    let mut unused_imports = imported_modules; // We'll remove these as we encounter them.

    // Translate referenced symbols into constraints. We do this on the main
    // thread because we need exclusive access to the exposed_types map, in order
    // to get the necessary constraint info for any aliases we imported. We also
    // resolve builtin types now, so we can use a refernce to stdlib instead of
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
                    let loc_symbol = Located {
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
            let loc_symbol = Located {
                value: symbol,
                region,
            };

            match exposed_types.get(&module_id) {
                Some(ExposedModuleTypes::Valid(solved_types, new_aliases)) => {
                    let solved_type = solved_types.get(&symbol).unwrap_or_else(|| {
                        panic!(
                            "Could not find {:?} in solved_types {:?}",
                            loc_symbol.value, solved_types
                        )
                    });

                    // TODO should this be a union?
                    for (k, v) in new_aliases.clone() {
                        imported_aliases.insert(k, v);
                    }

                    imported_symbols.push(Import {
                        loc_symbol,
                        solved_type: solved_type.clone(),
                    });
                }
                Some(ExposedModuleTypes::Invalid) => {
                    // If that module was invalid, use True constraints
                    // for everything imported from it.
                    imported_symbols.push(Import {
                        loc_symbol,
                        solved_type: SolvedType::Erroneous(Problem::InvalidModule),
                    });
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
