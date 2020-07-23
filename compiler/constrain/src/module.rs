use crate::expr::constrain_decls;
use roc_builtins::std::{Mode, StdLib};
use roc_can::constraint::{Constraint, LetConstraint};
use roc_can::module::ModuleOutput;
use roc_collections::all::{ImMap, MutMap, MutSet, SendMap};
use roc_module::ident::Lowercase;
use roc_module::symbol::{ModuleId, Symbol};
use roc_region::all::{Located, Region};
use roc_types::boolean_algebra::Bool;
use roc_types::solved_types::{BuiltinAlias, SolvedBool, SolvedType};
use roc_types::subs::{VarId, VarStore, Variable};
use roc_types::types::{Alias, Problem, RecordField, Type};

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
        Standard => constrain_decls(home, decls, send_aliases),
        Uniqueness => crate::uniq::constrain_decls(home, decls, send_aliases, var_store),
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
                let typ = to_type(&import.solved_type, &mut free_vars, var_store);

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
            def_aliases: SendMap::default(),
            defs_constraint: True,
            ret_constraint: body_con,
        })),
    )
}

pub fn load_builtin_aliases<I>(
    aliases: I,
    body_con: Constraint,
    var_store: &mut VarStore,
) -> Constraint
where
    I: IntoIterator<Item = (Symbol, BuiltinAlias)>,
{
    use Constraint::*;

    // Load all the given builtin aliases.
    let mut def_aliases = SendMap::default();

    for (symbol, builtin_alias) in aliases {
        let mut free_vars = FreeVars::default();

        let actual = to_type(&builtin_alias.typ, &mut free_vars, var_store);

        let mut vars = Vec::with_capacity(builtin_alias.vars.len());

        for (loc_lowercase, index) in builtin_alias.vars.iter().zip(1..) {
            let var = if let Some(result) = free_vars.unnamed_vars.get(&VarId::from_u32(index)) {
                result
            } else {
                panic!(
                    "var_id {:?} was not instantiated in the body of {:?} : {:?} (is it phantom?)",
                    index, symbol, &builtin_alias
                )
            };

            vars.push(Located::at(
                loc_lowercase.region,
                (loc_lowercase.value.clone(), *var),
            ));
        }

        let alias = Alias {
            vars,
            region: builtin_alias.region,
            uniqueness: None,
            typ: actual,
        };

        def_aliases.insert(symbol, alias);
    }

    Let(Box::new(LetConstraint {
        rigid_vars: Vec::new(),
        flex_vars: Vec::new(),
        def_types: SendMap::default(),
        def_aliases,
        defs_constraint: True,
        ret_constraint: body_con,
    }))
}

#[derive(Debug, Clone, Default)]
pub struct FreeVars {
    pub named_vars: ImMap<Lowercase, Variable>,
    pub unnamed_vars: ImMap<VarId, Variable>,
    pub wildcards: Vec<Variable>,
}

fn to_type(solved_type: &SolvedType, free_vars: &mut FreeVars, var_store: &mut VarStore) -> Type {
    use roc_types::solved_types::SolvedType::*;

    match solved_type {
        Func(args, ret) => {
            let mut new_args = Vec::with_capacity(args.len());

            for arg in args {
                new_args.push(to_type(&arg, free_vars, var_store));
            }

            let new_ret = to_type(&ret, free_vars, var_store);

            Type::Function(new_args, Box::new(new_ret))
        }
        Apply(symbol, args) => {
            let mut new_args = Vec::with_capacity(args.len());

            for arg in args {
                new_args.push(to_type(&arg, free_vars, var_store));
            }

            Type::Apply(*symbol, new_args)
        }
        Rigid(lowercase) => {
            if let Some(var) = free_vars.named_vars.get(&lowercase) {
                Type::Variable(*var)
            } else {
                let var = var_store.fresh();
                free_vars.named_vars.insert(lowercase.clone(), var);
                Type::Variable(var)
            }
        }
        Flex(var_id) => Type::Variable(var_id_to_flex_var(*var_id, free_vars, var_store)),
        Wildcard => {
            let var = var_store.fresh();
            free_vars.wildcards.push(var);
            Type::Variable(var)
        }
        Record { fields, ext } => {
            use RecordField::*;

            let mut new_fields = SendMap::default();

            for (label, field) in fields {
                let field_val = match field {
                    Required(typ) => Required(to_type(&typ, free_vars, var_store)),
                    Optional(typ) => Optional(to_type(&typ, free_vars, var_store)),
                    Demanded(typ) => Demanded(to_type(&typ, free_vars, var_store)),
                };

                new_fields.insert(label.clone(), field_val);
            }

            Type::Record(new_fields, Box::new(to_type(ext, free_vars, var_store)))
        }
        EmptyRecord => Type::EmptyRec,
        EmptyTagUnion => Type::EmptyTagUnion,
        TagUnion(tags, ext) => {
            let mut new_tags = Vec::with_capacity(tags.len());

            for (tag_name, args) in tags {
                let mut new_args = Vec::with_capacity(args.len());

                for arg in args.iter() {
                    new_args.push(to_type(arg, free_vars, var_store));
                }

                new_tags.push((tag_name.clone(), new_args));
            }

            Type::TagUnion(new_tags, Box::new(to_type(ext, free_vars, var_store)))
        }
        RecursiveTagUnion(rec_var_id, tags, ext) => {
            let mut new_tags = Vec::with_capacity(tags.len());

            for (tag_name, args) in tags {
                let mut new_args = Vec::with_capacity(args.len());

                for arg in args.iter() {
                    new_args.push(to_type(arg, free_vars, var_store));
                }

                new_tags.push((tag_name.clone(), new_args));
            }

            let rec_var = free_vars
                .unnamed_vars
                .get(rec_var_id)
                .expect("rec var not in unnamed vars");

            Type::RecursiveTagUnion(
                *rec_var,
                new_tags,
                Box::new(to_type(ext, free_vars, var_store)),
            )
        }
        Boolean(SolvedBool::SolvedShared) => Type::Boolean(Bool::Shared),
        Boolean(SolvedBool::SolvedContainer(solved_cvar, solved_mvars)) => {
            let cvar = var_id_to_flex_var(*solved_cvar, free_vars, var_store);

            let mvars = solved_mvars
                .iter()
                .map(|var_id| var_id_to_flex_var(*var_id, free_vars, var_store));

            Type::Boolean(Bool::container(cvar, mvars))
        }
        Alias(symbol, solved_type_variables, solved_actual) => {
            let mut type_variables = Vec::with_capacity(solved_type_variables.len());

            for (lowercase, solved_arg) in solved_type_variables {
                type_variables.push((lowercase.clone(), to_type(solved_arg, free_vars, var_store)));
            }

            let actual = to_type(solved_actual, free_vars, var_store);

            Type::Alias(*symbol, type_variables, Box::new(actual))
        }
        Error => Type::Erroneous(Problem::SolvedTypeError),
        Erroneous(problem) => Type::Erroneous(problem.clone()),
    }
}

fn var_id_to_flex_var(
    var_id: VarId,
    free_vars: &mut FreeVars,
    var_store: &mut VarStore,
) -> Variable {
    if let Some(var) = free_vars.unnamed_vars.get(&var_id) {
        *var
    } else {
        let var = var_store.fresh();
        free_vars.unnamed_vars.insert(var_id, var);

        var
    }
}

pub fn constrain_imported_aliases(
    aliases: MutMap<Symbol, Alias>,
    body_con: Constraint,
    var_store: &mut VarStore,
) -> Constraint {
    use Constraint::*;
    let mut def_aliases = SendMap::default();

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

        let alias = Alias {
            vars,
            region: imported_alias.region,
            uniqueness: imported_alias.uniqueness,
            typ: actual,
        };

        def_aliases.insert(symbol, alias);
    }

    Let(Box::new(LetConstraint {
        rigid_vars: Vec::new(),
        flex_vars: Vec::new(),
        def_types: SendMap::default(),
        def_aliases,
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
                        || stdlib.aliases.contains_key(&symbol);

                    if !is_valid_alias {
                        panic!(
                            "Could not find {:?} in builtin types {:?} or aliases {:?}",
                            symbol, stdlib.types, stdlib.aliases
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
