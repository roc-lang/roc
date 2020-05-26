use crate::expr::constrain_decls;
use roc_builtins::std::Mode;
use roc_can::constraint::{Constraint, LetConstraint};
use roc_can::module::ModuleOutput;
use roc_collections::all::{ImMap, MutMap, MutSet, SendMap};
use roc_module::ident::Lowercase;
use roc_module::symbol::{ModuleId, Symbol};
use roc_region::all::Located;
use roc_types::boolean_algebra::{Atom, Bool};
use roc_types::solved_types::{BuiltinAlias, SolvedAtom, SolvedType};
use roc_types::subs::{VarId, VarStore, Variable};
use roc_types::types::{Alias, Type};

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
    var_store: &VarStore,
) -> Constraint {
    use Mode::*;

    let mut send_aliases = SendMap::default();

    for (symbol, alias) in module.aliases.iter() {
        send_aliases.insert(*symbol, alias.clone());
    }

    let decls = &module.declarations;

    match mode {
        Standard => constrain_decls(home, decls, send_aliases),
        Uniqueness => crate::uniq::constrain_decls(home, decls, send_aliases, &var_store),
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
    var_store: &VarStore,
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

pub fn load_builtin_aliases<I>(aliases: I, body_con: Constraint, var_store: &VarStore) -> Constraint
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

fn to_type(solved_type: &SolvedType, free_vars: &mut FreeVars, var_store: &VarStore) -> Type {
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
        Flex(var_id) => {
            if let Some(var) = free_vars.unnamed_vars.get(&var_id) {
                Type::Variable(*var)
            } else {
                let var = var_store.fresh();
                free_vars.unnamed_vars.insert(*var_id, var);

                Type::Variable(var)
            }
        }
        Wildcard => {
            let var = var_store.fresh();
            free_vars.wildcards.push(var);
            Type::Variable(var)
        }
        Record { fields, ext } => {
            let mut new_fields = SendMap::default();

            for (label, typ) in fields {
                new_fields.insert(label.clone(), to_type(&typ, free_vars, var_store));
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
        Boolean(solved_free, solved_rest) => {
            let free = to_atom(solved_free, free_vars, var_store);
            let mut rest = Vec::with_capacity(solved_rest.len());

            for solved_atom in solved_rest {
                rest.push(to_atom(solved_atom, free_vars, var_store));
            }

            Type::Boolean(Bool::from_parts(free, rest))
        }
        Alias(symbol, solved_type_variables, solved_actual) => {
            let mut type_variables = Vec::with_capacity(solved_type_variables.len());

            for (lowercase, solved_arg) in solved_type_variables {
                type_variables.push((lowercase.clone(), to_type(solved_arg, free_vars, var_store)));
            }

            let actual = to_type(solved_actual, free_vars, var_store);

            Type::Alias(*symbol, type_variables, Box::new(actual))
        }
        Error => Type::Erroneous(roc_types::types::Problem::SolvedTypeError),
        Erroneous(problem) => Type::Erroneous(problem.clone()),
    }
}

pub fn to_atom(solved_atom: &SolvedAtom, free_vars: &mut FreeVars, var_store: &VarStore) -> Atom {
    match solved_atom {
        SolvedAtom::Zero => Atom::Zero,
        SolvedAtom::One => Atom::One,
        SolvedAtom::Variable(var_id) => {
            if let Some(var) = free_vars.unnamed_vars.get(&var_id) {
                Atom::Variable(*var)
            } else {
                let var = var_store.fresh();
                free_vars.unnamed_vars.insert(*var_id, var);

                Atom::Variable(var)
            }
        }
    }
}

pub fn constrain_imported_aliases(
    aliases: MutMap<Symbol, Alias>,
    body_con: Constraint,
    var_store: &VarStore,
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
