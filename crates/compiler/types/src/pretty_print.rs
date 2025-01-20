#![allow(clippy::too_many_arguments)]

use crate::subs::{
    self, AliasVariables, Content, FlatType, GetSubsSlice, Label, Subs, UnionLabels,
    UnsortedUnionLabels, Variable,
};
use crate::types::{
    name_type_var, name_type_var_with_hint, AbilitySet, Polarity, RecordField, Uls,
};
use roc_collections::all::MutMap;
use roc_collections::VecSet;
use roc_module::ident::{Lowercase, TagName};
use roc_module::symbol::{Interns, ModuleId, Symbol};

pub static WILDCARD: &str = "*";
static EMPTY_RECORD: &str = "{}";
static EMPTY_TAG_UNION: &str = "[]";
static EFFECTFUL_FUNC: &str = "! : ... => ?";

/// Requirements for parentheses.
///
/// If we're inside a function (that is, this is either an argument or a return
/// value), we may need to use parens. Examples:
///
/// a -> (* -> a)
/// (* -> a) -> a
///
/// Separately, if we're inside a type parameter, we may need to use parens:
///
/// List I64
/// List (List I64)
///
/// Otherwise, parens are unnecessary.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Parens {
    InFn,
    InTypeParam,
    Unnecessary,
}

macro_rules! write_parens {
    ($insert_parens:expr, $buf:expr, $body:expr) => {{
        if $insert_parens {
            $buf.push('(');
        }

        $body

        if $insert_parens {
            $buf.push(')');
        }
    }
    };
}

#[derive(Clone, Copy)]
pub struct DebugPrint {
    pub print_lambda_sets: bool,
    pub print_only_under_alias: bool,
    pub print_ranks: bool,
    pub ignore_polarity: bool,
    pub print_weakened_vars: bool,
    pub print_variables: bool,
}

impl DebugPrint {
    pub const NOTHING: DebugPrint = DebugPrint {
        print_lambda_sets: false,
        print_only_under_alias: false,
        print_ranks: false,
        ignore_polarity: false,
        print_weakened_vars: false,
        print_variables: false,
    };
}

struct Env<'a> {
    home: ModuleId,
    interns: &'a Interns,
    debug: DebugPrint,
}

/// How many times a root variable appeared in Subs.
///
/// We only care about whether it was a single time or multiple times,
/// because single appearances get a wildcard (*) and multiple times
/// get a generated letter ("a" etc).
#[derive(Debug)]
enum Appearances {
    Single,
    Multiple,
}

/// Generate names for all type variables, replacing FlexVar(None) with
/// FlexVar(Some(name)) where appropriate. Example: for the identity
/// function, generate a name of "a" for both its argument and return
/// type variables.
///
/// We also want to count how many times a root appears, because we should
/// only generate a name for it if it appears more than once.
fn find_names_needed(
    variable: Variable,
    subs: &mut Subs,
    roots: &mut Vec<Variable>,
    root_appearances: &mut MutMap<Variable, Appearances>,
    names_taken: &mut MutMap<Lowercase, Variable>,
    find_under_alias: bool,
) {
    use crate::subs::Content::*;
    use crate::subs::FlatType::*;

    match &subs.get_content_without_compacting(variable).clone() {
        RecursionVar { opt_name: None, .. } | FlexVar(None) => {
            let root = subs.get_root_key_without_compacting(variable);

            // If this var is *not* its own root, then the
            // root var necessarily appears in multiple places.
            // We need a name for it!
            match root_appearances.get(&root) {
                Some(Appearances::Single) => {
                    root_appearances.insert(root, Appearances::Multiple);
                }
                Some(Appearances::Multiple) => {
                    // It's already multiple, so do nothing!
                }
                None => {
                    roots.push(root);
                    root_appearances.insert(root, Appearances::Single);
                }
            }
        }
        FlexAbleVar(None, _) => {
            let root = subs.get_root_key_without_compacting(variable);
            if !root_appearances.contains_key(&root) {
                roots.push(root);
            }
            // Able vars are always printed at least twice (in the signature, and in the
            // "implements" clause set).
            root_appearances.insert(root, Appearances::Multiple);
        }
        RecursionVar {
            opt_name: Some(name_index),
            ..
        }
        | FlexVar(Some(name_index))
        | FlexAbleVar(Some(name_index), _)
        | RigidVar(name_index)
        | RigidAbleVar(name_index, _) => {
            let root = subs.get_root_key_without_compacting(variable);

            // User-defined names are already taken.
            // We must not accidentally generate names that collide with them!
            let name = subs.field_names[name_index.index()].clone();
            match names_taken.get(&name) {
                Some(var) if *var == root => {}
                Some(_) => {
                    if !root_appearances.contains_key(&root) {
                        roots.push(root);
                    }
                    // We want a name, but the default name is already taken by another root.
                    root_appearances.insert(root, Appearances::Multiple);
                }
                None => {
                    names_taken.insert(name, root);
                }
            }
        }
        Structure(Apply(_, args)) => {
            for index in args.into_iter() {
                let var = subs[index];
                find_names_needed(
                    var,
                    subs,
                    roots,
                    root_appearances,
                    names_taken,
                    find_under_alias,
                );
            }
        }
        Structure(Func(arg_vars, closure_var, ret_var, fx_var)) => {
            for index in arg_vars.into_iter() {
                let var = subs[index];
                find_names_needed(
                    var,
                    subs,
                    roots,
                    root_appearances,
                    names_taken,
                    find_under_alias,
                );
            }

            find_names_needed(
                *closure_var,
                subs,
                roots,
                root_appearances,
                names_taken,
                find_under_alias,
            );

            find_names_needed(
                *ret_var,
                subs,
                roots,
                root_appearances,
                names_taken,
                find_under_alias,
            );

            find_names_needed(
                *fx_var,
                subs,
                roots,
                root_appearances,
                names_taken,
                find_under_alias,
            );
        }
        Structure(EffectfulFunc) => {}
        Structure(Record(sorted_fields, ext_var)) => {
            for index in sorted_fields.iter_variables() {
                let var = subs[index];
                find_names_needed(
                    var,
                    subs,
                    roots,
                    root_appearances,
                    names_taken,
                    find_under_alias,
                );
            }

            find_names_needed(
                *ext_var,
                subs,
                roots,
                root_appearances,
                names_taken,
                find_under_alias,
            );
        }
        Structure(Tuple(elems, ext_var)) => {
            for index in elems.iter_variables() {
                let var = subs[index];
                find_names_needed(
                    var,
                    subs,
                    roots,
                    root_appearances,
                    names_taken,
                    find_under_alias,
                );
            }

            find_names_needed(
                *ext_var,
                subs,
                roots,
                root_appearances,
                names_taken,
                find_under_alias,
            );
        }
        Structure(TagUnion(tags, ext_var)) => {
            for slice_index in tags.variables() {
                let slice = subs[slice_index];
                for var_index in slice {
                    let var = subs[var_index];
                    find_names_needed(
                        var,
                        subs,
                        roots,
                        root_appearances,
                        names_taken,
                        find_under_alias,
                    );
                }
            }

            find_names_needed(
                ext_var.var(),
                subs,
                roots,
                root_appearances,
                names_taken,
                find_under_alias,
            );
        }
        Structure(FunctionOrTagUnion(_, _, ext_var)) => {
            find_names_needed(
                ext_var.var(),
                subs,
                roots,
                root_appearances,
                names_taken,
                find_under_alias,
            );
        }
        Structure(RecursiveTagUnion(rec_var, tags, ext_var)) => {
            for slice_index in tags.variables() {
                let slice = subs[slice_index];
                for var_index in slice {
                    let var = subs[var_index];
                    find_names_needed(
                        var,
                        subs,
                        roots,
                        root_appearances,
                        names_taken,
                        find_under_alias,
                    );
                }
            }

            find_names_needed(
                ext_var.var(),
                subs,
                roots,
                root_appearances,
                names_taken,
                find_under_alias,
            );
            find_names_needed(
                *rec_var,
                subs,
                roots,
                root_appearances,
                names_taken,
                find_under_alias,
            );
        }
        Alias(_symbol, args, actual, _kind) => {
            // only find names for named parameters!
            for var_index in args.into_iter().take(args.len()) {
                let var = subs[var_index];
                find_names_needed(
                    var,
                    subs,
                    roots,
                    root_appearances,
                    names_taken,
                    find_under_alias,
                );
            }
            if find_under_alias {
                find_names_needed(
                    *actual,
                    subs,
                    roots,
                    root_appearances,
                    names_taken,
                    find_under_alias,
                );
            }
        }
        LambdaSet(subs::LambdaSet {
            solved,
            recursion_var,
            unspecialized,
            ambient_function: _,
        }) => {
            for slice_index in solved.variables() {
                let slice = subs[slice_index];
                for var_index in slice {
                    let var = subs[var_index];
                    find_names_needed(
                        var,
                        subs,
                        roots,
                        root_appearances,
                        names_taken,
                        find_under_alias,
                    );
                }
            }

            for uls_index in unspecialized.into_iter() {
                let Uls(var, _, _) = subs[uls_index];
                find_names_needed(
                    var,
                    subs,
                    roots,
                    root_appearances,
                    names_taken,
                    find_under_alias,
                );
            }

            if let Some(rec_var) = recursion_var.into_variable() {
                find_names_needed(
                    rec_var,
                    subs,
                    roots,
                    root_appearances,
                    names_taken,
                    find_under_alias,
                );
            }
        }
        RangedNumber(_) => {
            subs.set_content(variable, FlexVar(None));
            find_names_needed(
                variable,
                subs,
                roots,
                root_appearances,
                names_taken,
                find_under_alias,
            );
        }
        Error
        | Structure(EmptyRecord)
        | Structure(EmptyTagUnion)
        | Pure
        | Effectful
        | ErasedLambda => {
            // Errors and empty records don't need names.
        }
    }
}

struct NamedResult {
    recursion_structs_to_expand: VecSet<Variable>,
}

fn name_all_type_vars(variable: Variable, subs: &mut Subs, debug_print: DebugPrint) -> NamedResult {
    let mut roots = Vec::new();
    let mut letters_used = 0;
    let mut appearances = MutMap::default();
    let mut taken = MutMap::default();

    // Populate names_needed
    find_names_needed(
        variable,
        subs,
        &mut roots,
        &mut appearances,
        &mut taken,
        debug_print.print_only_under_alias,
    );

    let mut recursion_structs_to_expand = VecSet::default();

    for root in roots {
        // show the type variable number instead of `*`. useful for debugging
        // set_root_name(root, (format!("<{:?}>", root).into()), subs);
        match appearances.get(&root) {
            Some(Appearances::Multiple) => {
                if let Content::RecursionVar { structure, .. } =
                    subs.get_content_without_compacting(root)
                {
                    recursion_structs_to_expand
                        .insert(subs.get_root_key_without_compacting(*structure));
                }
                letters_used = name_root(letters_used, root, subs, &mut taken, debug_print);
            }
            Some(Appearances::Single) => {
                if let Content::RecursionVar { structure, .. } =
                    subs.get_content_without_compacting(root)
                {
                    recursion_structs_to_expand
                        .insert(subs.get_root_key_without_compacting(*structure));
                    letters_used = name_root(letters_used, root, subs, &mut taken, debug_print);
                } else if debug_print.print_weakened_vars && is_weakened_unbound(subs, root) {
                    letters_used = name_root(letters_used, root, subs, &mut taken, debug_print);
                }
            }
            _ => {}
        }
    }

    NamedResult {
        recursion_structs_to_expand,
    }
}

fn is_weakened_unbound(subs: &Subs, var: Variable) -> bool {
    use Content::*;
    let desc = subs.get_without_compacting(var);
    !desc.rank.is_generalized()
        && matches!(
            desc.content,
            FlexVar(_) | RigidVar(_) | FlexAbleVar(..) | RigidAbleVar(..)
        )
}

fn name_root(
    letters_used: u32,
    root: Variable,
    subs: &mut Subs,
    taken: &mut MutMap<Lowercase, Variable>,
    debug_print: DebugPrint,
) -> u32 {
    let prefix = if debug_print.print_weakened_vars && is_weakened_unbound(subs, root) {
        "w_" // weakened variable
    } else {
        ""
    };

    let (generated_name, new_letters_used) = match subs.get_content_unchecked(root) {
        Content::FlexVar(Some(name))
        | Content::RigidVar(name)
        | Content::FlexAbleVar(Some(name), _)
        | Content::RigidAbleVar(name, _)
        | Content::RecursionVar {
            opt_name: Some(name),
            ..
        } => {
            let name_hint = &subs[*name];
            if name_hint.as_str() == "*" {
                // Give a proper name to named wildcards!
                name_type_var(prefix, letters_used, &mut taken.keys(), |var, str| {
                    var.as_str() == str
                })
            } else {
                let generated = name_type_var_with_hint(
                    prefix,
                    name_hint.as_str(),
                    &mut taken.keys(),
                    |var, str| var.as_str() == str,
                );

                (generated, letters_used)
            }
        }
        _ => name_type_var(prefix, letters_used, &mut taken.keys(), |var, str| {
            var.as_str() == str
        }),
    };

    taken.insert(generated_name.clone(), root);

    set_root_name(root, generated_name, subs);

    new_letters_used
}

fn set_root_name(root: Variable, name: Lowercase, subs: &mut Subs) {
    use crate::subs::Content::*;

    let old_content = subs.get_content_without_compacting(root);

    match old_content {
        FlexVar(_) => {
            let name_index = subs.push_field_name(name);
            let content = FlexVar(Some(name_index));
            subs.set_content(root, content);
        }
        &FlexAbleVar(_, ability) => {
            let name_index = subs.push_field_name(name);
            let content = FlexAbleVar(Some(name_index), ability);
            subs.set_content(root, content);
        }
        RecursionVar {
            opt_name: None,
            structure,
        } => {
            let structure = *structure;
            let name_index = subs.push_field_name(name);
            let content = RecursionVar {
                structure,
                opt_name: Some(name_index),
            };
            subs.set_content(root, content);
        }
        RecursionVar {
            opt_name: Some(_existing),
            ..
        } => {
            panic!("TODO FIXME - make sure the generated name does not clash with any bound vars! In other words, if the user decided to name a type variable 'a', make sure we don't generate 'a' to name a different one!");
        }

        _ => (),
    }
}

#[derive(Default)]
struct Context<'a> {
    able_variables: Vec<(&'a str, AbilitySet)>,
    recursion_structs_to_expand: VecSet<Variable>,
}

fn variable_to_string(
    var: Variable,
    subs: &Subs,
    home: ModuleId,
    interns: &Interns,
    named_result: NamedResult,
    debug_print: DebugPrint,
    pol: Polarity,
) -> String {
    let mut buf = String::new();
    let env = Env {
        home,
        interns,
        debug: debug_print,
    };
    let mut ctx = Context {
        able_variables: vec![],
        recursion_structs_to_expand: named_result.recursion_structs_to_expand,
    };

    write_content(
        &env,
        &mut ctx,
        var,
        subs,
        &mut buf,
        Parens::Unnecessary,
        pol,
    );

    ctx.able_variables.sort();
    ctx.able_variables.dedup();
    for (i, (var, abilities)) in ctx.able_variables.into_iter().enumerate() {
        if i == 0 {
            buf.push(' ');
            buf.push_str(roc_parse::keyword::WHERE)
        } else {
            buf.push(',');
        }
        buf.push(' ');
        buf.push_str(var);
        buf.push(' ');
        buf.push_str(roc_parse::keyword::IMPLEMENTS);
        for (i, ability) in abilities.into_sorted_iter().enumerate() {
            if i > 0 {
                buf.push_str(" &");
            }
            buf.push(' ');
            write_symbol(&env, ability, &mut buf);
        }
    }

    buf
}

pub fn name_and_print_var(
    var: Variable,
    subs: &mut Subs,
    home: ModuleId,
    interns: &Interns,
    debug_print: DebugPrint,
) -> String {
    let named_result = name_all_type_vars(var, subs, debug_print);
    variable_to_string(
        var,
        subs,
        home,
        interns,
        named_result,
        debug_print,
        Polarity::Pos,
    )
}

pub fn get_single_arg<'a>(subs: &'a Subs, args: &'a AliasVariables) -> Variable {
    debug_assert_eq!(args.len(), 1);

    let arg_var_index = args
        .into_iter()
        .next()
        .expect("Num was not applied to a type argument!");
    subs[arg_var_index]
}

fn write_content<'a>(
    env: &Env,
    ctx: &mut Context<'a>,
    var: Variable,
    subs: &'a Subs,
    buf: &mut String,
    parens: Parens,
    pol: Polarity,
) {
    use crate::subs::Content::*;

    if env.debug.print_ranks {
        buf.push_str(&format!("⟨@{:?}⟩", subs.get_rank(var).into_usize()));
    }
    if env.debug.print_variables {
        buf.push_str(&format!(
            "<{:?}>",
            subs.get_root_key_without_compacting(var)
        ));
    }

    match subs.get_content_without_compacting(var) {
        FlexVar(Some(name_index)) => {
            let name = &subs.field_names[name_index.index()];
            buf.push_str(name.as_str())
        }
        FlexVar(None) => buf.push_str(WILDCARD),
        RigidVar(name_index) => {
            let name = &subs.field_names[name_index.index()];
            buf.push_str(name.as_str())
        }
        FlexAbleVar(opt_name_index, abilities) => {
            let name = opt_name_index
                .map(|name_index| subs.field_names[name_index.index()].as_str())
                .unwrap_or(WILDCARD);
            let abilities = AbilitySet::from_iter(subs.get_subs_slice(*abilities).iter().copied());
            ctx.able_variables.push((name, abilities));
            buf.push_str(name);
        }
        RigidAbleVar(name_index, abilities) => {
            let name = subs.field_names[name_index.index()].as_str();
            let abilities = AbilitySet::from_iter(subs.get_subs_slice(*abilities).iter().copied());
            ctx.able_variables.push((name, abilities));
            buf.push_str(name);
        }
        RecursionVar {
            opt_name,
            structure,
        } => match opt_name {
            Some(name_index) => {
                let structure_root = subs.get_root_key_without_compacting(*structure);
                if ctx.recursion_structs_to_expand.remove(&structure_root) {
                    write_content(env, ctx, *structure, subs, buf, parens, pol);

                    ctx.recursion_structs_to_expand.insert(structure_root);
                } else {
                    let name = &subs.field_names[name_index.index()];
                    buf.push_str(name.as_str())
                }
            }
            None => {
                unreachable!("This should always be filled in!")
            }
        },
        Structure(flat_type) => write_flat_type(env, ctx, var, flat_type, subs, buf, parens, pol),
        Alias(symbol, args, actual, _kind) => {
            let write_parens = parens == Parens::InTypeParam && !args.is_empty();

            match *symbol {
                Symbol::NUM_NUM => {
                    let content = get_single_arg(subs, args);
                    match *subs.get_content_without_compacting(content) {
                        Alias(nested, args, _actual, _kind) => match nested {
                            Symbol::NUM_INTEGER => {
                                write_integer(
                                    env,
                                    ctx,
                                    get_single_arg(subs, &args),
                                    subs,
                                    buf,
                                    parens,
                                    write_parens,
                                    pol,
                                );
                            }
                            Symbol::NUM_FLOATINGPOINT => write_float(
                                env,
                                ctx,
                                get_single_arg(subs, &args),
                                subs,
                                buf,
                                parens,
                                write_parens,
                                pol,
                            ),

                            _ => write_parens!(write_parens, buf, {
                                buf.push_str("Num ");
                                write_content(env, ctx, content, subs, buf, parens, pol);
                            }),
                        },

                        _ => write_parens!(write_parens, buf, {
                            buf.push_str("Num ");
                            write_content(env, ctx, content, subs, buf, parens, pol);
                        }),
                    }
                }

                Symbol::NUM_INT => {
                    let content = get_single_arg(subs, args);

                    write_integer(env, ctx, content, subs, buf, parens, write_parens, pol)
                }

                Symbol::NUM_FRAC => write_float(
                    env,
                    ctx,
                    get_single_arg(subs, args),
                    subs,
                    buf,
                    parens,
                    write_parens,
                    pol,
                ),

                _ if env.debug.print_only_under_alias
                    // If any infer-open-in-output-position extension variable is now material, we
                    // cannot keep the alias as-is - we have to print its underlying type!
                    || args.any_infer_ext_var_is_material(subs) =>
                {
                    write_parens!(write_parens, buf, {
                        write_content(env, ctx, *actual, subs, buf, parens, pol)
                    })
                }

                _ => write_parens!(write_parens, buf, {
                    write_symbol(env, *symbol, buf);

                    for var_index in args.named_type_arguments() {
                        let var = subs[var_index];
                        buf.push(' ');
                        write_content(env, ctx, var, subs, buf, Parens::InTypeParam, pol);
                    }

                    roc_debug_flags::dbg_do!(roc_debug_flags::ROC_PRETTY_PRINT_ALIAS_CONTENTS, {
                        buf.push_str("[[ but really ");
                        write_content(env, ctx, *actual, subs, buf, parens, pol);
                        buf.push_str("]]");
                    });
                }),
            }
        }
        LambdaSet(subs::LambdaSet {
            solved,
            recursion_var,
            unspecialized,
            ambient_function: _,
        }) => {
            debug_assert!(env.debug.print_lambda_sets);

            buf.push_str("[[");

            let print_symbol = |symbol: &Symbol| {
                let ident_str = symbol.as_str(env.interns);
                let ident_index_str = symbol.ident_id().index().to_string();
                let disambiguation = if ident_str != ident_index_str {
                    // The pretty name is a named identifier; print the ident as well to avoid
                    // ambguity (in shadows or ability specializations).
                    format!("({ident_index_str})")
                } else {
                    "".to_string()
                };
                if env.home == symbol.module_id() {
                    format!("{ident_str}{disambiguation}",)
                } else {
                    format!(
                        "{}.{}{}",
                        symbol.module_string(env.interns),
                        ident_str,
                        disambiguation
                    )
                }
            };

            write_sorted_tags2(
                env,
                ctx,
                subs,
                buf,
                solved.unsorted_lambdas(subs),
                print_symbol,
                pol,
            );

            buf.push(']');

            if let Some(rec_var) = recursion_var.into_variable() {
                buf.push_str(" as ");
                write_content(env, ctx, rec_var, subs, buf, parens, pol)
            }

            for Uls(var, member, region) in subs.get_subs_slice(*unspecialized) {
                buf.push_str(" + ");
                write_content(env, ctx, *var, subs, buf, Parens::Unnecessary, pol);
                buf.push(':');
                buf.push_str(&print_symbol(member));
                buf.push(':');
                buf.push_str(&region.to_string());
            }

            buf.push(']');
        }
        ErasedLambda => {
            debug_assert!(env.debug.print_lambda_sets);

            // Easy mode 🤠
            buf.push('?');
        }
        Pure => {
            buf.push_str("->");
        }
        Effectful => {
            buf.push_str("=>");
        }
        RangedNumber(range) => {
            buf.push_str("Range(");
            for (i, &var) in range.variable_slice().iter().enumerate() {
                if i > 0 {
                    buf.push_str(", ");
                }
                write_content(env, ctx, var, subs, buf, Parens::Unnecessary, pol);
            }
            buf.push(')');
        }
        Error => buf.push_str("<type mismatch>"),
    }
}

fn write_float<'a>(
    env: &Env,
    ctx: &mut Context<'a>,
    var: Variable,
    subs: &'a Subs,
    buf: &mut String,
    parens: Parens,
    write_parens: bool,
    pol: Polarity,
) {
    use crate::subs::Content::*;
    match subs.get_content_without_compacting(var) {
        Alias(Symbol::NUM_BINARY32, _, _, _) => buf.push_str("F32"),
        Alias(Symbol::NUM_BINARY64, _, _, _) => buf.push_str("F64"),
        Alias(Symbol::NUM_DECIMAL, _, _, _) => buf.push_str("Dec"),
        _ => write_parens!(write_parens, buf, {
            buf.push_str("Frac ");
            write_content(env, ctx, var, subs, buf, parens, pol);
        }),
    }
}

fn write_integer<'a>(
    env: &Env,
    ctx: &mut Context<'a>,
    var: Variable,
    subs: &'a Subs,
    buf: &mut String,
    parens: Parens,
    write_parens: bool,
    pol: Polarity,
) {
    use crate::subs::Content::*;

    macro_rules! derive_num_writes {
        ($($lit:expr, $tag:path)*) => {
            match subs.get_content_without_compacting(var) {
                $(
                &Alias($tag, _, _, _) => {
                    buf.push_str($lit)
                },
                )*
                _ => {
                    write_parens!(
                        write_parens,
                        buf,
                        {
                            buf.push_str("Int ");
                            write_content(env, ctx, var, subs, buf, parens, pol);
                        }
                    )
                }
            }
        }
    }

    derive_num_writes! {
        "U8", Symbol::NUM_UNSIGNED8
        "U16", Symbol::NUM_UNSIGNED16
        "U32", Symbol::NUM_UNSIGNED32
        "U64", Symbol::NUM_UNSIGNED64
        "U128", Symbol::NUM_UNSIGNED128
        "I8", Symbol::NUM_SIGNED8
        "I16", Symbol::NUM_SIGNED16
        "I32", Symbol::NUM_SIGNED32
        "I64", Symbol::NUM_SIGNED64
        "I128", Symbol::NUM_SIGNED128
    }
}

enum ExtContent {
    Empty,
    Content(Variable),
}

impl ExtContent {
    fn for_tag(subs: &Subs, ext: Variable, pol: Polarity, debug_flags: &DebugPrint) -> Self {
        let content = subs.get_content_without_compacting(ext);
        match content {
            Content::Structure(FlatType::EmptyTagUnion) => ExtContent::Empty,
            Content::Structure(FlatType::EmptyRecord) => ExtContent::Empty,

            Content::FlexVar(None) | Content::FlexAbleVar(None, _)
                if pol.is_pos() && !debug_flags.ignore_polarity =>
            {
                // This is a wildcard `[...]*`, which is elided in positive positions!
                ExtContent::Empty
            }

            // All other vars are named, and must appear regardless of their position.
            Content::FlexVar(_)
            | Content::FlexAbleVar(..)
            | Content::RigidVar(_)
            | Content::RigidAbleVar(..) => ExtContent::Content(ext),

            other => unreachable!("something weird ended up in an ext var: {:?}", other),
        }
    }
}

fn write_ext_content<'a>(
    env: &Env,
    ctx: &mut Context<'a>,
    subs: &'a Subs,
    buf: &mut String,
    ext_content: ExtContent,
    parens: Parens,
    pol: Polarity,
) {
    if let ExtContent::Content(var) = ext_content {
        // This is an open record or tag union, so print the variable
        // right after the '}' or ']'
        //
        // e.g. the "*" at the end of `{ x: I64 }*`
        // or the "r" at the end of `{ x: I64 }r`
        write_content(env, ctx, var, subs, buf, parens, pol)
    }
}

fn write_sorted_tags2<'a, L>(
    env: &Env,
    ctx: &mut Context<'a>,
    subs: &'a Subs,
    buf: &mut String,
    tags: UnsortedUnionLabels<L>,
    label_to_string: impl Fn(&L) -> String,
    pol: Polarity,
) where
    L: Label + Ord,
{
    let mut sorted_fields = tags.tags;

    sorted_fields.sort_by(|(a, _), (b, _)| a.cmp(b));

    let mut any_written_yet = false;

    for (label, vars) in sorted_fields {
        if any_written_yet {
            buf.push_str(", ");
        } else {
            any_written_yet = true;
        }

        buf.push_str(&label_to_string(label));

        for var in vars {
            buf.push(' ');
            write_content(env, ctx, *var, subs, buf, Parens::InTypeParam, pol);
        }
    }
}

fn write_sorted_tags<'a>(
    env: &Env,
    ctx: &mut Context<'a>,
    subs: &'a Subs,
    buf: &mut String,
    tags: &MutMap<TagName, Vec<Variable>>,
    ext_var: Variable,
    pol: Polarity,
) -> ExtContent {
    // Sort the fields so they always end up in the same order.
    let mut sorted_fields = Vec::with_capacity(tags.len());

    for (label, vars) in tags {
        sorted_fields.push((label, vars));
    }

    // If the `ext` contains tags, merge them into the list of tags.
    // this can occur when inferring mutually recursive tags
    let mut from_ext = Default::default();
    let _ext_content = chase_ext_tag_union(subs, ext_var, &mut from_ext);

    for (tag_name, arguments) in from_ext.iter() {
        sorted_fields.push((tag_name, arguments));
    }

    sorted_fields.sort_by(|(a, _), (b, _)| a.as_ident_str().cmp(&b.as_ident_str()));

    let mut any_written_yet = false;

    for (label, vars) in sorted_fields {
        if any_written_yet {
            buf.push_str(", ");
        } else {
            any_written_yet = true;
        }

        buf.push_str(label.as_ident_str().as_str());

        for var in vars {
            buf.push(' ');
            write_content(env, ctx, *var, subs, buf, Parens::InTypeParam, pol);
        }
    }

    ExtContent::for_tag(subs, ext_var, pol, &env.debug)
}

fn write_flat_type<'a>(
    env: &Env,
    ctx: &mut Context<'a>,
    var: Variable,
    flat_type: &FlatType,
    subs: &'a Subs,
    buf: &mut String,
    parens: Parens,
    pol: Polarity,
) {
    use crate::subs::FlatType::*;

    match flat_type {
        Apply(symbol, args) => write_apply(
            env,
            ctx,
            *symbol,
            subs.get_subs_slice(*args),
            subs,
            buf,
            parens,
            pol,
        ),
        EmptyRecord => buf.push_str(EMPTY_RECORD),
        EmptyTagUnion => buf.push_str(EMPTY_TAG_UNION),
        Func(args, closure, ret, fx) => write_fn(
            env,
            ctx,
            subs.get_subs_slice(*args),
            *closure,
            *ret,
            *fx,
            subs,
            buf,
            parens,
            pol,
        ),
        EffectfulFunc => buf.push_str(EFFECTFUL_FUNC),
        Record(fields, ext_var) => {
            use crate::types::{gather_fields, RecordStructure};

            // If the `ext` has concrete fields (e.g. { foo : I64}{ bar : Bool }), merge them
            let RecordStructure {
                fields: sorted_fields,
                ext,
            } = gather_fields(subs, *fields, *ext_var)
                .expect("Something ended up weird in this record type");
            let ext_var = ext;

            if fields.is_empty() {
                buf.push_str(EMPTY_RECORD)
            } else {
                buf.push_str("{ ");

                let mut any_written_yet = false;

                for (label, record_field) in sorted_fields {
                    use RecordField::*;

                    let var = *record_field.as_inner();

                    if any_written_yet {
                        buf.push_str(", ");
                    } else {
                        any_written_yet = true;
                    }
                    buf.push_str(label.as_str());

                    match record_field {
                        Optional(_) | RigidOptional(_) => buf.push_str(" ? "),
                        Required(_) | Demanded(_) | RigidRequired(_) => buf.push_str(" : "),
                    };

                    write_content(env, ctx, var, subs, buf, Parens::Unnecessary, pol);
                }

                buf.push_str(" }");
            }

            match subs.get_content_without_compacting(ext_var) {
                Content::Structure(EmptyRecord) => {
                    // This is a closed record. We're done!
                }
                _ => {
                    // This is an open record, so print the variable
                    // right after the '}'
                    //
                    // e.g. the "*" at the end of `{ x: I64 }*`
                    // or the "r" at the end of `{ x: I64 }r`
                    write_content(env, ctx, ext_var, subs, buf, parens, pol)
                }
            }
        }
        Tuple(elems, ext_var) => {
            use crate::types::{gather_tuple_elems, TupleStructure};

            // If the `ext` has concrete elems (e.g. (I64, I64)(Bool)), merge them
            let TupleStructure {
                elems: sorted_elems,
                ext,
            } = gather_tuple_elems(subs, *elems, *ext_var)
                .expect("Something ended up weird in this record type");
            let ext_var = ext;

            buf.push_str("( ");

            let mut any_written_yet = false;
            let mut expected_next_index = 0;

            for (index, var) in sorted_elems {
                if any_written_yet {
                    buf.push_str(", ");
                } else {
                    any_written_yet = true;
                }

                if index - expected_next_index > 4 {
                    // Don't write out a large number of _'s - just write out a count
                    buf.push_str(&format!("... {} omitted, ", index - expected_next_index));
                } else if index - expected_next_index > 1 {
                    // Write out a bunch of _'s
                    for _ in expected_next_index..index {
                        buf.push_str("_, ");
                    }
                }
                expected_next_index = index + 1;

                write_content(env, ctx, var, subs, buf, Parens::Unnecessary, pol);
            }

            buf.push_str(" )");

            // This is an open tuple, so print the variable
            // right after the ')'
            //
            // e.g. the "*" at the end of `( I64, I64 )*`
            // or the "r" at the end of `( I64, I64 )r`
            write_content(env, ctx, ext_var, subs, buf, parens, pol)
        }
        TagUnion(tags, ext_var) => {
            buf.push('[');

            // Sort the fields so they always end up in the same order.
            let (tags, new_ext_var) = tags.unsorted_tags_and_ext(subs, *ext_var);
            write_sorted_tags2(
                env,
                ctx,
                subs,
                buf,
                tags,
                |tag| tag.0.as_str().to_string(),
                pol,
            );

            buf.push(']');

            write_ext_content(
                env,
                ctx,
                subs,
                buf,
                ExtContent::for_tag(subs, new_ext_var.var(), pol, &env.debug),
                parens,
                pol,
            )
        }

        FunctionOrTagUnion(tag_names, _, ext_var) => {
            buf.push('[');

            let mut tags: MutMap<TagName, _> = MutMap::default();
            tags.extend(
                subs.get_subs_slice(*tag_names)
                    .iter()
                    .map(|t| (t.clone(), vec![])),
            );
            let ext_content = write_sorted_tags(env, ctx, subs, buf, &tags, ext_var.var(), pol);

            buf.push(']');

            write_ext_content(env, ctx, subs, buf, ext_content, parens, pol)
        }

        RecursiveTagUnion(rec_var, tags, ext_var) => {
            ctx.recursion_structs_to_expand
                .remove(&subs.get_root_key_without_compacting(var));

            write_parens!(parens == Parens::InTypeParam, buf, {
                buf.push('[');

                let (tags, new_ext_var) = tags.unsorted_tags_and_ext(subs, *ext_var);
                write_sorted_tags2(
                    env,
                    ctx,
                    subs,
                    buf,
                    tags,
                    |tag| tag.0.as_str().to_string(),
                    pol,
                );

                buf.push(']');

                write_ext_content(
                    env,
                    ctx,
                    subs,
                    buf,
                    ExtContent::for_tag(subs, new_ext_var.var(), pol, &env.debug),
                    parens,
                    pol,
                );

                buf.push_str(" as ");
                write_content(env, ctx, *rec_var, subs, buf, parens, pol)
            })
        }
    }
}

pub fn push_union<L: Label>(
    subs: &Subs,
    tags: &UnionLabels<L>,
    fields: &mut Vec<(L, Vec<Variable>)>,
) {
    fields.reserve(tags.len());
    for (name_index, slice_index) in tags.iter_all() {
        let subs_slice = subs[slice_index];
        let slice = subs.get_subs_slice(subs_slice);
        let tag_name = L::index_subs(subs, name_index).clone();

        fields.push((tag_name, slice.to_vec()));
    }
}

pub enum ChasedExt {
    Empty,
    NonEmpty {
        variable: Variable,
        content: Content,
    },
}

pub fn chase_ext_tag_union(
    subs: &'_ Subs,
    var: Variable,
    fields: &mut Vec<(TagName, Vec<Variable>)>,
) -> ChasedExt {
    use FlatType::*;
    match subs.get_content_without_compacting(var) {
        Content::Structure(EmptyTagUnion) => ChasedExt::Empty,
        Content::Structure(TagUnion(tags, ext_var)) => {
            push_union(subs, tags, fields);
            chase_ext_tag_union(subs, ext_var.var(), fields)
        }

        Content::Structure(RecursiveTagUnion(_, tags, ext_var)) => {
            push_union(subs, tags, fields);
            chase_ext_tag_union(subs, ext_var.var(), fields)
        }
        Content::Structure(FunctionOrTagUnion(tag_names, _, ext_var)) => {
            fields.extend(
                subs.get_subs_slice(*tag_names)
                    .iter()
                    .map(|t| (t.clone(), vec![])),
            );

            chase_ext_tag_union(subs, ext_var.var(), fields)
        }

        Content::Alias(_, _, var, _) => chase_ext_tag_union(subs, *var, fields),

        content => ChasedExt::NonEmpty {
            variable: var,
            content: *content,
        },
    }
}

fn write_apply<'a>(
    env: &Env,
    ctx: &mut Context<'a>,
    symbol: Symbol,
    args: &[Variable],
    subs: &'a Subs,
    buf: &mut String,
    parens: Parens,
    pol: Polarity,
) {
    let write_parens = parens == Parens::InTypeParam && !args.is_empty();

    // Hardcoded type aliases
    match symbol {
        Symbol::STR_STR => {
            buf.push_str("Str");
        }
        Symbol::NUM_NUM => {
            let arg = args
                .iter()
                .next()
                .unwrap_or_else(|| panic!("Num did not have any type parameters somehow."));
            let arg_content = subs.get_content_without_compacting(*arg);
            let mut arg_param = String::new();

            let mut default_case = |subs, content| {
                if write_parens {
                    buf.push('(');
                }

                write_content(
                    env,
                    ctx,
                    content,
                    subs,
                    &mut arg_param,
                    Parens::InTypeParam,
                    pol,
                );
                buf.push_str("Num ");
                buf.push_str(&arg_param);

                if write_parens {
                    buf.push(')');
                }
            };

            match &arg_content {
                Content::Structure(FlatType::Apply(symbol, nested_args)) => match *symbol {
                    Symbol::NUM_INTEGER if nested_args.len() == 1 => {
                        buf.push_str("I64");
                    }
                    Symbol::NUM_FLOATINGPOINT if nested_args.len() == 1 => {
                        buf.push_str("F64");
                    }
                    _ => default_case(subs, *arg),
                },
                _ => default_case(subs, *arg),
            }
        }
        _ => {
            if write_parens {
                buf.push('(');
            }

            write_symbol(env, symbol, buf);

            for arg in args {
                buf.push(' ');
                write_content(env, ctx, *arg, subs, buf, Parens::InTypeParam, pol);
            }

            if write_parens {
                buf.push(')');
            }
        }
    }
}

#[allow(clippy::too_many_arguments)]
fn write_fn<'a>(
    env: &Env,
    ctx: &mut Context<'a>,
    args: &[Variable],
    closure: Variable,
    ret: Variable,
    fx: Variable,
    subs: &'a Subs,
    buf: &mut String,
    parens: Parens,
    pol: Polarity,
) {
    let mut needs_comma = false;
    let use_parens = parens != Parens::Unnecessary;

    if use_parens {
        buf.push('(');
    }

    if args.is_empty() {
        buf.push_str("()");
    } else {
        for arg in args {
            if needs_comma {
                buf.push_str(", ");
            } else {
                needs_comma = true;
            }

            write_content(env, ctx, *arg, subs, buf, Parens::InFn, Polarity::Neg);
        }
    }

    if !env.debug.print_lambda_sets {
        buf.push(' ');
        write_content(env, ctx, fx, subs, buf, Parens::Unnecessary, Polarity::Neg);
        buf.push(' ');
    } else {
        buf.push_str(" -");
        write_content(env, ctx, closure, subs, buf, parens, pol);
        write_content(env, ctx, fx, subs, buf, Parens::Unnecessary, Polarity::Neg);
        buf.push(' ');
    }

    write_content(env, ctx, ret, subs, buf, Parens::InFn, Polarity::Pos);

    if use_parens {
        buf.push(')');
    }
}

fn write_symbol(env: &Env, symbol: Symbol, buf: &mut String) {
    let interns = &env.interns;
    let ident_str = symbol.as_str(interns);
    let module_id = symbol.module_id();

    // Don't qualify the symbol if it's in our home module,
    // or if it's a builtin (since all their types are always in scope)
    if module_id != env.home && !module_id.is_builtin() {
        buf.push_str(module_id.to_ident_str(interns).as_str());
        buf.push('.');
    }

    buf.push_str(ident_str);
}
