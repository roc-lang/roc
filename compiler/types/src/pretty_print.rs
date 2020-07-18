use crate::boolean_algebra::Bool;
use crate::subs::{Content, FlatType, Subs, Variable};
use crate::types::{name_type_var, RecordField};
use roc_collections::all::{MutMap, MutSet};
use roc_module::ident::{Lowercase, TagName};
use roc_module::symbol::{Interns, ModuleId, Symbol};

static WILDCARD: &str = "*";
static EMPTY_RECORD: &str = "{}";
static EMPTY_TAG_UNION: &str = "[]";

/// Rerquirements for parentheses.
///
/// If we're inside a function (that is, this is either an argument or a return
/// value), we may need to use parens. Examples:
///
/// a -> (* -> a)
/// (* -> a) -> a
///
/// Separately, if we're inside a type parameter, we may need to use parens:
///
/// List Int
/// List (List Int)
///
/// Otherwise, parens are unnecessary.
#[derive(Clone, Copy, Debug, PartialEq)]
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

struct Env<'a> {
    home: ModuleId,
    interns: &'a Interns,
}

/// How many times a root variable appeared in Subs.
///
/// We only care about whether it was a single time or multiple times,
/// because single appearances get a wildcard (*) and multiple times
/// get a generated letter ("a" etc).
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
    names_taken: &mut MutSet<Lowercase>,
) {
    use crate::subs::Content::*;
    use crate::subs::FlatType::*;

    while let Some((recursive, _chain)) = subs.occurs(variable) {
        let content = subs.get_without_compacting(recursive).content;
        match content {
            Content::Structure(FlatType::TagUnion(tags, ext_var)) => {
                let rec_var = subs.fresh_unnamed_flex_var();

                let mut new_tags = MutMap::default();

                for (label, args) in tags {
                    let new_args = args
                        .clone()
                        .into_iter()
                        .map(|var| if var == recursive { rec_var } else { var })
                        .collect();

                    new_tags.insert(label.clone(), new_args);
                }

                let flat_type = FlatType::RecursiveTagUnion(rec_var, new_tags, ext_var);
                subs.set_content(recursive, Content::Structure(flat_type));
            }
            Content::Structure(FlatType::Boolean(Bool::Container(_cvar, _mvars))) => {
                crate::boolean_algebra::flatten(subs, recursive);
            }
            _ => panic!(
                "unfixable recursive type in roc_types::pretty_print {:?} {:?} {:?}",
                recursive, variable, content
            ),
        }
    }

    match subs.get_without_compacting(variable).content {
        FlexVar(None) => {
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
        FlexVar(Some(name)) => {
            // This root already has a name. Nothing more to do here!

            // User-defined names are already taken.
            // We must not accidentally generate names that collide with them!
            names_taken.insert(name);
        }
        RigidVar(name) => {
            // User-defined names are already taken.
            // We must not accidentally generate names that collide with them!
            names_taken.insert(name);
        }
        Structure(Apply(Symbol::ATTR_ATTR, args)) => {
            // assign uniqueness var names based on when they occur in the base type
            find_names_needed(args[1], subs, roots, root_appearances, names_taken);
            find_names_needed(args[0], subs, roots, root_appearances, names_taken);
        }
        Structure(Apply(_, args)) => {
            for var in args {
                find_names_needed(var, subs, roots, root_appearances, names_taken);
            }
        }
        Structure(Func(arg_vars, ret_var)) => {
            for var in arg_vars {
                find_names_needed(var, subs, roots, root_appearances, names_taken);
            }

            find_names_needed(ret_var, subs, roots, root_appearances, names_taken);
        }
        Structure(Record(fields, ext_var)) => {
            let mut sorted_fields: Vec<_> = fields.iter().collect();

            sorted_fields.sort_by(|(label1, _), (label2, _)| label1.cmp(label2));

            for (_, field) in sorted_fields {
                find_names_needed(
                    field.into_inner(),
                    subs,
                    roots,
                    root_appearances,
                    names_taken,
                );
            }

            find_names_needed(ext_var, subs, roots, root_appearances, names_taken);
        }
        Structure(TagUnion(tags, ext_var)) => {
            let mut sorted_tags: Vec<_> = tags.iter().collect();
            sorted_tags.sort();

            for var in sorted_tags.into_iter().map(|(_, v)| v).flatten() {
                find_names_needed(*var, subs, roots, root_appearances, names_taken);
            }

            find_names_needed(ext_var, subs, roots, root_appearances, names_taken);
        }
        Structure(RecursiveTagUnion(rec_var, tags, ext_var)) => {
            let mut sorted_tags: Vec<_> = tags.iter().collect();
            sorted_tags.sort();

            for var in sorted_tags.into_iter().map(|(_, v)| v).flatten() {
                find_names_needed(*var, subs, roots, root_appearances, names_taken);
            }

            find_names_needed(ext_var, subs, roots, root_appearances, names_taken);
            find_names_needed(rec_var, subs, roots, root_appearances, names_taken);
        }
        Structure(Boolean(b)) => match b {
            Bool::Shared => {}
            Bool::Container(cvar, mvars) => {
                find_names_needed(cvar, subs, roots, root_appearances, names_taken);

                for var in mvars {
                    find_names_needed(var, subs, roots, root_appearances, names_taken);
                }
            }
        },
        Alias(symbol, args, _actual) => {
            if let Symbol::ATTR_ATTR = symbol {
                find_names_needed(args[0].1, subs, roots, root_appearances, names_taken);
                find_names_needed(args[1].1, subs, roots, root_appearances, names_taken);
            } else {
                for (_, var) in args {
                    find_names_needed(var, subs, roots, root_appearances, names_taken);
                }
                // TODO should we also look in the actual variable?
                // find_names_needed(_actual, subs, roots, root_appearances, names_taken);
            }
        }
        Error | Structure(Erroneous(_)) | Structure(EmptyRecord) | Structure(EmptyTagUnion) => {
            // Errors and empty records don't need names.
        }
    }
}

pub fn name_all_type_vars(variable: Variable, subs: &mut Subs) {
    let mut roots = Vec::new();
    let mut letters_used = 0;
    let mut appearances = MutMap::default();
    let mut taken = MutSet::default();

    // Populate names_needed
    find_names_needed(variable, subs, &mut roots, &mut appearances, &mut taken);

    for root in roots {
        // show the type variable number instead of `*`. useful for debugging
        // set_root_name(root, (format!("<{:?}>", root).into()), subs);
        if let Some(Appearances::Multiple) = appearances.get(&root) {
            letters_used = name_root(letters_used, root, subs, &mut taken);
        }
    }
}

fn name_root(
    letters_used: u32,
    root: Variable,
    subs: &mut Subs,
    taken: &mut MutSet<Lowercase>,
) -> u32 {
    let (generated_name, new_letters_used) = name_type_var(letters_used, taken);

    set_root_name(root, generated_name, subs);

    new_letters_used
}

fn set_root_name(root: Variable, name: Lowercase, subs: &mut Subs) {
    use crate::subs::Content::*;

    let mut descriptor = subs.get_without_compacting(root);

    match descriptor.content {
        FlexVar(None) => {
            descriptor.content = FlexVar(Some(name));
            subs.set(root, descriptor);
        }
        FlexVar(Some(_existing)) => {
            panic!("TODO FIXME - make sure the generated name does not clash with any bound vars! In other words, if the user decided to name a type variable 'a', make sure we don't generate 'a' to name a different one!");
        }
        _ => (),
    }
}

pub fn content_to_string(
    content: Content,
    subs: &Subs,
    home: ModuleId,
    interns: &Interns,
) -> String {
    let mut buf = String::new();
    let env = Env { home, interns };

    write_content(&env, content, subs, &mut buf, Parens::Unnecessary);

    buf
}

fn write_content(env: &Env, content: Content, subs: &Subs, buf: &mut String, parens: Parens) {
    use crate::subs::Content::*;

    match content {
        FlexVar(Some(name)) => buf.push_str(name.as_str()),
        FlexVar(None) => buf.push_str(WILDCARD),
        RigidVar(name) => buf.push_str(name.as_str()),
        Structure(flat_type) => write_flat_type(env, flat_type, subs, buf, parens),
        Alias(symbol, args, _actual) => {
            let write_parens = parens == Parens::InTypeParam && !args.is_empty();

            match symbol {
                Symbol::NUM_NUM => {
                    debug_assert_eq!(args.len(), 1);

                    let (_, arg_var) = args
                        .get(0)
                        .expect("Num was not applied to a type argument!");
                    let content = subs.get_without_compacting(*arg_var).content;

                    match &content {
                        Alias(nested, _, _) => match *nested {
                            Symbol::NUM_INTEGER => buf.push_str("Int"),
                            Symbol::NUM_FLOATINGPOINT => buf.push_str("Float"),

                            _ => write_parens!(write_parens, buf, {
                                buf.push_str("Num ");
                                write_content(env, content, subs, buf, parens);
                            }),
                        },

                        Structure(FlatType::Apply(Symbol::ATTR_ATTR, nested_args)) => {
                            let attr_content = subs.get_without_compacting(nested_args[1]).content;
                            match &attr_content {
                                Alias(nested, _, _) => match *nested {
                                    Symbol::NUM_INTEGER => buf.push_str("Int"),
                                    Symbol::NUM_FLOATINGPOINT => buf.push_str("Float"),
                                    _ => write_parens!(write_parens, buf, {
                                        buf.push_str("Num ");
                                        write_content(env, content, subs, buf, parens);
                                    }),
                                },
                                _ => write_parens!(write_parens, buf, {
                                    buf.push_str("Num ");
                                    write_content(env, content, subs, buf, parens);
                                }),
                            }
                        }

                        _ => write_parens!(write_parens, buf, {
                            buf.push_str("Num ");
                            write_content(env, content, subs, buf, parens);
                        }),
                    }
                }

                _ => write_parens!(write_parens, buf, {
                    write_symbol(env, symbol, buf);

                    for (_, var) in args {
                        buf.push(' ');
                        write_content(
                            env,
                            subs.get_without_compacting(var).content,
                            subs,
                            buf,
                            Parens::InTypeParam,
                        );
                    }

                    // useful for debugging
                    if false {
                        buf.push_str("[[ but really ");
                        let content = subs.get_without_compacting(_actual).content;
                        write_content(env, content, subs, buf, parens);
                        buf.push_str("]]");
                    }
                }),
            }
        }
        Error => buf.push_str("<type mismatch>"),
    }
}

fn write_flat_type(env: &Env, flat_type: FlatType, subs: &Subs, buf: &mut String, parens: Parens) {
    use crate::subs::FlatType::*;

    match flat_type {
        Apply(symbol, args) => write_apply(env, symbol, args, subs, buf, parens),
        EmptyRecord => buf.push_str(EMPTY_RECORD),
        EmptyTagUnion => buf.push_str(EMPTY_TAG_UNION),
        Func(args, ret) => write_fn(env, args, ret, subs, buf, parens),
        Record(fields, ext_var) => {
            use crate::types::{gather_fields, RecordStructure};

            // If the `ext` has concrete fields (e.g. { foo : Int}{ bar : Bool }), merge them
            let RecordStructure { fields, ext } = gather_fields(subs, fields, ext_var);
            let ext_var = ext;

            if fields.is_empty() {
                buf.push_str(EMPTY_RECORD)
            } else {
                buf.push_str("{ ");

                // Sort the fields so they always end up in the same order.
                let mut sorted_fields = Vec::with_capacity(fields.len());

                sorted_fields.extend(fields);
                sorted_fields.sort_by(|(a, _), (b, _)| a.cmp(b));

                let mut any_written_yet = false;

                for (label, field_var) in sorted_fields {
                    use RecordField::*;

                    if any_written_yet {
                        buf.push_str(", ");
                    } else {
                        any_written_yet = true;
                    }
                    buf.push_str(label.as_str());

                    let var = match field_var {
                        Optional(var) => {
                            buf.push_str(" ? ");
                            var
                        }
                        Required(var) => {
                            buf.push_str(" : ");
                            var
                        }
                    };

                    write_content(
                        env,
                        subs.get_without_compacting(var).content,
                        subs,
                        buf,
                        parens,
                    );
                }

                buf.push_str(" }");
            }

            match subs.get_without_compacting(ext_var).content {
                Content::Structure(EmptyRecord) => {
                    // This is a closed record. We're done!
                }
                content => {
                    // This is an open record, so print the variable
                    // right after the '}'
                    //
                    // e.g. the "*" at the end of `{ x: Int }*`
                    // or the "r" at the end of `{ x: Int }r`
                    write_content(env, content, subs, buf, parens)
                }
            }
        }
        TagUnion(tags, ext_var) => {
            let interns = &env.interns;
            let home = env.home;

            buf.push_str("[ ");

            // Sort the fields so they always end up in the same order.
            let mut sorted_fields = Vec::with_capacity(tags.len());

            for (label, vars) in tags {
                sorted_fields.push((label.clone(), vars));
            }

            // If the `ext` contains tags, merge them into the list of tags.
            // this can occur when inferring mutually recursive tags
            let ext_content = chase_ext_tag_union(subs, ext_var, &mut sorted_fields);

            sorted_fields.sort_by(|(a, _), (b, _)| {
                a.clone()
                    .into_string(interns, home)
                    .cmp(&b.clone().into_string(&interns, home))
            });

            let mut any_written_yet = false;

            for (label, vars) in sorted_fields {
                if any_written_yet {
                    buf.push_str(", ");
                } else {
                    any_written_yet = true;
                }

                buf.push_str(&label.into_string(&interns, home));

                for var in vars {
                    buf.push(' ');
                    write_content(
                        env,
                        subs.get_without_compacting(var).content,
                        subs,
                        buf,
                        Parens::InTypeParam,
                    );
                }
            }

            buf.push_str(" ]");

            if let Err((_, content)) = ext_content {
                // This is an open tag union, so print the variable
                // right after the ']'
                //
                // e.g. the "*" at the end of `{ x: Int }*`
                // or the "r" at the end of `{ x: Int }r`
                write_content(env, content, subs, buf, parens)
            }
        }

        RecursiveTagUnion(rec_var, tags, ext_var) => {
            let interns = &env.interns;
            let home = env.home;

            buf.push_str("[ ");

            // Sort the fields so they always end up in the same order.
            let mut sorted_fields = Vec::with_capacity(tags.len());

            for (label, vars) in tags {
                sorted_fields.push((label.clone(), vars));
            }

            // If the `ext` contains tags, merge them into the list of tags.
            // this can occur when inferring mutually recursive tags
            let ext_content = chase_ext_tag_union(subs, ext_var, &mut sorted_fields);

            sorted_fields.sort_by(|(a, _), (b, _)| a.cmp(b));

            let mut any_written_yet = false;

            for (label, vars) in sorted_fields {
                if any_written_yet {
                    buf.push_str(", ");
                } else {
                    any_written_yet = true;
                }
                buf.push_str(&label.into_string(&interns, home));

                for var in vars {
                    buf.push(' ');
                    write_content(
                        env,
                        subs.get_without_compacting(var).content,
                        subs,
                        buf,
                        Parens::InTypeParam,
                    );
                }
            }

            buf.push_str(" ]");

            if let Err((_, content)) = ext_content {
                // This is an open tag union, so print the variable
                // right after the ']'
                //
                // e.g. the "*" at the end of `{ x: Int }*`
                // or the "r" at the end of `{ x: Int }r`
                write_content(env, content, subs, buf, parens)
            }

            buf.push_str(" as ");
            write_content(
                env,
                subs.get_without_compacting(rec_var).content,
                subs,
                buf,
                parens,
            )
        }
        Boolean(b) => {
            write_boolean(env, b, subs, buf, Parens::InTypeParam);
        }
        Erroneous(problem) => {
            buf.push_str(&format!("<Type Mismatch: {:?}>", problem));
        }
    }
}

pub fn chase_ext_tag_union(
    subs: &Subs,
    var: Variable,
    fields: &mut Vec<(TagName, Vec<Variable>)>,
) -> Result<(), (Variable, Content)> {
    use FlatType::*;
    match subs.get_without_compacting(var).content {
        Content::Structure(EmptyTagUnion) => Ok(()),
        Content::Structure(TagUnion(tags, ext_var))
        | Content::Structure(RecursiveTagUnion(_, tags, ext_var)) => {
            for (label, vars) in tags {
                fields.push((label.clone(), vars.to_vec()));
            }

            chase_ext_tag_union(subs, ext_var, fields)
        }
        Content::Structure(Apply(Symbol::ATTR_ATTR, arguments)) => {
            debug_assert_eq!(arguments.len(), 2);

            chase_ext_tag_union(subs, arguments[1], fields)
        }
        Content::Alias(_, _, var) => chase_ext_tag_union(subs, var, fields),

        content => Err((var, content)),
    }
}

pub fn chase_ext_record(
    subs: &Subs,
    var: Variable,
    fields: &mut MutMap<Lowercase, RecordField<Variable>>,
) -> Result<(), (Variable, Content)> {
    use crate::subs::Content::*;
    use crate::subs::FlatType::*;

    match subs.get_without_compacting(var).content {
        Structure(Record(sub_fields, sub_ext)) => {
            fields.extend(sub_fields.into_iter());

            chase_ext_record(subs, sub_ext, fields)
        }

        Structure(EmptyRecord) => Ok(()),

        Content::Structure(Apply(Symbol::ATTR_ATTR, arguments)) => {
            debug_assert_eq!(arguments.len(), 2);

            chase_ext_record(subs, arguments[1], fields)
        }

        Alias(_, _, var) => chase_ext_record(subs, var, fields),

        content => Err((var, content)),
    }
}

fn write_boolean(env: &Env, boolean: Bool, subs: &Subs, buf: &mut String, parens: Parens) {
    use crate::boolean_algebra::var_is_shared;

    match boolean.simplify(subs) {
        Bool::Shared => {
            buf.push_str("Shared");
        }
        Bool::Container(cvar, mvars) if mvars.iter().all(|v| var_is_shared(subs, *v)) => {
            debug_assert!(!var_is_shared(subs, cvar));

            write_content(
                env,
                subs.get_without_compacting(cvar).content,
                subs,
                buf,
                Parens::Unnecessary,
            );
        }
        Bool::Container(cvar, mvars) => {
            debug_assert!(!var_is_shared(subs, cvar));

            let mut buffers = Vec::with_capacity(mvars.len());
            for v in mvars {
                // don't print shared in a container
                if var_is_shared(subs, v) {
                    continue;
                }

                let mut inner_buf: String = "".to_string();
                write_content(
                    env,
                    subs.get_without_compacting(v).content,
                    subs,
                    &mut inner_buf,
                    parens,
                );
                buffers.push(inner_buf);
            }

            // sort type variables alphabetically
            buffers.sort();

            let combined = buffers.join(" | ");

            buf.push_str("(");
            write_content(
                env,
                subs.get_without_compacting(cvar).content,
                subs,
                buf,
                Parens::Unnecessary,
            );
            buf.push_str(" | ");
            buf.push_str(&combined);
            buf.push_str(")");
        }
    }
}

fn write_apply(
    env: &Env,
    symbol: Symbol,
    args: Vec<Variable>,
    subs: &Subs,
    buf: &mut String,
    parens: Parens,
) {
    let write_parens = parens == Parens::InTypeParam && !args.is_empty();

    // Hardcoded type aliases
    match symbol {
        Symbol::STR_STR => {
            buf.push_str("Str");
        }
        Symbol::NUM_NUM => {
            let arg = args
                .into_iter()
                .next()
                .unwrap_or_else(|| panic!("Num did not have any type parameters somehow."));
            let arg_content = subs.get_without_compacting(arg).content;
            let mut arg_param = String::new();

            let mut default_case = |subs, content| {
                if write_parens {
                    buf.push_str("(");
                }

                write_content(env, content, subs, &mut arg_param, Parens::InTypeParam);
                buf.push_str("Num ");
                buf.push_str(&arg_param);

                if write_parens {
                    buf.push_str(")");
                }
            };

            match &arg_content {
                Content::Structure(FlatType::Apply(symbol, nested_args)) => match *symbol {
                    Symbol::NUM_INTEGER if nested_args.is_empty() => {
                        buf.push_str("Int");
                    }
                    Symbol::NUM_FLOATINGPOINT if nested_args.is_empty() => {
                        buf.push_str("Float");
                    }
                    Symbol::ATTR_ATTR => match nested_args
                        .get(1)
                        .map(|v| subs.get_without_compacting(*v).content)
                    {
                        Some(Content::Structure(FlatType::Apply(
                            double_nested_symbol,
                            double_nested_args,
                        ))) => match double_nested_symbol {
                            Symbol::NUM_INTEGER if double_nested_args.is_empty() => {
                                buf.push_str("Int");
                            }
                            Symbol::NUM_FLOATINGPOINT if double_nested_args.is_empty() => {
                                buf.push_str("Float");
                            }
                            _ => default_case(subs, arg_content),
                        },

                        _other => default_case(subs, arg_content),
                    },
                    _ => default_case(subs, arg_content),
                },
                _ => default_case(subs, arg_content),
            }
        }
        _ => {
            if write_parens {
                buf.push_str("(");
            }

            write_symbol(env, symbol, buf);

            for arg in args {
                buf.push_str(" ");
                write_content(
                    env,
                    subs.get_without_compacting(arg).content,
                    subs,
                    buf,
                    Parens::InTypeParam,
                );
            }

            if write_parens {
                buf.push_str(")");
            }
        }
    }
}

fn write_fn(
    env: &Env,
    args: Vec<Variable>,
    ret: Variable,
    subs: &Subs,
    buf: &mut String,
    parens: Parens,
) {
    let mut needs_comma = false;
    let use_parens = parens != Parens::Unnecessary;

    if use_parens {
        buf.push_str("(");
    }

    for arg in args {
        if needs_comma {
            buf.push_str(", ");
        } else {
            needs_comma = true;
        }

        write_content(
            env,
            subs.get_without_compacting(arg).content,
            subs,
            buf,
            Parens::InFn,
        );
    }

    buf.push_str(" -> ");
    write_content(
        env,
        subs.get_without_compacting(ret).content,
        subs,
        buf,
        Parens::InFn,
    );

    if use_parens {
        buf.push_str(")");
    }
}

fn write_symbol(env: &Env, symbol: Symbol, buf: &mut String) {
    let interns = &env.interns;
    let ident = symbol.ident_string(interns);
    let module_id = symbol.module_id();

    // Don't qualify the symbol if it's in our home module,
    // or if it's a builtin (since all their types are always in scope)
    if module_id == env.home || module_id.is_builtin() {
        buf.push_str(ident);
    } else {
        buf.push_str(module_id.to_string(&interns));
        buf.push('.');
        buf.push_str(ident);
    }
}
