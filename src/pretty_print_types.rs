use crate::can::ident::{Lowercase, TagName};
use crate::collections::{ImSet, MutMap, MutSet};
use crate::module::symbol::{Interns, ModuleId, Symbol};
use crate::subs::{Content, FlatType, Subs, Variable};
use crate::types::name_type_var;
use crate::uniqueness::boolean_algebra::{Atom, Bool};

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
enum Parens {
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

    while let Some((recursive, _)) = subs.occurs(variable) {
        if let Content::Structure(FlatType::TagUnion(tags, ext_var)) = subs.get(recursive).content {
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
        } else {
            panic!("unfixable recursive type in pretty_print_types")
        }
    }

    match subs.get(variable).content {
        FlexVar(None) => {
            let root = subs.get_root_key(variable);

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
        FlexVar(Some(_)) => {
            // This root already has a name. Nothing to do here!
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
            for (_, var) in fields {
                find_names_needed(var, subs, roots, root_appearances, names_taken);
            }

            find_names_needed(ext_var, subs, roots, root_appearances, names_taken);
        }
        Structure(TagUnion(tags, ext_var)) => {
            for var in tags.values().flatten() {
                find_names_needed(*var, subs, roots, root_appearances, names_taken);
            }

            find_names_needed(ext_var, subs, roots, root_appearances, names_taken);
        }
        Structure(RecursiveTagUnion(rec_var, tags, ext_var)) => {
            for var in tags.values().flatten() {
                find_names_needed(*var, subs, roots, root_appearances, names_taken);
            }

            find_names_needed(ext_var, subs, roots, root_appearances, names_taken);
            find_names_needed(rec_var, subs, roots, root_appearances, names_taken);
        }
        Structure(Boolean(b)) => {
            for var in b.variables() {
                find_names_needed(var, subs, roots, root_appearances, names_taken);
            }
        }
        RigidVar(name) => {
            // User-defined names are already taken.
            // We must not accidentally generate names that collide with them!
            names_taken.insert(name);
        }
        Alias(_, args, _actual) => {
            // TODO should we also look in the actual variable?
            for (_, var) in args {
                find_names_needed(var, subs, roots, root_appearances, names_taken);
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

    set_root_name(root, &generated_name, subs);

    new_letters_used
}

fn set_root_name(root: Variable, name: &Lowercase, subs: &mut Subs) {
    use crate::subs::Content::*;

    let mut descriptor = subs.get(root);

    match descriptor.content {
        FlexVar(None) => {
            descriptor.content = FlexVar(Some(name.clone()));

            // TODO is this necessary, or was mutating descriptor in place sufficient?
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
    subs: &mut Subs,
    home: ModuleId,
    interns: &Interns,
) -> String {
    let mut buf = String::new();
    let env = Env { home, interns };

    write_content(&env, content, subs, &mut buf, Parens::Unnecessary);

    buf
}

fn write_content(env: &Env, content: Content, subs: &mut Subs, buf: &mut String, parens: Parens) {
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
                    debug_assert!(args.len() == 1);
                    let (_, arg_var) = args
                        .get(0)
                        .expect("Num was not applied to a type argument!");
                    let content = subs.get(*arg_var).content;

                    match &content {
                        Alias(nested, nested_args, _) => {
                            debug_assert!(nested_args.is_empty());
                            match *nested {
                                Symbol::INT_INTEGER => buf.push_str("Int"),
                                Symbol::FLOAT_FLOATINGPOINT => buf.push_str("Float"),
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
                        write_content(env, subs.get(var).content, subs, buf, Parens::InTypeParam);
                    }
                }),
            }
        }
        Error => buf.push_str("<type mismatch>"),
    }
}

fn write_flat_type(
    env: &Env,
    flat_type: FlatType,
    subs: &mut Subs,
    buf: &mut String,
    parens: Parens,
) {
    use crate::subs::FlatType::*;

    match flat_type {
        Apply(symbol, args) => write_apply(env, symbol, args, subs, buf, parens),
        EmptyRecord => buf.push_str(EMPTY_RECORD),
        EmptyTagUnion => buf.push_str(EMPTY_TAG_UNION),
        Func(args, ret) => write_fn(env, args, ret, subs, buf, parens),
        Record(fields, ext_var) => {
            use crate::unify::gather_fields;
            use crate::unify::RecordStructure;

            // If the `ext` has concrete fields (e.g. { foo : Int}{ bar : Bool }), merge them
            let RecordStructure { fields, ext } = gather_fields(subs, fields, ext_var);
            let ext_var = ext;

            if fields.is_empty() {
                buf.push_str(EMPTY_RECORD)
            } else {
                buf.push_str("{ ");

                // Sort the fields so they always end up in the same order.
                let mut sorted_fields = Vec::with_capacity(fields.len());

                for (label, field_var) in fields {
                    sorted_fields.push((label, field_var));
                }

                sorted_fields.sort_by(|(a, _), (b, _)| a.cmp(b));

                let mut any_written_yet = false;

                for (label, field_var) in sorted_fields {
                    if any_written_yet {
                        buf.push_str(", ");
                    } else {
                        any_written_yet = true;
                    }
                    buf.push_str(label.as_str());

                    buf.push_str(" : ");
                    write_content(env, subs.get(field_var).content, subs, buf, parens);
                }

                buf.push_str(" }");
            }

            match subs.get(ext_var).content {
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
            if tags.is_empty() {
                buf.push_str(EMPTY_TAG_UNION)
            } else {
                let interns = &env.interns;
                let home = env.home;

                buf.push_str("[ ");

                // Sort the fields so they always end up in the same order.
                let mut sorted_fields = Vec::with_capacity(tags.len());

                for (label, vars) in tags {
                    sorted_fields.push((label.clone(), vars));
                }

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
                        write_content(env, subs.get(var).content, subs, buf, parens);
                    }
                }

                buf.push_str(" ]");
            }

            match subs.get(ext_var).content {
                Content::Structure(EmptyTagUnion) => {
                    // This is a closed record. We're done!
                }
                content => {
                    // This is an open tag union, so print the variable
                    // right after the ']'
                    //
                    // e.g. the "*" at the end of `{ x: Int }*`
                    // or the "r" at the end of `{ x: Int }r`
                    write_content(env, content, subs, buf, parens)
                }
            }
        }

        RecursiveTagUnion(rec_var, tags, ext_var) => {
            if tags.is_empty() {
                buf.push_str(EMPTY_TAG_UNION)
            } else {
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
                        write_content(env, subs.get(var).content, subs, buf, parens);
                    }
                }

                buf.push_str(" ]");

                if let Some(content) = ext_content {
                    // This is an open tag union, so print the variable
                    // right after the ']'
                    //
                    // e.g. the "*" at the end of `{ x: Int }*`
                    // or the "r" at the end of `{ x: Int }r`
                    write_content(env, content, subs, buf, parens)
                }
            }

            buf.push_str(" as ");
            write_content(env, subs.get(rec_var).content, subs, buf, parens)
        }
        Boolean(b) => {
            write_boolean(env, b, subs, buf, Parens::InTypeParam);
        }
        Erroneous(problem) => {
            buf.push_str(&format!("<Type Mismatch: {:?}>", problem));
        }
    }
}

fn chase_ext_tag_union(
    subs: &mut Subs,
    var: Variable,
    fields: &mut Vec<(TagName, Vec<Variable>)>,
) -> Option<Content> {
    use FlatType::*;
    match subs.get(var).content {
        Content::Structure(EmptyTagUnion) => None,
        Content::Structure(TagUnion(tags, ext_var))
        | Content::Structure(RecursiveTagUnion(_, tags, ext_var)) => {
            for (label, vars) in tags {
                fields.push((label.clone(), vars.to_vec()));
            }

            chase_ext_tag_union(subs, ext_var, fields)
        }

        content => Some(content),
    }
}

fn write_boolean(env: &Env, boolean: Bool, subs: &mut Subs, buf: &mut String, parens: Parens) {
    match boolean.simplify(subs) {
        Err(atom) => write_boolean_atom(env, atom, subs, buf, parens),
        Ok(variables) => {
            let mut buffers_set = ImSet::default();

            for v in variables {
                let mut inner_buf: String = "".to_string();
                write_content(env, subs.get(v).content, subs, &mut inner_buf, parens);
                buffers_set.insert(inner_buf);
            }

            let mut buffers: Vec<String> = buffers_set.into_iter().collect();
            buffers.sort();

            let combined = buffers.join(" | ");

            let write_parens = buffers.len() > 1;

            if write_parens {
                buf.push_str("(");
            }
            buf.push_str(&combined);
            if write_parens {
                buf.push_str(")");
            }
        }
    }
}

fn write_boolean_atom(env: &Env, atom: Atom, subs: &mut Subs, buf: &mut String, parens: Parens) {
    match atom {
        Atom::Variable(var) => write_content(env, subs.get(var).content, subs, buf, parens),
        Atom::Zero => {
            buf.push_str("Attr.Shared");
        }
        Atom::One => {
            buf.push_str("Attr.Unique");
        }
    }
}

fn write_apply(
    env: &Env,
    symbol: Symbol,
    args: Vec<Variable>,
    subs: &mut Subs,
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
            let arg_content = subs.get(arg).content;
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
                    Symbol::INT_INTEGER if nested_args.is_empty() => {
                        buf.push_str("Int");
                    }
                    Symbol::FLOAT_FLOATINGPOINT if nested_args.is_empty() => {
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
                            Symbol::INT_INTEGER if double_nested_args.is_empty() => {
                                buf.push_str("Int");
                            }
                            Symbol::FLOAT_FLOATINGPOINT if double_nested_args.is_empty() => {
                                buf.push_str("Float");
                            }
                            _ => default_case(subs, arg_content),
                        },

                        _ => default_case(subs, arg_content),
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
                write_content(env, subs.get(arg).content, subs, buf, Parens::InTypeParam);
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
    subs: &mut Subs,
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

        write_content(env, subs.get(arg).content, subs, buf, Parens::InFn);
    }

    buf.push_str(" -> ");
    write_content(env, subs.get(ret).content, subs, buf, Parens::InFn);

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
