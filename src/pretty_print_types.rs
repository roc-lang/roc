use crate::can::ident::Lowercase;
use crate::collections::{MutMap, MutSet};
use crate::module::symbol::{Interns, ModuleId, Symbol};
use crate::subs::{Content, FlatType, Subs, Variable};
use crate::types::name_type_var;
use crate::uniqueness::boolean_algebra::Bool;

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
            write_symbol(env, symbol, buf);

            for (_, var) in args {
                buf.push(' ');
                write_content(env, subs.get(var).content, subs, buf, parens);
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
    use crate::collections::ImMap;
    use crate::subs::Content::Structure;
    use crate::subs::FlatType::*;
    use crate::uniqueness::boolean_algebra;

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
        Boolean(b) => {
            // push global substitutions into the boolean
            let mut global_substitution = ImMap::default();

            for v in b.variables() {
                if let Structure(Boolean(replacement)) = subs.get(v).content {
                    global_substitution.insert(v, replacement);
                }
            }

            write_boolean(
                env,
                boolean_algebra::simplify(b.substitute(&global_substitution)),
                subs,
                buf,
                Parens::InTypeParam,
            );
        }
        Erroneous(problem) => {
            buf.push_str(&format!("<Type Mismatch: {:?}>", problem));
        }
    }
}

fn write_boolean(env: &Env, boolean: Bool, subs: &mut Subs, buf: &mut String, parens: Parens) {
    let is_atom = boolean.is_var() || boolean == Bool::Zero || boolean == Bool::One;
    let write_parens = parens == Parens::InTypeParam && !is_atom;

    if write_parens {
        buf.push_str("(");
    }

    match boolean {
        Bool::Variable(var) => write_content(env, subs.get(var).content, subs, buf, parens),
        Bool::Or(p, q) => {
            write_boolean(env, *p, subs, buf, Parens::InTypeParam);
            buf.push_str(" | ");
            write_boolean(env, *q, subs, buf, Parens::InTypeParam);
        }
        Bool::And(p, q) => {
            write_boolean(env, *p, subs, buf, Parens::InTypeParam);
            buf.push_str(" & ");
            write_boolean(env, *q, subs, buf, Parens::InTypeParam);
        }
        Bool::Not(p) => {
            buf.push_str("!");
            write_boolean(env, *p, subs, buf, Parens::InTypeParam);
        }
        Bool::Zero => {
            buf.push_str("Attr.Shared");
        }
        Bool::One => {
            buf.push_str("Attr.Unique");
        }
    };

    if write_parens {
        buf.push_str(")");
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

            write_content(env, arg_content, subs, &mut arg_param, Parens::InTypeParam);

            if arg_param == "Int.Integer" {
                buf.push_str("Int");
            } else if arg_param == "Float.FloatingPoint" {
                buf.push_str("Float");
            } else {
                if write_parens {
                    buf.push_str("(");
                }

                buf.push_str("Num ");
                buf.push_str(&arg_param);

                if write_parens {
                    buf.push_str(")");
                }
            }
        }
        Symbol::LIST_LIST => {
            if write_parens {
                buf.push_str("(");
            }

            buf.push_str("List ");

            let arg = args
                .into_iter()
                .next()
                .unwrap_or_else(|| panic!("List did not have any type parameters somehow."));
            let arg_content = subs.get(arg).content;

            write_content(env, arg_content, subs, buf, Parens::InTypeParam);

            if write_parens {
                buf.push_str(")");
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

    // Don't qualify the symbol if it's in our home module
    if module_id == env.home {
        buf.push_str(ident);
    } else {
        buf.push_str(module_id.to_string(&interns));
        buf.push('.');
        buf.push_str(ident);
    }
}
