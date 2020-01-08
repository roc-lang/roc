use crate::can::ident::{Lowercase, ModuleName, Uppercase};
use crate::collections::{MutMap, MutSet};
use crate::subs::{Content, FlatType, Subs, Variable};
use crate::types::{self, name_type_var};
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
        Structure(Apply { args, .. }) => {
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
        Alias(_, _, _, _) => {
            panic!("TODO find_names_needed Alias");
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

pub fn content_to_string(content: Content, subs: &mut Subs) -> String {
    let mut buf = String::new();

    write_content(content, subs, &mut buf, Parens::Unnecessary);

    buf
}

fn write_content(content: Content, subs: &mut Subs, buf: &mut String, parens: Parens) {
    use crate::subs::Content::*;

    match content {
        FlexVar(Some(name)) => buf.push_str(name.as_str()),
        FlexVar(None) => buf.push_str(WILDCARD),
        RigidVar(name) => buf.push_str(name.as_str()),
        Structure(flat_type) => write_flat_type(flat_type, subs, buf, parens),
        Alias(_, _, _, _) => {
            panic!("TODO write_content Alias");
        }
        Error => buf.push_str("<type mismatch>"),
    }
}

fn write_flat_type(flat_type: FlatType, subs: &mut Subs, buf: &mut String, parens: Parens) {
    use crate::subs::FlatType::*;

    match flat_type {
        Apply {
            module_name,
            name,
            args,
        } => write_apply(module_name, name, args, subs, buf, parens),
        EmptyRecord => buf.push_str(EMPTY_RECORD),
        EmptyTagUnion => buf.push_str(EMPTY_TAG_UNION),
        Func(args, ret) => write_fn(args, ret, subs, buf, parens),
        Record(fields, ext_var) => {
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
                    write_content(subs.get(field_var).content, subs, buf, parens);
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
                    write_content(content, subs, buf, parens)
                }
            }
        }
        TagUnion(tags, ext_var) => {
            if tags.is_empty() {
                buf.push_str(EMPTY_TAG_UNION)
            } else {
                buf.push_str("[ ");

                // Sort the fields so they always end up in the same order.
                let mut sorted_fields = Vec::with_capacity(tags.len());

                for (label, vars) in tags {
                    sorted_fields.push((label.clone(), vars));
                }

                sorted_fields.sort_by(|(a, _), (b, _)| a.cmp(b));

                let mut any_written_yet = false;

                for (label, vars) in sorted_fields {
                    if any_written_yet {
                        buf.push_str(", ");
                    } else {
                        any_written_yet = true;
                    }
                    buf.push_str(label.as_str());

                    for var in vars {
                        buf.push(' ');
                        write_content(subs.get(var).content, subs, buf, parens);
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
                    write_content(content, subs, buf, parens)
                }
            }
        }
        Boolean(b) => {
            write_boolean(b, subs, buf, Parens::InTypeParam);
        }
        Erroneous(problem) => {
            buf.push_str(&format!("<Type Mismatch: {:?}>", problem));
        }
    }
}

fn write_boolean(boolean: Bool, subs: &mut Subs, buf: &mut String, parens: Parens) {
    let is_atom = boolean.is_var() || boolean == Bool::Zero || boolean == Bool::One;
    let write_parens = parens == Parens::InTypeParam && !is_atom;

    if write_parens {
        buf.push_str("(");
    }

    match boolean {
        Bool::Variable(var) => write_content(subs.get(var).content, subs, buf, parens),
        Bool::Or(p, q) => {
            write_boolean(*p, subs, buf, Parens::InTypeParam);
            buf.push_str(" | ");
            write_boolean(*q, subs, buf, Parens::InTypeParam);
        }
        Bool::And(p, q) => {
            write_boolean(*p, subs, buf, Parens::InTypeParam);
            buf.push_str(" & ");
            write_boolean(*q, subs, buf, Parens::InTypeParam);
        }
        Bool::Not(p) => {
            buf.push_str("!");
            write_boolean(*p, subs, buf, Parens::InTypeParam);
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
    module_name: ModuleName,
    type_name: Uppercase,
    args: Vec<Variable>,
    subs: &mut Subs,
    buf: &mut String,
    parens: Parens,
) {
    let write_parens = parens == Parens::InTypeParam && !args.is_empty();
    let module_name = module_name.as_str();
    let type_name = type_name.as_str();

    // Hardcoded type aliases
    if module_name == "Str" && type_name == "Str" {
        buf.push_str("Str");
    } else if module_name == types::MOD_NUM && type_name == types::TYPE_NUM {
        let arg = args
            .into_iter()
            .next()
            .unwrap_or_else(|| panic!("Num did not have any type parameters somehow."));
        let arg_content = subs.get(arg).content;
        let mut arg_param = String::new();

        write_content(arg_content, subs, &mut arg_param, Parens::InTypeParam);

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
    } else if module_name == "List" && type_name == "List" {
        if write_parens {
            buf.push_str("(");
        }

        buf.push_str("List ");

        let arg = args
            .into_iter()
            .next()
            .unwrap_or_else(|| panic!("List did not have any type parameters somehow."));
        let arg_content = subs.get(arg).content;

        write_content(arg_content, subs, buf, Parens::InTypeParam);

        if write_parens {
            buf.push_str(")");
        }
    } else {
        if write_parens {
            buf.push_str("(");
        }

        if module_name.is_empty() {
            buf.push_str(&type_name);
        } else {
            buf.push_str(&format!("{}.{}", module_name, type_name));
        }

        for arg in args {
            buf.push_str(" ");
            write_content(subs.get(arg).content, subs, buf, Parens::InTypeParam);
        }

        if write_parens {
            buf.push_str(")");
        }
    }
}

fn write_fn(args: Vec<Variable>, ret: Variable, subs: &mut Subs, buf: &mut String, parens: Parens) {
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

        write_content(subs.get(arg).content, subs, buf, Parens::InFn);
    }

    buf.push_str(" -> ");
    write_content(subs.get(ret).content, subs, buf, Parens::InFn);

    if use_parens {
        buf.push_str(")");
    }
}
