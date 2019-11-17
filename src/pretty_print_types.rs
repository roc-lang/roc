use collections::{MutMap, MutSet};
use subs::{Content, FlatType, Subs, Variable};
use types;

static WILDCARD: &str = "*";
static EMPTY_RECORD: &str = "{}";
static THE_LETTER_A: u32 = 'a' as u32;

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
/// It's important that we track insertion order, which is why
/// names_needed is a Vec. We also want to count how many times a root
/// appears, because we should only generate a name for it if it appears
/// more than once.
fn find_names_needed(
    variable: Variable,
    subs: &mut Subs,
    roots: &mut Vec<Variable>,
    root_appearances: &mut MutMap<Variable, Appearances>,
    names_taken: &mut MutSet<String>,
) {
    use subs::Content::*;
    use subs::FlatType::*;

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
        Structure(Apply {
            module_name: _,
            name: _,
            args,
        }) => {
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
        RigidVar(name) => {
            // User-defined names are already taken.
            // We must not accidentally generate names that collide with them!
            names_taken.insert(name.to_string());
        }
        Error(_) | Structure(Erroneous(_)) | Structure(EmptyRecord) => {
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
        match appearances.get(&root) {
            Some(Appearances::Multiple) => {
                letters_used = name_root(letters_used, root, subs, &taken);
            }
            _ => (),
        }
    }
}

fn name_root(letters_used: u32, root: Variable, subs: &mut Subs, taken: &MutSet<String>) -> u32 {
    // TODO we should arena-allocate this String,
    // so all the strings in the entire pass only require ~1 allocation.
    let generated_name = if letters_used < 26 {
        // This should generate "a", then "b", etc.
        std::char::from_u32(THE_LETTER_A + letters_used)
            .unwrap_or_else(|| panic!("Tried to convert {} to a char", THE_LETTER_A + letters_used))
            .to_string()
    } else {
        panic!("TODO generate aa, ab, ac, ...");
    };

    if taken.contains(&generated_name) {
        // If the generated name is already taken, try again.
        name_root(letters_used + 1, root, subs, taken)
    } else {
        set_root_name(root, &generated_name, subs);

        letters_used + 1
    }
}

fn set_root_name(root: Variable, name: &str, subs: &mut Subs) {
    use subs::Content::*;

    let mut descriptor = subs.get(root);

    match descriptor.content {
        FlexVar(None) => {
            descriptor.content = FlexVar(Some(name.into()));

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
    use subs::Content::*;

    match content {
        FlexVar(Some(name)) => buf.push_str(&name),
        FlexVar(None) => buf.push_str(WILDCARD),
        RigidVar(name) => buf.push_str(&name),
        Structure(flat_type) => write_flat_type(flat_type, subs, buf, parens),
        Error(_) => buf.push_str("<type mismatch>"),
    }
}

fn write_flat_type(flat_type: FlatType, subs: &mut Subs, buf: &mut String, parens: Parens) {
    use subs::FlatType::*;

    match flat_type {
        Apply {
            module_name,
            name,
            args,
        } => write_apply(
            module_name.to_string(),
            name.to_string(),
            args,
            subs,
            buf,
            parens,
        ),
        EmptyRecord => buf.push_str(EMPTY_RECORD),
        Func(args, ret) => write_fn(args, ret, subs, buf, parens),
        Erroneous(problem) => {
            buf.push_str(&format!("<Type Mismatch: {:?}>", problem));
        }
    }
}

fn write_apply(
    module_name: String,
    type_name: String,
    args: Vec<Variable>,
    subs: &mut Subs,
    buf: &mut String,
    parens: Parens,
) {
    let write_parens = parens == Parens::InTypeParam && !args.is_empty();

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

        buf.push_str(&format!("{}.{}", module_name, type_name));

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
