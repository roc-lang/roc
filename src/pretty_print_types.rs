use subs::{Content, FlatType, Subs, Variable};
use types;

static WILDCARD: &str = "*";
static EMPTY_RECORD: &str = "{}";
static THE_LETTER_A: u32 = 'a' as u32;

/// Generate names for all type variables, replacing FlexVar(None) with
/// FlexVar(Some(name)) where appropriate. Example: for the identity
/// function, generate a name of "a" for both its argument and return
/// type variables.
pub fn name_all_type_vars(letters_used: u32, variable: Variable, subs: &mut Subs) {
    use subs::Content::*;
    use subs::FlatType::*;

    let mut letters_used = letters_used;

    match subs.get(variable).content {
        Structure(Apply {
            module_name: _,
            name: _,
            args,
        }) => {
            for var in args {
                let root = subs.get_root_key(var);

                // If this var is *not* its own root, then the
                // root var necessarily appears in multiple places.
                // Generate a name for it!
                if var != root {
                    name_root(letters_used, root, subs);

                    letters_used += 1;
                }
            }
        }
        Structure(Func(arg_vars, ret_var)) => {
            for var in arg_vars {
                let root = subs.get_root_key(var);

                if var != root {
                    name_root(letters_used, root, subs);

                    letters_used += 1;
                }
            }

            let root = subs.get_root_key(ret_var);

            if ret_var != root {
                name_root(letters_used, root, subs);
            }
        }
        _ => (),
    }
}

fn name_root(letters_used: u32, root: Variable, subs: &mut Subs) {
    use subs::Content::*;

    // TODO we should arena-allocate this String,
    // so all the strings in the entire pass only require ~1 allocation.
    let generated_name = if letters_used < 26 {
        // This should generate "a", then "b", etc.
        std::char::from_u32(THE_LETTER_A + letters_used)
            .unwrap()
            .to_string()
    } else {
        panic!("TODO generate aa, ab, ac, ...");
    };

    let mut descriptor = subs.get(root);

    match descriptor.content {
        FlexVar(None) => {
            descriptor.content = FlexVar(Some(generated_name.into()));

            // TODO is this necessary, or was mutating descriptor in place sufficient?
            subs.set(root, descriptor);
        }
        _ => (),
    }
}

pub fn content_to_string(content: Content, subs: &mut Subs) -> String {
    let mut buf = String::new();

    write_content(content, subs, &mut buf, false);

    buf
}

fn write_content(content: Content, subs: &mut Subs, buf: &mut String, use_parens: bool) {
    use subs::Content::*;

    match content {
        FlexVar(Some(name)) => buf.push_str(&name),
        FlexVar(None) => buf.push_str(WILDCARD),
        RigidVar(name) => buf.push_str(&name),
        Structure(flat_type) => write_flat_type(flat_type, subs, buf, use_parens),
        Error(_) => buf.push_str("<type mismatch>"),
    }
}

fn write_flat_type(flat_type: FlatType, subs: &mut Subs, buf: &mut String, use_parens: bool) {
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
            use_parens,
        ),
        EmptyRecord => buf.push_str(EMPTY_RECORD),
        Func(args, ret) => write_fn(args, ret, subs, buf, use_parens),
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
    use_parens: bool,
) {
    let write_parens = use_parens && !args.is_empty();

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

        write_content(arg_content, subs, &mut arg_param, true);

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

        write_content(arg_content, subs, buf, true);

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
            write_content(subs.get(arg).content, subs, buf, true);
        }

        if write_parens {
            buf.push_str(")");
        }
    }
}

fn write_fn(
    args: Vec<Variable>,
    ret: Variable,
    subs: &mut Subs,
    buf: &mut String,
    use_parens: bool,
) {
    let mut needs_comma = false;

    if use_parens {
        buf.push_str("(");
    }

    for arg in args {
        if needs_comma {
            buf.push_str(", ");
        } else {
            needs_comma = true;
        }

        write_content(subs.get(arg).content, subs, buf, false);
    }

    buf.push_str(" -> ");
    write_content(subs.get(ret).content, subs, buf, false);

    if use_parens {
        buf.push_str(")");
    }
}
