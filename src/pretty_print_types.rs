use subs::{Subs, Content, FlatType, Variable};

static WILDCARD: &'static str = "*";
static EMPTY_RECORD: &'static str = "{}";

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
        Error(_) => buf.push_str("<type mismatch>")
    }
}

fn write_flat_type(flat_type: FlatType, subs: &mut Subs, buf: &mut String, use_parens: bool) {
    use subs::FlatType::*;

    match flat_type {
        Apply(module_name, type_name, args) => {
            let write_parens = use_parens && !args.is_empty();

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
        },
        EmptyRecord => buf.push_str(EMPTY_RECORD),
        Func(args, ret) => {
            write_fn(args, ret, subs, buf, use_parens)
        },
        Operator(l_arg, r_arg, ret) => {
            write_fn(vec![l_arg, r_arg], ret, subs, buf, use_parens)
        }
        Erroneous(problem) => {
            buf.push_str(&format!("<Type Mismatch: {:?}>", problem));
        }
    }
}

fn write_fn(args: Vec<Variable>, ret: Variable, subs: &mut Subs, buf: &mut String, use_parens: bool) {
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
