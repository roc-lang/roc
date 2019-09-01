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
            write_apply(module_name, type_name, args, subs, buf, use_parens)
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

fn write_apply(module_name: String, type_name: String, args: Vec<Variable>, subs: &mut Subs, buf: &mut String, use_parens: bool) {
    let write_parens = use_parens && !args.is_empty();

    // Hardcoded type aliases
    if module_name == "Num" && type_name == "Num" {
        let arg = args.into_iter().next().unwrap_or_else(|| {
            panic!("Num did not have any type parameters somehow.")
        });
        let arg_content = subs.get(arg).content;
        let mut arg_param = String::new();

        write_content(arg_content, subs, &mut arg_param, true);

        if arg_param == "Int.Integer" {
            buf.push_str("Int");
        } else if arg_param == "Float.FloatingPoint" {
            buf.push_str("Float");
        } else {
            if write_parens { buf.push_str("("); }

            buf.push_str("Num ");
            buf.push_str(&arg_param);

            if write_parens { buf.push_str(")"); }
        }
    } else if module_name == "List" && type_name == "List" {
        if write_parens { buf.push_str("("); }

        buf.push_str("List ");

        let arg = args.into_iter().next().unwrap_or_else(|| {
            panic!("List did not have any type parameters somehow.")
        });
        let arg_content = subs.get(arg).content;

        write_content(arg_content, subs, buf, true);

        if write_parens { buf.push_str(")"); }
    } else {
        if write_parens { buf.push_str("("); }

        buf.push_str(&format!("{}.{}", module_name, type_name));

        for arg in args {
            buf.push_str(" ");
            write_content(subs.get(arg).content, subs, buf, true);
        }

        if write_parens { buf.push_str(")"); }
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
