use subs::{Subs, Content, FlatType};

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
        Error => buf.push_str("<type mismatch>")
    }
}


fn write_flat_type(flat_type: FlatType, subs: &mut Subs, buf: &mut String, use_parens: bool) {
    use subs::FlatType::*;

    match flat_type {
        Apply(module_name, type_name, vars) => {
            if use_parens {
                buf.push_str("(");
            }

            buf.push_str(&format!("{}.{}", module_name, type_name));

            for var in vars {
                buf.push_str(" ");
                write_content(subs.get(var).content, subs, buf, true);
            }

            if use_parens {
                buf.push_str(")");
            }
        },
        EmptyRecord => buf.push_str(EMPTY_RECORD),
        Func(_, _) => panic!("TODO write_flat_type for Func")
    }
}
