use interpret::{eval, literal_to_string};
use unify::infer;
use unify::Expr;
use unify::Type;

pub fn eval_and_print<'a>(expr: &Expr<'a>) -> String {
    match infer(&expr) {
        Ok(typ) => {
            let lit = eval(expr);

            format!("{}\n: {}", literal_to_string(lit), type_to_string(true, &typ))
        },
        Err(_) =>
            "[TYPE MISMATCH!]".to_string()
    }
}

pub fn type_to_string<'a>(outermost: bool, typ: &'a Type<'a>) -> String {
    match typ {
        Type::Unbound => "*".to_string(),
        Type::String => "String".to_string(),
        Type::Char => "Char".to_string(),
        Type::Int => "Int".to_string(),
        Type::Float => "Float".to_string(),
        Type::Number => "Int | Float".to_string(),
        Type::Symbol(sym) => format!(":{}", sym),
        Type::Array(elem_type) => {
            let str = format!("Array {}", type_to_string(false, elem_type));

            if outermost {
                str
            } else {
                format!("({})", str)
            }
        },
        Type::Record(fields) => {
            let field_strings = fields.into_iter().map(|(field, subtyp)| {
                let typ_str = type_to_string(false, subtyp);

                format!("{} : {}", field, typ_str)
            });

            format!("{{ {} }}", field_strings.collect::<Vec<String>>().join(", "))
        },
        Type::Tuple(elems) => {
            let elem_strings = elems.into_iter().map(|subtyp| { type_to_string(false, subtyp) });
            let str = elem_strings.collect::<Vec<String>>().join(", ");

            if outermost {
                str
            } else {
                format!("({})", str)
            }
        }
        Type::Assignment(_, assigned_typ) => type_to_string(outermost, assigned_typ),
        Type::Union(set) => {
            set.into_iter().collect::<Vec<&'a Type<'a>>>().into_iter().map(|typ_in_set| {
                type_to_string(false, typ_in_set)
            }).collect::<Vec<String>>().join(" | ")
        }
        
    }
}
