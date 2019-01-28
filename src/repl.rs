use interpret::{eval, literal_to_string};
use unify::infer;
use unify::Expr;
use unify::Type;

pub fn eval_and_print<'a>(expr: &Expr<'a>) -> String {
    match infer(&expr) {
        Ok(typ) => {
            let lit = eval(expr);

            format!("{}\n: {}", literal_to_string(lit), type_to_string(&typ))
        },
        Err(_) =>
            "[TYPE MISMATCH!]".to_string()
    }
}

pub fn type_to_string<'a>(typ: &'a Type<'a>) -> String {
    match typ {
        Type::String => "String".to_string(),
        Type::Int => "Int".to_string(),
        Type::Float => "Float".to_string(),
        Type::Number => "Int | Float".to_string(),
        Type::Symbol(sym) => format!(":{}", sym),
        Type::Record(fields) => {
            let field_strings = fields.into_iter().map(|(field, subtyp)| {
                let typ_str = type_to_string(subtyp);

                format!("{} : {}", field, typ_str)
            });

            format!("{{ {} }}", field_strings.collect::<Vec<String>>().join(", "))
        }
        Type::Assignment(_, assigned_typ) => type_to_string(assigned_typ),
        Type::Union(set) => {
            set.into_iter().collect::<Vec<&'a Type<'a>>>().into_iter().map(|typ_in_set| {
                type_to_string(typ_in_set)
            }).collect::<Vec<String>>().join(" | |")
        }
        
    }
}
