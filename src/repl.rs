use interpret::{eval, literal_to_string};
use unify::infer;
use unify::Expr;
use unify::Field;
use unify::Type;

pub fn eval_and_print<'a>(expr: &Expr<'a>) -> String {
    let lit = eval(expr);
    let typ = infer(&Expr::Literal(lit));

    format!("{}\n: {}", literal_to_string(lit), type_to_string(typ))
}

pub fn type_to_string<'a>(typ: Type<'a>) -> String {
    match typ {
        Type::String => "String".to_string(),
        Type::Record(fields) => {
            let mut field_strings = Vec::new();
            let mut field_pairs = fields.into_iter().collect::<Vec<(Field<'a>, Type<'a>)>>();

            // Sort record fields alphabetically in the type
            field_pairs.sort_unstable_by(|(a, _), (b, _)| a.partial_cmp(b).unwrap());

            for (field, subtyp) in field_pairs {
                let typ_str = type_to_string(subtyp);

                field_strings.push(format!("{} : {}", field, typ_str));
            }

            format!("{{ {} }}", field_strings.join(", "))
        }
        Type::Assignment(_, typ) => type_to_string(*typ),
        
    }
}
