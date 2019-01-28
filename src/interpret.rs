use unify::Expr;
use unify::Literal;

pub fn eval<'a>(expr: &'a Expr<'a>) -> &'a Literal<'a> {
    match expr {
        Expr::Literal(literal) => literal,
        Expr::Assignment(_, subexpr) => eval(subexpr)
    }
}

pub fn literal_to_string<'a>(literal: &'a Literal<'a>) -> String {
    match literal {
        Literal::String(str) => format!("\"{}\"", str),
        Literal::Record(fields) => {
            let mut field_strings = Vec::new();

            for (field, subexpr) in fields {
                let val = literal_to_string(eval(subexpr));

                field_strings.push(format!("{} = {}", field, val));
            }

            format!("{{ {} }}", field_strings.join(", "))
        },
    }
}
