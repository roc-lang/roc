use unify::Expr;
use unify::Literal;

pub fn eval<'a>(expr: &'a Expr<'a>) -> &'a Literal<'a> {
    match expr {
        Expr::Literal(literal) => literal,
        Expr::Assignment(_, subexpr) => eval(subexpr),
        Expr::If(cond, if_true, if_false) => {
            match eval(cond) {
                Literal::Symbol("True") => eval(if_true),
                Literal::Symbol("False") => eval(if_false),
                _ => {
                    panic!("somehow an if-conditional did not evaluate to True or False!")
                }
            }
        }
    }
}

pub fn literal_to_string<'a>(literal: &'a Literal<'a>) -> String {
    match literal {
        Literal::String(str) => format!("\"{}\"", str),
        Literal::Char(character) => format!("'{}'", character),
        Literal::Symbol(str) => str.to_string(),
        Literal::HexOctalBinary(str) => str.to_string(),
        Literal::Number(str) => str.to_string(),
        Literal::Record(field_exprs) => {
            let mut field_strings = Vec::new();

            for (field, subexpr) in field_exprs {
                let val = literal_to_string(eval(subexpr));

                field_strings.push(format!("{} = {}", field, val));
            }

            format!("{{ {} }}", field_strings.join(", "))
        },
        Literal::Tuple(elem_exprs) => {
            let mut elem_strings = Vec::new();

            for elem_expr in elem_exprs {
                elem_strings.push(literal_to_string(eval(elem_expr)));
            }

            format!("({})", elem_strings.join(", "))
        },
        Literal::Array(elem_exprs) => {
            let mut elem_strings = Vec::new();

            for elem_expr in elem_exprs {
                elem_strings.push(literal_to_string(eval(elem_expr)));
            }

            format!("[{}]", elem_strings.join(", "))
        },
    }
}
