use std::collections::HashMap;

pub type Ident<'a> = &'a str;

pub type Field<'a> = &'a str;

#[derive(Debug, PartialEq)]
pub enum Type<'a> {
    String,
    Record(HashMap<Field<'a>, Type<'a>>),
    Assignment(Ident<'a>, Box<Type<'a>>)
}


// CANONICAL IR - we have already done stuff like giving errors for
// duplicate field names

#[derive(Debug, PartialEq)]
pub enum Expr<'a> {
    Literal(Literal<'a>),
    Assignment(Ident<'a>, Box<Expr<'a>>),
    // TODO add record update
    // TODO add conditional
    // TODO add function
}

#[derive(Debug, PartialEq)]
pub enum Literal<'a> {
    String(&'a str),
    Record(Vec<(Field<'a>, Expr<'a>)>)
}


pub fn infer<'a>(expr: Expr<'a>) -> Type<'a> {
    match expr {
        Expr::Literal(Literal::String(_)) => Type::String,
        Expr::Literal(Literal::Record(fields)) => {
            let mut rec_type: HashMap<&'a str, Type<'a>> = HashMap::new();

            for (field, subexpr) in fields {
                let field_type = infer(subexpr);

                rec_type.insert(&field, field_type);
            }

            Type::Record(rec_type)
        },
        Expr::Assignment(ident, subexpr) => {
            Type::Assignment(ident, Box::new(infer(*subexpr)))
        }
    }
}
