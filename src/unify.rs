use std::collections::HashMap;
use std::collections::HashSet;
use std::hash::{Hash, Hasher};
use std::cmp::Ordering;

pub type Ident<'a> = &'a str;

pub type Field<'a> = &'a str;

#[derive(Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum Type<'a> {
    String,
    Symbol(&'a str),
    Record(Vec<(Field<'a>, Type<'a>)>),
    Assignment(Ident<'a>, Box<Type<'a>>),
    Union(TypeSet<'a>),
}

#[derive(Debug)]
struct TypeSet<'a>(HashSet<Type<'a>>);

impl<'a> PartialOrd for TypeSet<'a> {
    fn partial_cmp(&self, other: &TypeSet<'a>) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl<'a> Ord for TypeSet<'a> {
    fn cmp(&self, other: &TypeSet<'a>) -> Ordering {
        panic!("TypeSet does not support ordering or equality. Do not use them!");
    }
}

impl<'a> PartialEq for TypeSet<'a> {
    fn eq(&self, other: &TypeSet) -> bool {
        match (self, other) {
            (TypeSet(my_set), TypeSet(other_set)) => {
                // Delegate to HashSet's eq
                my_set == other_set
            }
        }
    }
}

impl<'a> Eq for TypeSet<'a> {}

impl<'a> Hash for TypeSet<'a> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        panic!("at the dissco");
        // self.id.hash(state);
        // self.phone.hash(state);
    }
}

// CANONICAL IR - we have already done stuff like giving errors for
// duplicate field names

#[derive(Debug, PartialEq)]
pub enum Expr<'a> {
    Literal(&'a Literal<'a>),
    Assignment(Ident<'a>, Box<&'a Expr<'a>>),
    If(Box<&'a Expr<'a>> /* Conditional */, Box<&'a Expr<'a>> /* True branch */, Box<&'a Expr<'a>> /* False branch */),
    // TODO add record update
    // TODO add conditional
    // TODO add function
}

#[derive(Debug, PartialEq)]
pub enum Literal<'a> {
    String(&'a str),
    Symbol(&'a str),
    Record(Vec<(Field<'a>, &'a Expr<'a>)>)
}


pub fn infer<'a>(expr: &Expr<'a>) -> Result<Type<'a>, ()> {
    match expr {
        Expr::Literal(Literal::String(_)) => Ok(Type::String),
        Expr::Literal(Literal::Symbol(sym)) => Ok(Type::Symbol(sym)),
        Expr::Literal(Literal::Record(fields)) => {
            let mut rec_type: HashMap<&'a str, Type<'a>> = HashMap::new();

            for (field, subexpr) in fields {
                let field_type = infer(subexpr)?;

                rec_type.insert(&field, field_type);
            }

            Ok(Type::Record(rec_type))
        },
        Expr::If(cond, expr_if_true, expr_if_false) => {
            panic!("TODO union the types of expr_if_true and expr_if_false");
        },
        Expr::Assignment(ident, subexpr) => {
            Ok(Type::Assignment(ident, Box::new(infer(subexpr)?)))
        }
    }
}
