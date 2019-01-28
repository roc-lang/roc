use std::collections::BTreeSet;

pub type Ident<'a> = &'a str;

pub type Field<'a> = &'a str;

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum Type<'a> {
    Unbound,
    String,
    Int,
    Float,
    Number,
    Symbol(&'a str),
    Array(Box<Type<'a>>),
    Record(Vec<(Field<'a>, Type<'a>)>),
    Tuple(Vec<Type<'a>>),
    Assignment(Ident<'a>, Box<Type<'a>>),
    Union(BTreeSet<Type<'a>>),
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
    Number(&'a str),
    Symbol(&'a str),
    Array(Vec<Expr<'a>>),
    Record(Vec<(Field<'a>, &'a Expr<'a>)>),
    Tuple(Vec<&'a Expr<'a>>)
}


pub fn infer<'a>(expr: &Expr<'a>) -> Result<Type<'a>, UnificationProblem> {
    match expr {
        Expr::Literal(Literal::String(_)) => Ok(Type::String),
        Expr::Literal(Literal::Number(_)) => Ok(Type::Number),
        Expr::Literal(Literal::Symbol(sym)) => Ok(Type::Symbol(sym)),
        Expr::Literal(Literal::Array(elem_exprs)) => {
            let elem_type;

            if elem_exprs.is_empty() {
                elem_type = Type::Unbound;
            } else {
                let mut unified_type = BTreeSet::new();

                // Unify the types of all the elements
                for elem_expr in elem_exprs {
                    unified_type.insert(infer(&elem_expr)?);
                }

                if unified_type.len() == 1 {
                    // No point in storing a union of 1.
                    elem_type = unified_type.into_iter().next().unwrap()
                } else {
                    elem_type = Type::Union(unified_type)
                }
            }

            Ok(Type::Array(Box::new(elem_type)))
        },
        Expr::Literal(Literal::Record(fields)) => {
            let mut rec_type: Vec<(&'a str, Type<'a>)> = Vec::new();

            for (field, subexpr) in fields {
                let field_type = infer(subexpr)?;

                rec_type.push((&field, field_type));
            }

            Ok(Type::Record(rec_type))
        },
        Expr::Literal(Literal::Tuple(exprs)) => {
            let mut tuple_type: Vec<Type<'a>> = Vec::new();

            for subexpr in exprs {
                let field_type = infer(subexpr)?;

                tuple_type.push(field_type);
            }

            Ok(Type::Tuple(tuple_type))
        },
        Expr::If(box cond, expr_if_true, expr_if_false) => {
            let cond_type = infer(&cond)?;

            // if-conditionals must be of type Bool
            if !matches_bool_type(&cond_type) {
                return Err(UnificationProblem::IfConditionNotBool);
            }

            // unify the true and false branches
            let true_type = infer(&expr_if_true)?;
            let false_type = infer(&expr_if_false)?;

            let mut unified_type = BTreeSet::new();

            unified_type.insert(true_type);
            unified_type.insert(false_type);

            if unified_type.len() == 1 {
                // No point in storing a union of 1.
                //
                // We can't reuse true_type because it's been moved into the set
                // but we can pull it back out of the set
                Ok(unified_type.into_iter().next().unwrap())
            } else {
                Ok(Type::Union(unified_type))
            }
        },
        Expr::Assignment(ident, subexpr) => {
            Ok(Type::Assignment(ident, Box::new(infer(subexpr)?)))
        }
    }
}

const TRUE_SYMBOL_STR: &'static str = "True";
const FALSE_SYMBOL_STR: &'static str = "False";

pub fn matches_bool_type<'a>(candidate: &Type<'a>) -> bool {
    match candidate {
        Type::Symbol(str) => {
            str == &TRUE_SYMBOL_STR || str == &FALSE_SYMBOL_STR
        }
        Type::Union(types) => {
            types.len() <= 2 && types.iter().all(|typ| matches_bool_type(typ))
        }
        _ => {
            false
        }
    }
}

#[derive(Debug)]
pub enum UnificationProblem {
    CannotUnifyAssignments,
    NotMemberOfUnion,
    TypeMismatch,
    IfConditionNotBool,
    SymbolMismatch
}

