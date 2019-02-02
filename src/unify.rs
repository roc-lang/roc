use std::collections::BTreeSet;
use std::collections::BTreeMap;
use self::Type::*;

pub type Name<'a> = &'a str;

pub type ModuleName<'a> = &'a str;

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum Type<'a> {
    Unbound,
    String,
    Char,
    Int,
    Float,
    Number,
    Symbol(&'a str),
    Array(Box<Type<'a>>),
    Function(Box<Type<'a>>, Box<Type<'a>>),
    Record(BTreeMap<Name<'a>, Type<'a>>),
    Tuple(Vec<Type<'a>>),
    Union(BTreeSet<Type<'a>>),
}

// CANONICAL IR - we have already done stuff like giving errors for
// duplicate field names

#[derive(Debug, PartialEq)]
pub enum Expr<'a> {
    // Variables
    Declaration(&'a Pattern<'a>, Box<&'a Expr<'a>>, Box<Expr<'a>>),
    LookupLocal(&'a Name<'a>),
    LookupGlobal(&'a ModuleName<'a>, &'a Name<'a>),

    // Scalars
    Symbol(&'a str),
    String(&'a str),
    Char(char),
    HexOctalBinary(i64),   // : Int
    FractionalNumber(f64), // : Float
    WholeNumber(i64),      // : Int | Float

    // Collections
    Array(Vec<Expr<'a>>),
    Record(Vec<(&'a Name<'a>, &'a Expr<'a>)>),
    Tuple(Vec<&'a Expr<'a>>),
    LookupName(Name<'a>, Box<&'a Expr<'a>>),
    // TODO add record update

    // Functions
    Function(&'a Pattern<'a>, &'a Expr<'a>),
    Call(Box<&'a Expr<'a>>, Box<&'a Expr<'a>>),
    CallOperator(&'a Operator, Box<&'a Expr<'a>>, Box<&'a Expr<'a>>),

    // Conditionals
    If(Box<&'a Expr<'a>> /* Conditional */, Box<&'a Expr<'a>> /* True branch */, Box<&'a Expr<'a>> /* False branch */),
    Case(Box<&'a Expr<'a>>, Vec<(&'a Pattern<'a>, &'a Expr<'a>)>),
}

#[derive(Debug, PartialEq)]
pub enum Operator {
    Plus, Minus, Star, Caret, Percent, FloatDivision, IntDivision,
    GT, GTE, LT, LTE,
    EQ, NE, And, Or,
    QuestionMark, Or
}

#[derive(Debug, PartialEq)]
pub enum Pattern<'a> {
    Name(&'a Name<'a>),                // `foo =`
    As(&'a Name<'a>, &'a Pattern<'a>), // `<pattern> as foo`
    Type(&'a Type<'a>),
    Symbol(&'a str),
    String(&'a str),
    Char(char),
    WholeNumber(&'a str),
    FractionalNumber(&'a str),
    HexOctalBinary(&'a str),
    Tuple(Vec<Pattern<'a>>),
    Record(Vec<(Name<'a>, Option<Pattern<'a>>)>), // { a = 5, b : Int as x, c }
}


pub fn infer<'a>(expr: &Expr<'a>) -> Result<Type<'a>, UnificationProblem> {
    match expr {
        Expr::String(_) => Ok(String),
        Expr::Char(_) => Ok(Char),
        Expr::HexOctalBinary(_) => Ok(Int),
        Expr::FractionalNumber(_) => Ok(Float),
        Expr::WholeNumber(_) => Ok(Number),
        Expr::Symbol(sym) => Ok(Symbol(sym)),
        Expr::Array(elem_exprs) => {
            let elem_type;

            if elem_exprs.is_empty() {
                elem_type = Unbound;
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
                    elem_type = Union(unified_type)
                }
            }

            Ok(Array(Box::new(elem_type)))
        },
        Expr::Record(fields) => {
            let mut rec_type: BTreeMap<&'a Name<'a>, Type<'a>> = BTreeMap::new();

            for (field, subexpr) in fields {
                let field_type = infer(subexpr)?;

                rec_type.insert(&field, field_type);
            }

            Ok(Record(rec_type))
        },
        Expr::Tuple(exprs) => {
            let mut tuple_type: Vec<Type<'a>> = Vec::new();

            for subexpr in exprs {
                let field_type = infer(subexpr)?;

                tuple_type.push(field_type);
            }

            Ok(Tuple(tuple_type))
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
                Ok(Union(unified_type))
            }
        },
        Call(func, arg) => {
            
        },
        CallOperator(op, left_expr, right_expr) => {
            let left = &(infer(left_expr)?);
            let right = &(infer(right_expr)?);

            match op {
                Operator::EQ | Operator::NE | Operator::And | Operator::Or => {
                    if types_match(left, right) {
                        conform_to_bool(left)
                    } else {
                        Err(UnificationProblem::TypeMismatch)
                    }
                },
                Operator::Plus | Operator::Minus | Operator::Star 
                    | Operator::GT | Operator::LT | Operator::GTE | Operator::LTE 
                    | Operator::Caret | Operator::Percent => {
                    if types_match(left, right) {
                        conform_to_number(left)
                    } else {
                        Err(UnificationProblem::TypeMismatch)
                    }
                },
                Operator::FloatDivision => {
                    if matches_float_type(left) && matches_float_type(right) {
                        Ok(&Float)
                    } else {
                        Err(UnificationProblem::TypeMismatch)
                    }
                },
                Operator::IntDivision => {
                    if matches_int_type(left) && matches_int_type(right) {
                        Ok(&Int)
                    } else {
                        Err(UnificationProblem::TypeMismatch)
                    }
                },
                Operator::CombineStrings => {
                    if matches_string_type(left) && matches_string_type(right) {
                        Ok(&String)
                    } else {
                        Err(UnificationProblem::TypeMismatch)
                    }
                },
                Operator::QuestionMark => {
                    if types_match(left, right) {
                        conform_to_optional(left)
                    } else {
                        Err(UnificationProblem::TypeMismatch)
                    }
                }
            }
        },
        Expr::Declaration(pattern, let_expr, in_expr) => {
            // Think of this as a let..in even though syntactically it's not.
            // We need to type-check the let-binding, but the type of the 
            // *expression* we're expaning is only affected by the in-block.
            check_pattern(&pattern, &let_expr)?;

            infer(in_expr)
        }
    }
}

fn types_match<'a>(first: &'a Type<'a>, second: &'a Type<'a>) -> bool {
    match (first, second) {
        (Type::Union(first_types), Type::Union(second_types)) => {
            // If any type is not directly present in the other union,
            // it must at least match *some* type in the other union
            first_types.difference(second_types).into_iter().all(|not_in_second_type| {
                second_types.iter().any(|second_type| types_match(second_type, not_in_second_type))
            }) &&
            second_types.difference(first_types).into_iter().all(|not_in_first_type| {
                first_types.iter().any(|first_type| types_match(first_type, not_in_first_type))
            })
        },

        // Happy path: try these first, since we expect them to succeed.
        // These are sorted based on a vague guess of how often they will be used in practice.
        (Type::Symbol(sym_one), Type::Symbol(sym_two)) => sym_one == sym_two,
        (Type::String, Type::String) => true,
        (Type::Unbound, _) | (_, Type::Unbound)=> true,
        (Type::Array(box elem_type_one), Type::Array(box elem_type_two)) => {
            types_match(elem_type_one, elem_type_two)
        },
        (Type::Number, Type::Number) => true,
        (Type::Number, other) => matches_number_type(other),
        (other, Type::Number) => matches_number_type(other),
        (Type::Int, Type::Int) => true,
        (Type::Float, Type::Float) => true,
        (Type::Tuple(first_elems), Type::Tuple(second_elems)) => {
            // TODO verify that the elems and their types match up
            // TODO write some scenarios to understand these better -
            // like, what happens if you have a function that takes 
            // a lambda whose argument takes an open record,
            // and you pass a lamba whose argument takes *fewer* fields?
            // that should work! the function is gonna pass it a lambda that
            // has more fields than it needs.
            // I think there's an element of directionality here that I'm
            // disregarding. Maybe this function shouldn't commute.
        },
        (Type::Function(first_arg), Type::Function(second_arg)) => {
            // TODO verify that the elems and their types match up
        },
        (Type::Record(first_fields), Type::Record(second_fields)) => {
            // TODO verify that the fields and their types match up
            // TODO what should happen if one is a superset of the other? fail?
        },
        (Type::Char, Type::Char) => true,

        // Unhappy path - expect these to fail, so check them last
        (Type::Union(first_types), _)  => {
            first_types.iter().all(|typ| types_match(typ, second))
        },
        (_, Type::Union(second_types))  => {
            second_types.iter().all(|typ| types_match(first, typ))
        },
        (Type::String, _) | (_, Type::String) => false,
        (Type::Char, _) | (_, Type::Char) => false,
        (Type::Int, _) | (_, Type::Int) => false,
        (Type::Float, _) | (_, Type::Float) => false,
        (Type::Symbol(_), _) | (_, Type::Symbol(_)) => false,
        (Type::Array(_), _) | (_, Type::Array(_)) => false,
        (Type::Record(_), _) | (_, Type::Record(_)) => false,
        (Type::Tuple(_), _) | (_, Type::Tuple(_)) => false,
        (Type::Function(_, _), _) | (_, Type::Function(_, _)) => false,
    }
}


fn check_pattern<'a>(pattern: &'a Pattern<'a>, expr: &'a Expr<'a>) -> Result<(), UnificationProblem> {
    let expr_type = infer(expr)?;

    panic!("TODO check the pattern's type against expr_type, then write some tests for funky record pattern cases - this is our first real unification! Next one will be field access, ooooo - gonna want lots of tests for that")
}

const TRUE_SYMBOL_STR: &'static str = "True";
const FALSE_SYMBOL_STR: &'static str = "False";

pub fn matches_string_type<'a>(candidate: &Type<'a>) -> bool {
    match candidate {
        Unbound | String => true,
        Type::Union(types) => {
            types.iter().all(|typ| matches_string_type(typ))
        },
        _ => Err(UnificationProblem::TypeMismatch)
    }
}

pub fn matches_bool_type<'a>(candidate: &Type<'a>) -> bool {
    match candidate {
        Type::Unbound => true,
        Type::Symbol(str) => str == &TRUE_SYMBOL_STR || str == &FALSE_SYMBOL_STR,
        Type::Union(types) => {
            types.iter().all(|typ| matches_bool_type(typ))
        }
        _ => false
    }
}

pub fn matches_number_type<'a>(candidate: &Type<'a>) -> bool {
    match candidate {
        Type::Unbound | Type::Int | Type::Float | Type::Number => true,
        Type::Union(types) => {
            types.iter().all(|typ| matches_number_type(typ))
        }
        _ => false
    }
}

pub fn matches_int_type<'a>(candidate: &Type<'a>) -> bool {
    match candidate {
        Type::Unbound | Type::Int => true,
        Type::Union(types) => {
            types.iter().all(|typ| matches_int_type(typ))
        }
        _ => false
    }
}

pub fn matches_float_type<'a>(candidate: &Type<'a>) -> bool {
    match candidate {
        Type::Unbound | Type::Float => true,
        Type::Union(types) => {
            types.iter().all(|typ| matches_float_type(typ))
        }
        _ => false
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

