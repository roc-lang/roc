use self::Type::*;

pub type Name<'a> = &'a str;

pub type ModuleName<'a> = &'a str;

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum Type<'a> {
    Symbol(&'a str),
    Int,
    Float,
    Number,
    Function(Box<Type<'a>>, Box<Type<'a>>),
}


#[derive(Debug, PartialEq)]
pub enum Expr<'a> {
    HexOctalBinary(i64),    // : Int
    FractionalNumber(f64),  // : Float
    WholeNumber(i64),       // : Int | Float

    // Functions
    CallOperator(Operator, Box<&'a Expr<'a>>, Box<&'a Expr<'a>>),
}

#[derive(Debug, PartialEq)]
pub enum Operator {
    Plus, Minus, FloatDivision, IntDivision,
}

#[derive(Debug, PartialEq)]
pub enum Problem {
    Mismatch
}

pub fn infer_type<'a>(expr: Expr<'a>) -> Result<Type<'a>, Problem> {
    Err(Problem::Mismatch)
}
