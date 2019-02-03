

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum Operator {
    Plus, Minus, FloatDivision, IntDivision,
}

#[derive(Debug, PartialEq)]
pub enum Expr {
    HexOctalBinary(i64),    // : Int
    FractionalNumber(f64),  // : Float
    WholeNumber(i64),       // : Int | Float

    // Functions
    CallOperator(Operator, Box<Expr>, Box<Expr>),
}
