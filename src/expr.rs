use name::Name;
use typ::Type;

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum Operator {
    Plus, Minus, Star, Slash, DoubleSlash,
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum Builtin {
    // Default
    Negate,
    Not,

    // String
    // StringLength,
}


#[derive(Debug, PartialEq)]
pub enum Expr {
    Int(i64),
    Ratio(i64, u64),

    // Functions
    CallOperator(Box<Expr>, Operator, Box<Expr>),
    CallBuiltin(Builtin, Box<Expr>),
    CallLambda(Box<Expr>, Box<Expr>),
}

#[derive(Debug, PartialEq)]
pub enum Pattern {
    Name(Name),             // `foo =`
    As(Name, Box<Pattern>), // `<pattern> as foo`
    Type(Type),
    Symbol(String),
    String(String),
    Char(char),
    Int(i64),
    Float(f64),
    Tuple(Vec<Pattern>),
    Record(Vec<(Name, Option<Pattern>)>), // { a = 5, b : Int as x, c }
}

