
#[derive(Debug, PartialEq)]
pub enum Expr {
    // Literals
    Int(i64),
    Frac(i64, u64),
    String(String),
    Char(char),

    Var(String),

    // Functions
    Func(String, Box<Expr>),
    Apply(Box<Expr>, Box<Expr>),
    Operator(Box<Expr>, Operator, Box<Expr>),

    If(Box<Expr>, Box<Expr>, Box<Expr>),
    SyntaxProblem(String),
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum Operator {
    Plus, Minus, Star, Slash, DoubleSlash,
}
