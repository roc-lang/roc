
#[derive(Clone, Debug, PartialEq)]
pub enum Expr {
    // Literals
    Int(i64),
    Frac(i64, u64),
    String(String),
    Char(char),

    Var(String),
    Let(Pattern, Box<Expr>, Box<Expr>),
    Pattern(Pattern),

    // Functions
    Func(String, Box<Expr>),
    Apply(Box<Expr>, Box<Expr>),
    Operator(Box<Expr>, Operator, Box<Expr>),
    Closure(Vec<Pattern>, Box<Expr>),

    If(Box<Expr>, Box<Expr>, Box<Expr>),
}

#[derive(Clone, Debug, PartialEq)]
pub enum Pattern {
    Identifier(String),
    Underscore
}


#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum Operator {
    Plus, Minus, Star, Slash, DoubleSlash, Equals,
}
