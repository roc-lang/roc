
#[derive(Clone, Debug, PartialEq)]
pub enum Expr {
    // Literals
    Int(i64),
    Frac(i64, u64),
    Str(String),
    Char(char),
    Bool(bool),

    Var(String),
    Let(Pattern, Box<Expr>, Box<Expr>),

    // Functions
    Func(String, Vec<Expr>),
    Apply(Box<Expr>, Vec<Expr>),
    Operator(Box<Expr>, Operator, Box<Expr>),
    Closure(Vec<Pattern>, Box<Expr>),

    If(Box<Expr>, Box<Expr>, Box<Expr>),
    Error(Problem),
}

#[derive(Clone, Debug, PartialEq)]
pub enum Problem {
    UnrecognizedVarName(String),
    TypeMismatch(String),
    ReassignedVarName(String),
    WrongArity(u32 /* Expected */, u32 /* Provided */),
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
