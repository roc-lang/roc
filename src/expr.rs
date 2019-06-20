#[derive(Clone, Debug, PartialEq)]
pub enum Expr {
    // Literals
    Int(i64),
    Frac(i64, u64),
    EmptyStr,
    Str(String),
    InterpolatedStr(Vec<(String, Ident)>, String),
    Char(char),

    Var(Ident),
    Assign(Pattern, Box<Expr>, Box<Expr>),

    // Functions
    CallByName(Ident, Vec<Expr>),
    Apply(Box<Expr>, Vec<Expr>),
    Operator(Box<Expr>, Operator, Box<Expr>),
    Closure(Vec<Pattern>, Box<Expr>),

    // Sum Types
    ApplyVariant(String, Option<Vec<Expr>>),

    // Product Types
    EmptyRecord,

    // Conditionals
    If(Box<Expr>, Box<Expr>, Box<Expr>),
    Case(Box<Expr>, Vec<(Pattern, Box<Expr>)>),
}

pub type Ident = String;


#[derive(Clone, Debug, PartialEq)]
pub enum Pattern {
    Identifier(String),
    Variant(String, Option<Vec<Pattern>>),
    Integer(i64),
    Fraction(i64, u64),
    EmptyRecordLiteral,
    Underscore
}


#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum Operator {
    Plus, Minus, Star, Slash, DoubleSlash, Equals,
}
