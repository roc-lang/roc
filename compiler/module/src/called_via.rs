use self::BinOp::*;
use std::cmp::Ordering;
use std::fmt;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum CalledVia {
    /// Calling with space, e.g. (foo bar)
    Space,

    /// Calling with an operator, e.g. (bar |> foo) or (1 + 2)
    BinOp(BinOp),

    /// Calling with a unary operator, e.g. (!foo bar baz) or (-foo bar baz)
    UnaryOp(UnaryOp),

    /// This call is the result of desugaring string interpolation,
    /// e.g. "\(first) \(last)" is transformed into Str.concat (Str.concat first " ") last.
    StringInterpolation,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum UnaryOp {
    /// (-), e.g. (-x)
    Negate,
    /// (!), e.g. (!x)
    Not,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum BinOp {
    // highest precedence
    Caret,
    Star,
    Slash,
    DoubleSlash,
    Percent,
    DoublePercent,
    Plus,
    Minus,
    Equals,
    NotEquals,
    LessThan,
    GreaterThan,
    LessThanOrEq,
    GreaterThanOrEq,
    And,
    Or,
    Pizza,
    Assignment,
    HasType,
    Backpassing,
    // lowest precedence
}

impl BinOp {
    /// how wide this operator is when typed out
    pub fn width(self) -> u16 {
        match self {
            Caret | Star | Slash | Percent | Plus | Minus | LessThan | GreaterThan => 1,
            DoubleSlash | DoublePercent | Equals | NotEquals | LessThanOrEq | GreaterThanOrEq
            | And | Or | Pizza => 2,
            Assignment | HasType | Backpassing => unreachable!(),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum ArgSide {
    Left,
    Right,
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum Associativity {
    /// left-associative operators:
    ///
    /// arithmetic: * / // % + -
    /// application: |>
    LeftAssociative,

    /// right-associative operators:
    ///
    /// exponentiation: ^
    /// boolean: && ||
    /// application: <|
    RightAssociative,

    /// non-associative operators:
    ///
    /// comparison: == > >= < <=
    NonAssociative,
}

impl BinOp {
    pub fn associativity(self) -> Associativity {
        use self::Associativity::*;

        match self {
            Pizza | Star | Slash | DoubleSlash | DoublePercent | Percent | Plus | Minus => {
                LeftAssociative
            }
            And | Or | Caret => RightAssociative,
            Equals | NotEquals | LessThan | GreaterThan | LessThanOrEq | GreaterThanOrEq => {
                NonAssociative
            }
            Assignment | HasType | Backpassing => unreachable!(),
        }
    }

    fn precedence(self) -> u8 {
        match self {
            Caret => 7,
            Star | Slash | DoubleSlash | DoublePercent | Percent => 6,
            Plus | Minus => 5,
            Equals | NotEquals | LessThan | GreaterThan | LessThanOrEq | GreaterThanOrEq => 4,
            And => 3,
            Or => 2,
            Pizza => 1,
            Assignment | HasType | Backpassing => unreachable!(),
        }
    }
}

impl PartialOrd for BinOp {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for BinOp {
    fn cmp(&self, other: &Self) -> Ordering {
        self.precedence().cmp(&other.precedence())
    }
}

impl std::fmt::Display for BinOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let as_str = match self {
            Caret => "^",
            Star => "*",
            Slash => "/",
            DoubleSlash => "//",
            Percent => "%",
            DoublePercent => "%%",
            Plus => "+",
            Minus => "-",
            Equals => "==",
            NotEquals => "!=",
            LessThan => "<",
            GreaterThan => ">",
            LessThanOrEq => "<=",
            GreaterThanOrEq => ">=",
            And => "&&",
            Or => "||",
            Pizza => "|>",
            Assignment => "=",
            HasType => ":",
            Backpassing => "<-",
        };

        write!(f, "{}", as_str)
    }
}
