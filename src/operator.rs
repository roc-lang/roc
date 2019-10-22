use self::Operator::*;
use std::cmp::Ordering;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum CalledVia {
    /// Calling with space, e.g. (foo bar)
    Space,

    /// Calling with an operator, e.g. (bar |> foo) or (1 + 2)
    Operator(Operator),

    /// Calling with the unary (!) operator, e.g. (!foo bar baz)
    UnaryNot,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Operator {
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
    Pizza, // lowest precedence
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

impl Operator {
    pub fn associativity(&self) -> Associativity {
        use self::Associativity::*;

        match self {
            Pizza | Star | Slash | DoubleSlash | DoublePercent | Percent | Plus | Minus => {
                LeftAssociative
            }
            And | Or | Caret => RightAssociative,
            Equals | NotEquals | LessThan | GreaterThan | LessThanOrEq | GreaterThanOrEq => {
                NonAssociative
            }
        }
    }

    fn precedence(&self) -> u8 {
        match self {
            Caret => 7,
            Star | Slash | DoubleSlash | DoublePercent | Percent => 6,
            Plus | Minus => 5,
            Equals | NotEquals | LessThan | GreaterThan | LessThanOrEq | GreaterThanOrEq => 4,
            And => 3,
            Or => 2,
            Pizza => 1,
        }
    }
}

impl PartialOrd for Operator {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Operator {
    fn cmp(&self, other: &Self) -> Ordering {
        self.precedence().cmp(&other.precedence())
    }
}
