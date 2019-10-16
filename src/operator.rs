use self::Operator::*;
use can::symbol::Symbol;
use std::cmp::Ordering;

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
            Equals | LessThan | GreaterThan | LessThanOrEq | GreaterThanOrEq => NonAssociative,
        }
    }

    fn precedence(&self) -> u8 {
        match self {
            Caret => 7,
            Star | Slash | DoubleSlash | DoublePercent | Percent => 6,
            Plus | Minus => 5,
            Equals | LessThan | GreaterThan | LessThanOrEq | GreaterThanOrEq => 4,
            And => 3,
            Or => 2,
            Pizza => 1,
        }
    }

    pub fn desugar(&self) -> Symbol {
        match self {
            Caret => Symbol::new("Num", "pow"),
            Star => Symbol::new("Num", "mul"),
            Slash => Symbol::new("Float", "div"),
            DoubleSlash => Symbol::new("Int", "div"),
            Percent => Symbol::new("Num", "rem"),
            DoublePercent => Symbol::new("Num", "mod"),
            Plus => Symbol::new("Num", "plus"),
            Minus => Symbol::new("Num", "sub"),
            Equals => Symbol::new("Bool", "isEqual"),
            LessThan => Symbol::new("Num", "isLessThan"),
            GreaterThan => Symbol::new("Num", "isGreaterThan"),
            LessThanOrEq => Symbol::new("Num", "isLessThanOrEqualTo"),
            GreaterThanOrEq => Symbol::new("Num", "isGreaterThanOrEqualTo"),
            And => Symbol::new("Bool", "and"),
            Or => Symbol::new("Bool", "or"),
            Pizza => panic!("Cannot desugar the |> operator"),
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
