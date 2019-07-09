use std::cmp::Ordering;
use self::Operator::*;

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Operator {
    // highest precedence
    Caret,
    Star, Slash, DoubleSlash, Percent,
    Plus, Minus,
    Equals, LessThan, GreaterThan, LessThanOrEq, GreaterThanOrEq,
    And,
    Or,
    Pizza
    // lowest precedence
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
    NonAssociative
}

impl Operator {
	pub fn associativity(&self) -> Associativity {
        use self::Associativity::*;

        match self {
            Pizza | Star | Slash | DoubleSlash | Percent | Plus | Minus => LeftAssociative,
            And | Or | Caret => RightAssociative,
            Equals | LessThan | GreaterThan | LessThanOrEq | GreaterThanOrEq => NonAssociative
        }
    }

	fn precedence(&self) -> u8 {
        match self {
            Caret => 7,
            Star | Slash | DoubleSlash | Percent => 6,
            Plus | Minus => 5,
            Equals | LessThan | GreaterThan | LessThanOrEq | GreaterThanOrEq => 4,
            And => 3,
            Or => 2,
            Pizza => 1
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