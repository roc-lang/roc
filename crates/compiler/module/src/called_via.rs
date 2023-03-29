use self::Associativity::*;
use self::BinOp::*;
use std::cmp::Ordering;
use std::fmt;

const PRECEDENCES: [(BinOp, u8); 20] = [
    (Pizza, 6),
    (Caret, 5),
    (Star, 4),
    (Slash, 4),
    (DoubleSlash, 4),
    (Percent, 4),
    (Plus, 3),
    (Minus, 3),
    (Equals, 2),
    (NotEquals, 2),
    (LessThan, 1),
    (GreaterThan, 1),
    (LessThanOrEq, 1),
    (GreaterThanOrEq, 1),
    (And, 0),
    (Or, 0),
    // These should never come up
    (Assignment, 255),
    (IsAliasType, 255),
    (IsOpaqueType, 255),
    (Backpassing, 255),
];

const ASSOCIATIVITIES: [(BinOp, Associativity); 20] = [
    (Pizza, LeftAssociative),
    (Caret, RightAssociative),
    (Star, LeftAssociative),
    (Slash, LeftAssociative),
    (DoubleSlash, LeftAssociative),
    (Percent, LeftAssociative),
    (Plus, LeftAssociative),
    (Minus, LeftAssociative),
    (Equals, NonAssociative),
    (NotEquals, NonAssociative),
    (LessThan, NonAssociative),
    (GreaterThan, NonAssociative),
    (LessThanOrEq, NonAssociative),
    (GreaterThanOrEq, NonAssociative),
    (And, RightAssociative),
    (Or, RightAssociative),
    // These should never come up
    (Assignment, LeftAssociative),
    (IsAliasType, LeftAssociative),
    (IsOpaqueType, LeftAssociative),
    (Backpassing, LeftAssociative),
];

const DISPLAY_STRINGS: [(BinOp, &str); 20] = [
    (Pizza, "|>"),
    (Caret, "^"),
    (Star, "*"),
    (Slash, "/"),
    (DoubleSlash, "//"),
    (Percent, "%"),
    (Plus, "+"),
    (Minus, "-"),
    (Equals, "=="),
    (NotEquals, "!="),
    (LessThan, "<"),
    (GreaterThan, ">"),
    (LessThanOrEq, "<="),
    (GreaterThanOrEq, ">="),
    (And, "&&"),
    (Or, "||"),
    (Assignment, "="),
    (IsAliasType, ":"),
    (IsOpaqueType, ":="),
    (Backpassing, "<-"),
];

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
    Pizza,
    Caret,
    Star,
    Slash,
    DoubleSlash,
    Percent,
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
    Assignment,
    IsAliasType,
    IsOpaqueType,
    Backpassing,
    // lowest precedence
}

impl BinOp {
    /// how wide this operator is when typed out
    pub fn width(self) -> u16 {
        match self {
            Caret | Star | Slash | Percent | Plus | Minus | LessThan | GreaterThan => 1,
            DoubleSlash | Equals | NotEquals | LessThanOrEq | GreaterThanOrEq | And | Or
            | Pizza => 2,
            Assignment | IsAliasType | IsOpaqueType | Backpassing => unreachable!(),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum ArgSide {
    Left,
    Right,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
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
        // The compiler should never pass any of these to this function!
        debug_assert_ne!(self, Assignment);
        debug_assert_ne!(self, IsAliasType);
        debug_assert_ne!(self, IsOpaqueType);
        debug_assert_ne!(self, Backpassing);

        const ASSOCIATIVITY_TABLE: [Associativity; 20] = generate_associativity_table();

        ASSOCIATIVITY_TABLE[self as usize]
    }

    fn precedence(self) -> u8 {
        // The compiler should never pass any of these to this function!
        debug_assert_ne!(self, Assignment);
        debug_assert_ne!(self, IsAliasType);
        debug_assert_ne!(self, IsOpaqueType);
        debug_assert_ne!(self, Backpassing);

        const PRECEDENCE_TABLE: [u8; 20] = generate_precedence_table();

        PRECEDENCE_TABLE[self as usize]
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
        debug_assert_ne!(*self, Assignment);
        debug_assert_ne!(*self, IsAliasType);
        debug_assert_ne!(*self, IsOpaqueType);
        debug_assert_ne!(*self, Backpassing);

        const DISPLAY_TABLE: [&str; 20] = generate_display_table();

        write!(f, "{}", DISPLAY_TABLE[*self as usize])
    }
}

const fn generate_precedence_table() -> [u8; 20] {
    let mut table = [0u8; 20];
    let mut i = 0;

    while i < PRECEDENCES.len() {
        table[(PRECEDENCES[i].0) as usize] = PRECEDENCES[i].1;
        i += 1;
    }

    table
}

const fn generate_associativity_table() -> [Associativity; 20] {
    let mut table = [NonAssociative; 20];
    let mut i = 0;

    while i < ASSOCIATIVITIES.len() {
        table[(ASSOCIATIVITIES[i].0) as usize] = ASSOCIATIVITIES[i].1;
        i += 1;
    }

    table
}

const fn generate_display_table() -> [&'static str; 20] {
    let mut table = [""; 20];
    let mut i = 0;

    while i < DISPLAY_STRINGS.len() {
        table[(DISPLAY_STRINGS[i].0) as usize] = DISPLAY_STRINGS[i].1;
        i += 1;
    }

    table
}

#[cfg(test)]
mod tests {
    use super::{BinOp, ASSOCIATIVITIES, DISPLAY_STRINGS, PRECEDENCES};

    fn index_is_binop_u8(iter: impl Iterator<Item = BinOp>, table_name: &'static str) {
        for (index, op) in iter.enumerate() {
            assert_eq!(op as usize, index);
        }
    }

    fn no_duplicates(iter: impl Iterator<Item = BinOp> + Clone, table_name: &'static str) {
        for op_to_count in iter.clone() {
            let mut instances = 0;

            for current_op in iter.clone() {
                if current_op == op_to_count {
                    instances += 1
                }
            }

            // Each op should appear exactly once in the table
            assert_eq!(instances, 1, "{op_to_count} appeared {instances} times in {table_name}, but we expected it to appear once at most.");
        }
    }

    #[test]
    fn indices_are_correct_in_precedences() {
        index_is_binop_u8(PRECEDENCES.iter().map(|(op, _)| *op), "PRECEDENCES")
    }

    #[test]
    fn indices_are_correct_in_associativities() {
        index_is_binop_u8(ASSOCIATIVITIES.iter().map(|(op, _)| *op), "ASSOCIATIVITIES")
    }

    #[test]
    fn indices_are_correct_in_display_string() {
        index_is_binop_u8(DISPLAY_STRINGS.iter().map(|(op, _)| *op), "DISPLAY_STRINGS")
    }

    #[test]
    fn no_duplicates_in_precedences() {
        no_duplicates(PRECEDENCES.iter().map(|(op, _)| *op), "PRECEDENCES")
    }

    #[test]
    fn no_duplicates_in_associativities() {
        no_duplicates(ASSOCIATIVITIES.iter().map(|(op, _)| *op), "ASSOCIATIVITIES")
    }

    #[test]
    fn no_duplicates_in_display_strings() {
        no_duplicates(DISPLAY_STRINGS.iter().map(|(op, _)| *op), "DISPLAY_STRINGS")
    }
}
