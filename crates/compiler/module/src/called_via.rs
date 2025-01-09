use self::Associativity::*;
use self::BinOp::*;
use std::cmp::Ordering;
use std::fmt;

const PRECEDENCES: [(BinOp, u8); 17] = [
    (Caret, 8),
    (Star, 7),
    (Slash, 7),
    (DoubleSlash, 6),
    (Percent, 6),
    (Plus, 5),
    (Minus, 5),
    (DoubleQuestion, 5),
    (Pizza, 4),
    (Equals, 3),
    (NotEquals, 3),
    (LessThan, 2),
    (GreaterThan, 2),
    (LessThanOrEq, 2),
    (GreaterThanOrEq, 2),
    (And, 1),
    (Or, 0),
];

const ASSOCIATIVITIES: [(BinOp, Associativity); 17] = [
    (Caret, RightAssociative),
    (Star, LeftAssociative),
    (Slash, LeftAssociative),
    (DoubleSlash, LeftAssociative),
    (Percent, LeftAssociative),
    (Plus, LeftAssociative),
    (Minus, LeftAssociative),
    (DoubleQuestion, LeftAssociative),
    (Pizza, LeftAssociative),
    (Equals, NonAssociative),
    (NotEquals, NonAssociative),
    (LessThan, NonAssociative),
    (GreaterThan, NonAssociative),
    (LessThanOrEq, NonAssociative),
    (GreaterThanOrEq, NonAssociative),
    (And, RightAssociative),
    (Or, RightAssociative),
];

const DISPLAY_STRINGS: [(BinOp, &str); 17] = [
    (Caret, "^"),
    (Star, "*"),
    (Slash, "/"),
    (DoubleSlash, "//"),
    (Percent, "%"),
    (Plus, "+"),
    (Minus, "-"),
    (DoubleQuestion, "??"),
    (Pizza, "|>"),
    (Equals, "=="),
    (NotEquals, "!="),
    (LessThan, "<"),
    (GreaterThan, ">"),
    (LessThanOrEq, "<="),
    (GreaterThanOrEq, ">="),
    (And, "&&"),
    (Or, "||"),
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
    /// e.g. "$(first) $(last)" is transformed into Str.concat (Str.concat first " ") last.
    StringInterpolation,

    /// This call is the result of desugaring a map2-based Record Builder field. e.g.
    /// ```roc
    /// { Result.parallel <-
    ///     foo: get("a"),
    ///     bar: get("b"),
    /// }
    /// ```
    /// is transformed into
    /// ```roc
    /// Result.parallel(get("a"), get("b"), (\foo, bar -> { foo, bar }))
    /// ```
    RecordBuilder,

    /// This call is the result of desugaring a Result.try from `?` syntax
    /// e.g. Dict.get? items "key" becomes Result.try (Dict.get items "key") \item -> ...
    QuestionSuffix,

    /// This call is a result of lowering a reference to a module-params-extended def
    NakedParamsVar,

    /// This call is the result of desugaring a `try` expression into an early return on Err
    /// e.g. `try parseDate input` becomes:
    ///
    /// ```roc
    /// when parseDate input is
    ///     Err err -> return Err err
    ///     Ok value -> value
    /// ```
    Try,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum UnaryOp {
    /// (-), e.g. (-x)
    Negate,
    /// (!), e.g. (!x)
    Not,
}

impl std::fmt::Display for UnaryOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            UnaryOp::Negate => write!(f, "-"),
            UnaryOp::Not => write!(f, "!"),
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Suffix {
    /// (!), e.g. (Stdin.line!)
    Bang,
    /// (?), e.g. (parseData? data)
    Question,
}

impl std::fmt::Display for Suffix {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Suffix::Bang => write!(f, "!"),
            Suffix::Question => write!(f, "?"),
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum BinOp {
    // highest precedence
    Caret,
    Star,
    Slash,
    DoubleSlash,
    Percent,
    Plus,
    Minus,
    DoubleQuestion,
    Pizza,
    Equals,
    NotEquals,
    LessThan,
    GreaterThan,
    LessThanOrEq,
    GreaterThanOrEq,
    And,
    Or,
    // lowest precedence
}

impl BinOp {
    /// how wide this operator is when typed out
    pub fn width(self) -> u16 {
        match self {
            Caret | Star | Slash | Percent | Plus | Minus | LessThan | GreaterThan => 1,
            DoubleSlash | Equals | NotEquals | LessThanOrEq | GreaterThanOrEq | And | Or
            | Pizza | DoubleQuestion => 2,
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
        const ASSOCIATIVITY_TABLE: [Associativity; 17] = generate_associativity_table();

        ASSOCIATIVITY_TABLE[self as usize]
    }

    fn precedence(self) -> u8 {
        const PRECEDENCE_TABLE: [u8; 17] = generate_precedence_table();

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
        const DISPLAY_TABLE: [&str; 17] = generate_display_table();

        write!(f, "{}", DISPLAY_TABLE[*self as usize])
    }
}

const fn generate_precedence_table() -> [u8; 17] {
    let mut table = [0u8; 17];
    let mut i = 0;

    while i < PRECEDENCES.len() {
        table[(PRECEDENCES[i].0) as usize] = PRECEDENCES[i].1;
        i += 1;
    }

    table
}

const fn generate_associativity_table() -> [Associativity; 17] {
    let mut table = [NonAssociative; 17];
    let mut i = 0;

    while i < ASSOCIATIVITIES.len() {
        table[(ASSOCIATIVITIES[i].0) as usize] = ASSOCIATIVITIES[i].1;
        i += 1;
    }

    table
}

const fn generate_display_table() -> [&'static str; 17] {
    let mut table = [""; 17];
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
            assert_eq!(op as usize, index,  "{op} was found at index {index} in {table_name}, but it should have been at index {} instead.", op as usize);
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
}
