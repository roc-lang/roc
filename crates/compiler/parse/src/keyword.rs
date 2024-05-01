// These keywords are valid in expressions
pub const IF: &str = "if";
pub const THEN: &str = "then";
pub const ELSE: &str = "else";
pub const WHEN: &str = "when";
pub const AS: &str = "as";
pub const IS: &str = "is";
pub const DBG: &str = "dbg";
pub const IMPORT: &str = "import";
pub const EXPECT: &str = "expect";
pub const EXPECT_FX: &str = "expect-fx";
pub const CRASH: &str = "crash";

// These keywords are valid in imports
pub const EXPOSING: &str = "exposing";

// These keywords are valid in types
pub const IMPLEMENTS: &str = "implements";
pub const WHERE: &str = "where";

pub const KEYWORDS: [&str; 11] = [
    IF, THEN, ELSE, WHEN, AS, IS, DBG, IMPORT, EXPECT, EXPECT_FX, CRASH,
];
