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
pub const RETURN: &str = "return";
pub const CRASH: &str = "crash";
pub const AND: &str = "and";
pub const OR: &str = "or";

// These keywords are valid in imports
pub const EXPOSING: &str = "exposing";

// These keywords are valid in types
pub const IMPLEMENTS: &str = "implements";
pub const WHERE: &str = "where";

// These keywords are valid in headers
pub const PLATFORM: &str = "platform";

pub const KEYWORDS: [&str; 13] = [
    IF, THEN, ELSE, WHEN, AS, IS, DBG, IMPORT, EXPECT, RETURN, CRASH, AND, OR,
];

pub fn is_allowed_identifier(mut ident: &str) -> bool {
    if ident.ends_with('!') {
        ident = &ident[..ident.len() - 1];
    }
    !crate::keyword::KEYWORDS.iter().any(|kw| &ident == kw)
}
