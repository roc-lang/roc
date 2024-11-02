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
pub const RETURN: &str = "return";
pub const CRASH: &str = "crash";

// These keywords are valid in imports
pub const EXPOSING: &str = "exposing";

// These keywords are valid in types
pub const IMPLEMENTS: &str = "implements";
pub const WHERE: &str = "where";

// These keywords are valid in headers
pub const PLATFORM: &str = "platform";

pub const KEYWORDS: [&str; 12] = [
    IF, THEN, ELSE, WHEN, AS, IS, DBG, IMPORT, EXPECT, EXPECT_FX, RETURN, CRASH,
];

pub const KEYWORD_MIN_LEN: usize = IF.len();
pub const KEYWORD_MAX_LEN: usize = EXPECT_FX.len();

// todo: @perf can we do better, fixed match table or ideal hash map, benchmark it?
#[inline(always)]
pub(crate) fn is_keyword(ident: &str) -> bool {
    ident.len() >= KEYWORD_MIN_LEN
        && ident.len() <= KEYWORD_MAX_LEN
        && crate::keyword::KEYWORDS.iter().any(|kw| &ident == kw)
}
