#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
/// Tokens are full of very dense information to make checking properties about them
/// very fast.
/// Some bits have specific meanings: 
/// * 0b_001*_****: "Identifier-like" things
/// * 0b_0100_****: "Punctuation"
///     * 0b_0100_1***: []{}()
///         * 0b_0100_1**0 [{(
///         * 0b_0100_1**1 ]})
///     * 0b_011*_**** Operators
pub enum Token {
    Ident               = 0b_0010_0000,
    MalformedIdent      = 0b_0010_0001,

    KeywordIf           = 0b_0010_0010,
    KeywordThen         = 0b_0010_0011,
    KeywordElse         = 0b_0010_0100,
    KeywordWhen         = 0b_0010_0101,
    KeywordAs           = 0b_0010_0110,
    KeywordIs           = 0b_0010_0111,
    KeywordExpect       = 0b_0010_1000,
    KeywordApp          = 0b_0010_1001,
    KeywordInterface    = 0b_0010_1010,
    KeywordPackages     = 0b_0010_1011,
    KeywordImports      = 0b_0010_1100,
    KeywordProvides     = 0b_0010_1101,
    KeywordTo           = 0b_0010_1110,
    KeywordExposes      = 0b_0010_1111,
    KeywordEffects      = 0b_0011_0000,
    KeywordPlatform     = 0b_0011_0001,
    KeywordRequires     = 0b_0011_0010,

    Comma               = 0b_0100_0000,
    Colon               = 0b_0100_0001,

    OpenParen           = 0b_0100_1000,
    CloseParen          = 0b_0100_1001,
    OpenCurly           = 0b_0100_1010,
    CloseCurly          = 0b_0100_1011,
    OpenSquare          = 0b_0100_1100,
    CloseSquare         = 0b_0100_1101,

    OpPlus              = 0b_0110_0000,
    OpMinus             = 0b_0110_0001,
    OpSlash             = 0b_0110_0010,
    OpPercent           = 0b_0110_0011,
    OpCaret             = 0b_0110_0100,
    OpGreaterThan       = 0b_0110_0101,
    OpLessThan          = 0b_0110_0110,
    OpAssignment        = 0b_0110_0111,
    OpPizza             = 0b_0110_1000,
    OpEquals            = 0b_0110_1001,
    OpNotEquals         = 0b_0110_1010,
    OpGreaterThanOrEq   = 0b_0110_1011,
    OpLessThanOrEq      = 0b_0110_1100,
    OpAnd               = 0b_0110_1101,
    OpOr                = 0b_0110_1110,
    OpDoubleSlash       = 0b_0110_1111,
    OpDoublePercent     = 0b_0111_0001,
    OpBackpassing       = 0b_0111_1010,

    TodoNextThing       = 0b_1000_0000,

    Malformed,
    MalformedOperator,

    // PackageName, // TODO: this seems to be a combo of two idents, i.e. "rtfeldman/blah"
    // LowercaseIdent, // TODO: maybe this should just be Ident, then checked afterwards?
    // UppercaseIdent, // TODO: maybe this should just be Ident, then checked afterwards?
    // UnqualifiedIdent, // TODO: maybe this should just be Ident, then checked afterwards?
    // ModuleName, // TODO: maybe this should just be Ident, then checked afterwards?
    // ConcreteType, // TODO: made of two idents separated by a '.'
    PrivateTag,

    String,

    NumberBase,
    Number,

    QuestionMark,

    Underscore,

    Ampersand,
    Pipe,
    Dot,
    Bang,
    LambdaStart,
    Arrow,
    FatArrow,
    Astrisk,
}

pub struct TokenTable {
    pub tokens: Vec<Token>,
    pub offsets: Vec<usize>,
    pub lengths: Vec<usize>,
}

pub struct LexState {
    indents: Vec<usize>,
}

impl Token {
    pub fn lex_single(state: &mut LexState, mut bytes: &[u8]) -> Option<(Token, usize, usize)> {
        let mut skip = 0;
        loop {
            let bytes = &bytes[skip..];
            // println!("at {:?}", std::str::from_utf8(bytes).unwrap());
            if bytes.len() == 0 {
                // println!("return");
                return None;
            }

            let (token, len) = match bytes[0] {
                b'(' => (Token::OpenParen, 1),
                b')' => (Token::CloseParen, 1),
                b'{' => (Token::OpenCurly, 1),
                b'}' => (Token::CloseCurly, 1),
                b'[' => (Token::OpenSquare, 1),
                b']' => (Token::CloseSquare, 1),
                b',' => (Token::Comma, 1),
                b'_' => lex_underscore(bytes),
                b'@' => lex_private_tag(bytes),
                b'a'..=b'z' => lex_ident(false, bytes),
                b'A'..=b'Z' => lex_ident(true, bytes),
                b'0'..=b'9' => lex_number(bytes),
                b'-' | b':' | b'!' | b'.' | b'*' | b'/' | b'&' |
                b'%' | b'^' | b'+' | b'<' | b'=' | b'>' | b'|' | b'\\' => lex_operator(bytes),
                b' ' => {
                    skip += skip_whitespace(bytes);
                    continue;
                }
                b'\n' => {
                    // TODO: add newline to side_table
                    skip += skip_newlines(bytes);
                    continue;
                }
                b'#' => {
                    // TODO: add comment to side_table
                    skip += skip_comment(bytes);
                    continue;
                }
                b'"' => lex_string(bytes),
                b => todo!("handle {:?}", b as char),
            };

            // println!("return");
            return Some((token, skip, len))
        }
    }
}

impl TokenTable {
    pub fn new(text: &str) -> TokenTable {
        let mut tt = TokenTable {
            tokens: Vec::new(),
            offsets: Vec::new(),
            lengths: Vec::new(),
        };

        let mut offset = 0;
        let mut state = LexState::new();

        while let Some((token, skip, length)) = Token::lex_single(&mut state, &text.as_bytes()[offset..]) {
            tt.tokens.push(token);
            offset += skip;
            tt.offsets.push(offset);
            offset += length;
            tt.lengths.push(length);
        }

        tt
    }
}

impl LexState {
    pub fn new() -> LexState {
        LexState {
            indents: Vec::new(),
        }
    }
}

fn skip_comment(bytes: &[u8]) -> usize {
    let mut skip = 0;
    while skip < bytes.len() && bytes[skip] != b'\n' {
        skip += 1;
    }
    if skip < bytes.len() && bytes[skip] == b'\n' {
        skip += 1;
    }
    skip
}

fn skip_whitespace(bytes: &[u8]) -> usize {
    let mut skip = 0;
    while skip < bytes.len() && bytes[skip] == b' ' {
        skip += 1;
    }
    skip
}

fn skip_newlines(bytes: &[u8]) -> usize {
    let mut skip = 0;
    while skip < bytes.len() && (bytes[skip] == b'\n' || bytes[skip] == b' ') {
        skip += 1;
    }
    skip
}

fn is_op_continue(ch: u8) -> bool {
    matches!(ch, b'-' | b':' | b'!' | b'.' | b'*' | b'/' | b'&' |
                b'%' | b'^' | b'+' | b'<' | b'=' | b'>' | b'|' | b'\\')
}

fn lex_operator(bytes: &[u8]) -> (Token, usize) {
    let mut i = 0;
    while i < bytes.len() && is_op_continue(bytes[i]) {
        i += 1;
    }
    let tok = match &bytes[0..i] {
        b"+" => Token::OpPlus,
        b"-" => Token::OpMinus,
        b"*" => Token::Astrisk,
        b"/" => Token::OpSlash,
        b"%" => Token::OpPercent,
        b"^" => Token::OpCaret,
        b">" => Token::OpGreaterThan,
        b"<" => Token::OpLessThan,
        b"." => Token::Dot,
        b"=" => Token::OpAssignment,
        b":" => Token::Colon,
        b"|" => Token::Pipe,
        b"\\" => Token::LambdaStart,
        b"|>" => Token::OpPizza,
        b"==" => Token::OpEquals,
        b"!" => Token::Bang,
        b"!=" => Token::OpNotEquals,
        b">=" => Token::OpGreaterThanOrEq,
        b"<=" => Token::OpLessThanOrEq,
        b"&&" => Token::OpAnd,
        b"&" => Token::Ampersand,
        b"||" => Token::OpOr,
        b"//" => Token::OpDoubleSlash,
        b"%%" => Token::OpDoublePercent,
        b"->" => Token::Arrow,
        b"<-" => Token::OpBackpassing,
        op => {
            dbg!(std::str::from_utf8(op).unwrap());
            Token::MalformedOperator
        }
    };
    (tok, i)
}

fn is_ident_continue(ch: u8) -> bool {
    matches!(ch, b'a'..=b'z'|b'A'..=b'Z'|b'0'..=b'9'|b'_')
}

fn lex_private_tag(bytes: &[u8]) -> (Token, usize) {
    debug_assert!(bytes[0] == b'@');
    let mut i = 1;
    while i < bytes.len() && is_ident_continue(bytes[i]) {
        i += 1;
    }
    (Token::PrivateTag, i)
}

fn lex_ident(uppercase: bool, bytes: &[u8]) -> (Token, usize) {
    let mut i = 0;
    while i < bytes.len() && is_ident_continue(bytes[i]) {
        i += 1;
    }
    let tok = match &bytes[0..i] {
        b"if" => Token::KeywordIf,
        b"then" => Token::KeywordThen,
        b"else" => Token::KeywordElse,
        b"when" => Token::KeywordWhen,
        b"as" => Token::KeywordAs,
        b"is" => Token::KeywordIs,
        b"expect" => Token::KeywordExpect,
        b"app" => Token::KeywordApp,
        b"interface" => Token::KeywordInterface,
        b"packages" => Token::KeywordPackages,
        b"imports" => Token::KeywordImports,
        b"provides" => Token::KeywordProvides,
        b"to" => Token::KeywordTo,
        b"exposes" => Token::KeywordExposes,
        b"effects" => Token::KeywordEffects,
        b"platform" => Token::KeywordPlatform,
        b"requires" => Token::KeywordRequires,
        ident => {
            if ident.contains(&b'_') {
                Token::MalformedIdent
            } else {
                Token::Ident
            }
        },
    };
    (tok, i)
}

fn lex_underscore(bytes: &[u8]) -> (Token, usize) {
    let mut i = 0;
    while i < bytes.len() && is_ident_continue(bytes[i]) {
        i += 1;
    }
    (Token::Underscore, i)
}

fn is_int_continue(ch: u8) -> bool {
    matches!(ch, b'0'..=b'9' | b'_')
}

fn lex_number(bytes: &[u8]) -> (Token, usize) {
    let mut i = 0;
    while i < bytes.len() && is_int_continue(bytes[i]) {
        i += 1;
    }

    if i < bytes.len() && bytes[i] == b'.' {
        i += 1;
        while i < bytes.len() && is_int_continue(bytes[i]) {
            i += 1;
        }
    }

    (Token::Number, i)
}

fn lex_string(bytes: &[u8]) -> (Token, usize) {
    let mut i = 0;
    assert_eq!(bytes[i], b'"');
    i += 1;

    while i < bytes.len() {
        match bytes[i] {
            b'"' => break,
            // TODO: escapes
            _ => i += 1,
        }
    }

    assert_eq!(bytes[i], b'"');
    i += 1;

    (Token::String, i)
}
