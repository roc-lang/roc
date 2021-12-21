#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Token {
    Malformed,
    MalformedIdent,
    MalformedOperator,

    BinOpPlus,
    Minus,
    BinOpStar,
    BinOpSlash,
    BinOpPercent,
    BinOpCaret,
    BinOpGreaterThan,
    BinOpLessThan,
    BinOpAssignment,
    BinOpPizza,
    BinOpEquals,
    BinOpNotEquals,
    BinOpGreaterThanOrEq,
    BinOpLessThanOrEq,
    BinOpAnd,
    BinOpOr,
    BinOpDoubleSlash,
    BinOpDoublePercent,
    BinOpBackpassing,

    KeywordIf,
    KeywordThen,
    KeywordElse,
    KeywordWhen,
    KeywordAs,
    KeywordIs,
    KeywordExpect,
    KeywordApp,
    KeywordInterface,
    KeywordPackages,
    KeywordImports,
    KeywordProvides,
    KeywordTo,
    KeywordExposes,
    KeywordEffects,
    KeywordPlatform,

    Ident,

    // PackageName, // TODO: this seems to be a combo of two idents, i.e. "rtfeldman/blah"
    // LowercaseIdent, // TODO: maybe this should just be Ident, then checked afterwards?
    // UppercaseIdent, // TODO: maybe this should just be Ident, then checked afterwards?
    // UnqualifiedIdent, // TODO: maybe this should just be Ident, then checked afterwards?
    // ModuleName, // TODO: maybe this should just be Ident, then checked afterwards?
    // ConcreteType, // TODO: made of two idents separated by a '.'
    PrivateTag,

    NumberBase,
    Number,

    Comma,
    Colon,

    OpenParen,
    CloseParen,
    OpenCurly,
    CloseCurly,
    OpenSquare,
    CloseSquare,

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

impl Token {
    pub fn lex_single(expected: Token, mut bytes: &[u8]) -> Option<(Token, usize)> {
        loop {
            if bytes.len() == 0 {
                return None;
            }

            let res = match bytes[0] {
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
                    bytes = skip_whitespace(bytes);
                    continue;
                }
                b'\n' => {
                    // TODO: add newline to side_table
                    bytes = skip_newlines(bytes);
                    continue;
                }
                b'#' => {
                    // TODO: add comment to side_table
                    bytes = skip_comment(bytes);
                    continue;
                }
                b => todo!("handle {:?}; expected {:?}", b as char, expected),
            };

            return Some(res)
        }
    }
}

fn skip_comment(mut bytes: &[u8]) -> &[u8] {
    while bytes.len() > 0 && bytes[0] != b'\n' {
        bytes = &bytes[1..];
    }
    if bytes.len() > 0 && bytes[0] == b'\n' {
        bytes = &bytes[1..];
    }
    bytes
}

fn skip_whitespace(mut bytes: &[u8]) -> &[u8] {
    while bytes.len() > 0 && bytes[0] == b' ' {
        bytes = &bytes[1..];
    }
    bytes
}

fn skip_newlines(mut bytes: &[u8]) -> &[u8] {
    while bytes.len() > 0 && (bytes[0] == b'\n' || bytes[0] == b' ') {
        bytes = &bytes[1..];
    }
    bytes
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
        b"+" => Token::BinOpPlus,
        b"-" => Token::Minus,
        b"*" => Token::BinOpStar,
        b"/" => Token::BinOpSlash,
        b"%" => Token::BinOpPercent,
        b"^" => Token::BinOpCaret,
        b">" => Token::BinOpGreaterThan,
        b"<" => Token::BinOpLessThan,
        b"." => Token::Dot,
        b"=" => Token::BinOpAssignment,
        b":" => Token::Colon,
        b"|" => Token::Pipe,
        b"\\" => Token::LambdaStart,
        b"|>" => Token::BinOpPizza,
        b"==" => Token::BinOpEquals,
        b"!" => Token::Bang,
        b"!=" => Token::BinOpNotEquals,
        b">=" => Token::BinOpGreaterThanOrEq,
        b"<=" => Token::BinOpLessThanOrEq,
        b"&&" => Token::BinOpAnd,
        b"&" => Token::Ampersand,
        b"||" => Token::BinOpOr,
        b"//" => Token::BinOpDoubleSlash,
        b"%%" => Token::BinOpDoublePercent,
        b"->" => Token::Arrow,
        b"<-" => Token::BinOpBackpassing,
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
