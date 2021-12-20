#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Token {
    Malformed,
    MalformedIdent,

    CommentOrNewline,
    UnaryNegate,

    BinOpPlus,
    BinOpMinus,
    BinOpStar,
    BinOpSlash,
    BinOpPercent,
    BinOpCaret,
    BinOpGreaterThan,
    BinOpLessThan,
    BinOpAssignment,
    BinOpHasType,
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

    Ident,

    PackageName, // TODO: this seems to be a combo of two idents, i.e. "rtfeldman/blah"
    LowercaseIdent, // TODO: maybe this should just be Ident, then checked afterwards?
    UppercaseIdent, // TODO: maybe this should just be Ident, then checked afterwards?
    UnqualifiedIdent, // TODO: maybe this should just be Ident, then checked afterwards?
    ModuleName, // TODO: maybe this should just be Ident, then checked afterwards?
    ConcreteType, // TODO: made of two idents separated by a '.'
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
                b'a'..=b'z' => lex_ident(true, bytes),
                b'A'..=b'Z' => lex_ident(false, bytes),
                b'0'..=b'9' => lex_int(bytes),
                b' ' => {
                    bytes = skip_whitespace(bytes);
                    continue;
                }
                b => todo!("handle {:?}; expected {:?}", b as char, expected),
            };

            return Some(res)
        }
    }
}

fn skip_whitespace(mut bytes: &[u8]) -> &[u8] {
    while bytes.len() > 0 && bytes[0] == b' ' {
        bytes = &bytes[1..];
    }
    bytes
}

fn is_ident_continue(ch: u8) -> bool {
    matches!(ch, b'a'..=b'z'|b'A'..=b'Z'|b'0'..=b'9'|b'_')
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
        _ => Token::Ident,
    };
    (tok, i)
}

fn is_int_continue(ch: u8) -> bool {
    matches!(ch, b'0'..=b'9')
}

fn lex_int(bytes: &[u8]) -> (Token, usize) {
    let mut i = 0;
    while i < bytes.len() && is_int_continue(bytes[i]) {
        i += 1;
    }
    (Token::Number, i)
}
