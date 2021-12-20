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
    pub fn lex_single(expected: Token, bytes: &[u8]) -> Option<(Token, usize)> {
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
            b => todo!("handle {:?}; expected {:?}", b as char, expected),
        };

        Some(res)
    }
}
