use std::cmp::Ordering;

#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
/// Tokens are full of very dense information to make checking properties about them
/// very fast.
/// Some bits have specific meanings:
/// * 0b_001*_****: "Identifier-like" things
/// * 0b_01**_****: "Punctuation"
///     * 0b_0100_1***: []{}() INDENT/DEDENT
///         * 0b_0100_1**0 [{(INDENT
///         * 0b_0100_1**1 ]})DEDENT
///     * 0b_011*_**** Operators
pub enum Token {
    LowercaseIdent = 0b_0010_0000,
    UppercaseIdent = 0b_0011_0011,
    MalformedIdent = 0b_0010_0001,

    KeywordIf = 0b_0010_0010,
    KeywordThen = 0b_0010_0011,
    KeywordElse = 0b_0010_0100,
    KeywordWhen = 0b_0010_0101,
    KeywordAs = 0b_0010_0110,
    KeywordIs = 0b_0010_0111,
    KeywordExpect = 0b_0010_1000,
    KeywordApp = 0b_0010_1001,
    KeywordInterface = 0b_0010_1010,
    KeywordPackages = 0b_0010_1011,
    KeywordImports = 0b_0010_1100,
    KeywordProvides = 0b_0010_1101,
    KeywordTo = 0b_0010_1110,
    KeywordExposes = 0b_0010_1111,
    KeywordEffects = 0b_0011_0000,
    KeywordPackage = 0b_0111_1100,
    KeywordPlatform = 0b_0011_0001,
    KeywordRequires = 0b_0011_0010,
    KeywordDbg = 0b_0111_1011,

    Comma = 0b_0100_0000,
    Colon = 0b_0100_0001,

    OpenParen = 0b_0100_1000,
    CloseParen = 0b_0100_1001,
    OpenCurly = 0b_0100_1010,
    CloseCurly = 0b_0100_1011,
    OpenSquare = 0b_0100_1100,
    CloseSquare = 0b_0100_1101,
    OpenIndent = 0b_0100_1110,
    CloseIndent = 0b_0100_1111,
    SameIndent = 0b_0101_0000,

    OpPlus = 0b_0110_0000,
    OpMinus = 0b_0110_0001,
    OpSlash = 0b_0110_0010,
    OpPercent = 0b_0110_0011,
    OpCaret = 0b_0110_0100,
    OpGreaterThan = 0b_0110_0101,
    OpLessThan = 0b_0110_0110,
    OpAssignment = 0b_0110_0111,
    OpPizza = 0b_0110_1000,
    OpEquals = 0b_0110_1001,
    OpNotEquals = 0b_0110_1010,
    OpGreaterThanOrEq = 0b_0110_1011,
    OpLessThanOrEq = 0b_0110_1100,
    OpAnd = 0b_0110_1101,
    OpOr = 0b_0110_1110,
    OpDoubleSlash = 0b_0110_1111,
    OpBackpassing = 0b_0111_1010,

    TodoNextThing = 0b_1000_0000,

    Malformed,
    MalformedOperator,

    String,

    NumberBase,
    Number,

    QuestionMark,

    Underscore,

    Ampersand,
    Pipe,
    Dot,
    SpaceDot, // ` .` necessary to know difference between `Result.map .position` and `Result.map.position`
    Bang,
    LambdaStart,
    Arrow,
    FatArrow,
    Asterisk,
}

#[derive(Default)]
pub struct TokenTable {
    pub tokens: Vec<Token>,
    pub offsets: Vec<usize>,
    pub lengths: Vec<usize>,
}

#[derive(Default)]
pub struct LexState {
    indents: Vec<usize>,
}

trait ConsumeToken {
    fn token(&mut self, token: Token, _offset: usize, _length: usize);
}

#[derive(Default)]
struct TokenConsumer {
    token_table: TokenTable,
}

impl ConsumeToken for TokenConsumer {
    fn token(&mut self, token: Token, offset: usize, length: usize) {
        self.token_table.tokens.push(token);
        self.token_table.offsets.push(offset);
        self.token_table.lengths.push(length);
    }
}

pub fn tokenize(code_str: &str) -> Vec<Token> {
    full_tokenize(code_str).tokens
}

pub fn full_tokenize(code_str: &str) -> TokenTable {
    let mut lex_state = LexState::default();
    let mut consumer = TokenConsumer::default();

    consume_all_tokens(&mut lex_state, code_str.as_bytes(), &mut consumer);

    consumer.token_table
}

fn consume_all_tokens(state: &mut LexState, bytes: &[u8], consumer: &mut impl ConsumeToken) {
    let mut i = 0;

    while i < bytes.len() {
        let bytes = &bytes[i..];

        let (token, len) = match bytes[0] {
            b'(' => (Token::OpenParen, 1),
            b')' => (Token::CloseParen, 1),
            b'{' => (Token::OpenCurly, 1),
            b'}' => (Token::CloseCurly, 1),
            b'[' => (Token::OpenSquare, 1),
            b']' => (Token::CloseSquare, 1),
            b',' => (Token::Comma, 1),
            b'_' => lex_underscore(bytes),
            b'a'..=b'z' => lex_ident(false, bytes),
            b'A'..=b'Z' => lex_ident(true, bytes),
            b'0'..=b'9' => lex_number(bytes),
            b'-' | b':' | b'!' | b'.' | b'*' | b'/' | b'&' | b'%' | b'^' | b'+' | b'<' | b'='
            | b'>' | b'|' | b'\\' => lex_operator(bytes),
            b' ' => match skip_whitespace(bytes) {
                SpaceDotOrSpaces::SpacesWSpaceDot(skip) => {
                    i += skip;
                    (Token::SpaceDot, 1)
                }
                SpaceDotOrSpaces::Spaces(skip) => {
                    i += skip;
                    continue;
                }
            },
            b'\n' => {
                // TODO: add newline to side_table
                let skip_newline_return = skip_newlines_and_comments(bytes);

                match skip_newline_return {
                    SkipNewlineReturn::SkipWIndent(skipped_lines, curr_line_indent) => {
                        add_indents(skipped_lines, curr_line_indent, state, consumer, &mut i);
                        continue;
                    }
                    SkipNewlineReturn::WSpaceDot(skipped_lines, curr_line_indent) => {
                        add_indents(skipped_lines, curr_line_indent, state, consumer, &mut i);
                        (Token::SpaceDot, 1)
                    }
                }
            }
            b'#' => {
                // TODO: add comment to side_table
                i += skip_comment(bytes);
                continue;
            }
            b'"' => lex_string(bytes),
            b => todo!("handle {:?}", b as char),
        };

        consumer.token(token, i, len);
        i += len;
    }
}

fn add_indents(
    skipped_lines: usize,
    curr_line_indent: usize,
    state: &mut LexState,
    consumer: &mut impl ConsumeToken,
    curr_byte_ctr: &mut usize,
) {
    *curr_byte_ctr += skipped_lines;

    if let Some(&prev_indent) = state.indents.last() {
        if curr_line_indent > prev_indent {
            state.indents.push(curr_line_indent);
            consumer.token(Token::OpenIndent, *curr_byte_ctr, 0);
        } else {
            *curr_byte_ctr += curr_line_indent;

            match prev_indent.cmp(&curr_line_indent) {
                Ordering::Equal => {
                    consumer.token(Token::SameIndent, *curr_byte_ctr, 0);
                }
                Ordering::Greater => {
                    while state.indents.last().is_some()
                        && curr_line_indent < *state.indents.last().unwrap()
                    // safe unwrap because we check first
                    {
                        state.indents.pop();
                        consumer.token(Token::CloseIndent, *curr_byte_ctr, 0);
                    }
                }
                Ordering::Less => {}
            }
        }
    } else if curr_line_indent > 0 {
        state.indents.push(curr_line_indent);
        consumer.token(Token::OpenIndent, *curr_byte_ctr, 0);
    } else {
        consumer.token(Token::SameIndent, *curr_byte_ctr, 0);
    }
}

impl TokenTable {
    pub fn extract_str<'a>(&self, index: usize, content: &'a str) -> &'a str {
        // Not returning a result here because weaving it through highlight_parser makes it more difficult to expand and understand.
        // The only way I think this can panic is by calling position! in highlight_parser after the last element, which does not make sense to begin with.
        let len = *self.lengths.get(index).unwrap_or_else(|| {
            panic!(
                "Index {:?} was out of bounds for TokenTable.lengths with len {:?}",
                index,
                self.lengths.len()
            )
        });
        let offset = *self.offsets.get(index).unwrap_or_else(|| {
            panic!(
                "Index {:?} was out of bounds for TokenTable.offsets with len {:?}",
                index,
                self.lengths.len()
            )
        });

        &content[offset..(offset + len)]
    }
}

fn skip_comment(bytes: &[u8]) -> usize {
    let mut skip = 0;
    while skip < bytes.len() && bytes[skip] != b'\n' {
        skip += 1;
    }
    if (skip + 1) < bytes.len() && bytes[skip] == b'\n' && bytes[skip + 1] == b'#' {
        skip += 1;
    }

    skip
}

#[derive(Copy, Clone, Eq, PartialEq, PartialOrd, Ord)]
struct Indent(usize);

enum SpaceDotOrSpaces {
    SpacesWSpaceDot(usize),
    Spaces(usize),
}

fn skip_whitespace(bytes: &[u8]) -> SpaceDotOrSpaces {
    debug_assert!(bytes[0] == b' ');

    let mut skip = 0;
    while skip < bytes.len() && bytes[skip] == b' ' {
        skip += 1;
    }

    if skip < bytes.len() && bytes[skip] == b'.' {
        SpaceDotOrSpaces::SpacesWSpaceDot(skip)
    } else {
        SpaceDotOrSpaces::Spaces(skip)
    }
}

enum SkipNewlineReturn {
    SkipWIndent(usize, usize),
    WSpaceDot(usize, usize),
}

// also skips lines that contain only whitespace
fn skip_newlines_and_comments(bytes: &[u8]) -> SkipNewlineReturn {
    let mut skip = 0;
    let mut indent = 0;

    while skip < bytes.len() && bytes[skip] == b'\n' {
        skip += indent + 1;

        if bytes.len() > skip {
            if bytes[skip] == b' ' {
                let space_dot_or_spaces = skip_whitespace(&bytes[skip..]);

                match space_dot_or_spaces {
                    SpaceDotOrSpaces::SpacesWSpaceDot(spaces) => {
                        return SkipNewlineReturn::WSpaceDot(skip, spaces)
                    }
                    SpaceDotOrSpaces::Spaces(spaces) => {
                        if bytes.len() > (skip + spaces) {
                            if bytes[skip + spaces] == b'\n' {
                                indent = 0;
                                skip += spaces;
                            } else if bytes[skip + spaces] == b'#' {
                                let comment_skip = skip_comment(&bytes[(skip + spaces)..]);

                                indent = 0;
                                skip += spaces + comment_skip;
                            } else {
                                indent = spaces;
                            }
                        } else {
                            indent = spaces;
                        }
                    }
                }
            } else {
                while bytes[skip] == b'#' {
                    let comment_skip = skip_comment(&bytes[skip..]);

                    indent = 0;
                    skip += comment_skip;
                }
            }
        }
    }

    SkipNewlineReturn::SkipWIndent(skip, indent)
}

fn is_op_continue(ch: u8) -> bool {
    matches!(
        ch,
        b'-' | b':'
            | b'!'
            | b'.'
            | b'*'
            | b'/'
            | b'&'
            | b'%'
            | b'^'
            | b'+'
            | b'<'
            | b'='
            | b'>'
            | b'|'
            | b'\\'
    )
}

fn lex_operator(bytes: &[u8]) -> (Token, usize) {
    let mut i = 0;
    while i < bytes.len() && is_op_continue(bytes[i]) {
        i += 1;
    }
    let tok = match &bytes[0..i] {
        b"+" => Token::OpPlus,
        b"-" => Token::OpMinus,
        b"*" => Token::Asterisk,
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
        b"dbg" => Token::KeywordDbg,
        b"expect" => Token::KeywordExpect,
        b"app" => Token::KeywordApp,
        b"interface" => Token::KeywordInterface,
        b"packages" => Token::KeywordPackages,
        b"imports" => Token::KeywordImports,
        b"provides" => Token::KeywordProvides,
        b"to" => Token::KeywordTo,
        b"exposes" => Token::KeywordExposes,
        b"effects" => Token::KeywordEffects,
        b"package" => Token::KeywordPackage,
        b"platform" => Token::KeywordPlatform,
        b"requires" => Token::KeywordRequires,
        ident => {
            if ident.contains(&b'_') {
                Token::MalformedIdent
            } else if uppercase {
                Token::UppercaseIdent
            } else {
                Token::LowercaseIdent
            }
        }
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

#[cfg(test)]
mod test_tokenizer {
    use super::Token;
    use crate::tokenizer::tokenize;

    type T = Token;

    #[test]
    fn test_indent_tokenization_1() {
        let tokens = tokenize(
            r#"showBool = \b ->
        when b is
            True ->
                "True""#,
        );

        assert_eq!(
            tokens,
            [
                T::LowercaseIdent,
                T::OpAssignment,
                T::LambdaStart,
                T::LowercaseIdent,
                T::Arrow,
                T::OpenIndent,
                T::KeywordWhen,
                T::LowercaseIdent,
                T::KeywordIs,
                T::OpenIndent,
                T::UppercaseIdent,
                T::Arrow,
                T::OpenIndent,
                T::String
            ]
        );
    }

    #[test]
    fn test_indent_tokenization_2() {
        let tokens = tokenize(
            r#"showBool = \b ->
        when b is
            True ->
                "True"
    "#,
        );

        assert_eq!(
            tokens,
            [
                T::LowercaseIdent,
                T::OpAssignment,
                T::LambdaStart,
                T::LowercaseIdent,
                T::Arrow,
                T::OpenIndent,
                T::KeywordWhen,
                T::LowercaseIdent,
                T::KeywordIs,
                T::OpenIndent,
                T::UppercaseIdent,
                T::Arrow,
                T::OpenIndent,
                T::String,
                T::CloseIndent,
                T::CloseIndent,
                T::CloseIndent
            ]
        );
    }

    #[test]
    fn test_tokenization_line_with_only_spaces() {
        let tokens = tokenize(
            r#"\key ->
    when dict is
        Empty ->
            4

        Node ->
            5"#,
        );

        assert_eq!(
            tokens,
            [
                T::LambdaStart,
                T::LowercaseIdent,
                T::Arrow,
                T::OpenIndent,
                T::KeywordWhen,
                T::LowercaseIdent,
                T::KeywordIs,
                T::OpenIndent,
                T::UppercaseIdent,
                T::Arrow,
                T::OpenIndent,
                T::Number,
                T::CloseIndent,
                T::UppercaseIdent,
                T::Arrow,
                T::OpenIndent,
                T::Number
            ]
        );
    }

    #[test]
    fn test_tokenization_empty_lines_and_comments() {
        let tokens = tokenize(
            r#"a = 5

# com1
# com2
b = 6"#,
        );

        assert_eq!(
            tokens,
            [
                T::LowercaseIdent,
                T::OpAssignment,
                T::Number,
                T::SameIndent,
                T::LowercaseIdent,
                T::OpAssignment,
                T::Number
            ]
        );
    }

    #[test]
    fn test_tokenization_when_branch_comments() {
        let tokens = tokenize(
            r#"when errorCode is
    # A -> Task.fail InvalidCharacter
    # B -> Task.fail IOError
    _ ->
        Task.succeed -1"#,
        );

        assert_eq!(
            tokens,
            [
                T::KeywordWhen,
                T::LowercaseIdent,
                T::KeywordIs,
                T::OpenIndent,
                T::Underscore,
                T::Arrow,
                T::OpenIndent,
                T::UppercaseIdent,
                T::Dot,
                T::LowercaseIdent,
                T::OpMinus,
                T::Number
            ]
        );
    }
}
