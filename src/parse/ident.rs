use bumpalo::collections::string::String;
use bumpalo::Bump;
use parse::ast::Attempting;
use parse::parser::{
    unexpected, unexpected_eof, Fail, FailReason, Keyword, ParseResult, Parser, State,
};

#[inline(always)]
pub fn parse_into<'a, I>(
    arena: &'a Bump,
    chars: &mut I,
    state: State<'a>,
) -> ParseResult<'a, (&'a str, Option<char>)>
where
    I: Iterator<Item = char>,
{
    let mut buf = String::new_in(arena);

    // Identifiers must start with an ASCII letter.
    // If this doesn't, it must not be an identifier!
    match chars.next() {
        Some(ch) => {
            if ch.is_ascii_alphabetic() {
                buf.push(ch);
            } else {
                return Err(unexpected(ch, 0, state, Attempting::Identifier));
            }
        }
        None => {
            return Err(unexpected_eof(0, Attempting::Identifier, state));
        }
    }

    let mut next_char = None;

    while let Some(ch) = chars.next() {
        // After the first character, letters, numbers, and '.' are allowed.
        if ch.is_ascii_alphanumeric() {
            buf.push(ch);
        } else if ch == '.' {
            panic!("TODO support qualified identifiers. Make sure we don't have consecutive dots, and that module names are capitalized but post-module nothing is capitalized.");
        } else {
            // This must be the end of the identifier. We're done!
            next_char = Some(ch);

            break;
        }
    }

    let ident_str = buf.as_str();

    // Make sure we aren't trying to use a reserved keyword as an identifier
    match Keyword::from_str(ident_str) {
        Some(keyword) => Err((
            Fail {
                reason: FailReason::UnexpectedKeyword(keyword),
                attempting: Attempting::Identifier,
            },
            state,
        )),
        None => {
            let state = state.advance_without_indenting(buf.len())?;

            Ok(((buf.into_bump_str(), next_char), state))
        }
    }
}

pub fn ident<'a>() -> impl Parser<'a, &'a str> {
    move |arena: &'a Bump, state: State<'a>| {
        // Discard next_char; we don't need it.
        let ((string, _), state) = parse_into(arena, &mut state.input.chars(), state)?;

        Ok((string, state))
    }
}
