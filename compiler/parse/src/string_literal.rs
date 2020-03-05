use crate::ast::Attempting;
use crate::parser::{unexpected, unexpected_eof, ParseResult, Parser, State};
use bumpalo::collections::vec::Vec;
use bumpalo::Bump;
use std::char;

pub enum StringLiteral<'a> {
    Line(&'a str),
    Block(&'a [&'a str]),
}

pub fn parse<'a>() -> impl Parser<'a, StringLiteral<'a>> {
    move |arena: &'a Bump, state: State<'a>| {
        let mut chars = state.input.chars();

        // String literals must start with a quote.
        // If this doesn't, it must not be a string literal!
        match chars.next() {
            Some('"') => (),
            Some(other_char) => {
                return Err(unexpected(other_char, 0, state, Attempting::StringLiteral));
            }
            None => {
                return Err(unexpected_eof(0, Attempting::StringLiteral, state));
            }
        }

        // At the parsing stage we keep the entire raw string, because the formatter
        // needs the raw string. (For example, so it can "remember" whether you
        // wrote \u{...} or the actual unicode character itself.)
        //
        // Later, in canonicalization, we'll do things like resolving
        // unicode escapes and string interpolation.
        //
        // Since we're keeping the entire raw string, all we need to track is
        // how many characters we've parsed. So far, that's 1 (the opening `"`).
        let mut parsed_chars = 1;
        let mut prev_ch = '"';

        while let Some(ch) = chars.next() {
            parsed_chars += 1;

            // Potentially end the string (unless this is an escaped `"`!)
            if ch == '"' && prev_ch != '\\' {
                let string = if parsed_chars == 2 {
                    if let Some('"') = chars.next() {
                        // If the first three chars were all `"`, then this
                        // literal begins with `"""` and is a block string.
                        return parse_block_string(arena, state, &mut chars);
                    } else {
                        ""
                    }
                } else {
                    // Start at 1 so we omit the opening `"`.
                    // Subtract 1 from parsed_chars so we omit the closing `"`.
                    &state.input[1..(parsed_chars - 1)]
                };

                let next_state = state.advance_without_indenting(parsed_chars)?;

                return Ok((StringLiteral::Line(string), next_state));
            } else if ch == '\n' {
                // This is a single-line string, which cannot have newlines!
                // Treat this as an unclosed string literal, and consume
                // all remaining chars. This will mask all other errors, but
                // it should make it easiest to debug; the file will be a giant
                // error starting from where the open quote appeared.
                return Err(unexpected(
                    '\n',
                    state.input.len() - 1,
                    state,
                    Attempting::StringLiteral,
                ));
            } else {
                prev_ch = ch;
            }
        }

        // We ran out of characters before finding a closed quote
        Err(unexpected_eof(
            parsed_chars,
            Attempting::StringLiteral,
            state.clone(),
        ))
    }
}

fn parse_block_string<'a, I>(
    arena: &'a Bump,
    state: State<'a>,
    chars: &mut I,
) -> ParseResult<'a, StringLiteral<'a>>
where
    I: Iterator<Item = char>,
{
    // So far we have consumed the `"""` and that's it.
    let mut parsed_chars = 3;
    let mut prev_ch = '"';
    let mut quotes_seen = 0;

    // start at 3 to omit the opening `"`.
    let mut line_start = 3;

    let mut lines = Vec::new_in(arena);

    for ch in chars {
        parsed_chars += 1;

        // Potentially end the string (unless this is an escaped `"`!)
        if ch == '"' && prev_ch != '\\' {
            if quotes_seen == 2 {
                // three consecutive qoutes, end string

                // Subtract 3 from parsed_chars so we omit the closing `"`.
                let string = &state.input[line_start..(parsed_chars - 3)];
                lines.push(string);

                let next_state = state.advance_without_indenting(parsed_chars)?;

                return Ok((StringLiteral::Block(arena.alloc(lines)), next_state));
            }
            quotes_seen += 1;
        } else if ch == '\n' {
            // note this includes the newline
            let string = &state.input[line_start..parsed_chars];
            lines.push(string);
            quotes_seen = 0;
            line_start = parsed_chars;
        } else {
            quotes_seen = 0;
        }
        prev_ch = ch;
    }

    // We ran out of characters before finding 3 closing quotes
    Err(unexpected_eof(
        parsed_chars,
        // TODO custom BlockStringLiteral?
        Attempting::StringLiteral,
        state.clone(),
    ))
}
