use crate::ast::Attempting;
use crate::parser::{parse_utf8, unexpected, unexpected_eof, ParseResult, Parser, State};
use bumpalo::collections::vec::Vec;
use bumpalo::Bump;

pub enum StringLiteral<'a> {
    Line(&'a str),
    Block(&'a [&'a str]),
}

pub fn parse<'a>() -> impl Parser<'a, StringLiteral<'a>> {
    move |arena: &'a Bump, state: State<'a>| {
        let mut bytes = state.bytes.iter();

        // String literals must start with a quote.
        // If this doesn't, it must not be a string literal!
        match bytes.next() {
            Some(&byte) => {
                if byte != b'"' {
                    return Err(unexpected(0, state, Attempting::StringLiteral));
                }
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
        let mut prev_byte = b'"';

        while let Some(&byte) = bytes.next() {
            parsed_chars += 1;

            // Potentially end the string (unless this is an escaped `"`!)
            match byte {
                b'"' if prev_byte != b'\\' => {
                    let (string, state) = if parsed_chars == 2 {
                        match bytes.next() {
                            Some(b'"') => {
                                // If the first three chars were all `"`, then this
                                // literal begins with `"""` and is a block string.
                                return parse_block_string(arena, state, &mut bytes);
                            }
                            _ => ("", state.advance_without_indenting(2)?),
                        }
                    } else {
                        // Start at 1 so we omit the opening `"`.
                        // Subtract 1 from parsed_chars so we omit the closing `"`.
                        let string_bytes = &state.bytes[1..(parsed_chars - 1)];

                        match parse_utf8(string_bytes) {
                            Ok(string) => (string, state.advance_without_indenting(parsed_chars)?),
                            Err(reason) => {
                                return state.fail(reason);
                            }
                        }
                    };

                    return Ok((StringLiteral::Line(string), state));
                }
                b'\n' => {
                    // This is a single-line string, which cannot have newlines!
                    // Treat this as an unclosed string literal, and consume
                    // all remaining chars. This will mask all other errors, but
                    // it should make it easiest to debug; the file will be a giant
                    // error starting from where the open quote appeared.
                    return Err(unexpected(
                        state.bytes.len() - 1,
                        state,
                        Attempting::StringLiteral,
                    ));
                }
                _ => {
                    prev_byte = byte;
                }
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
    bytes: &mut I,
) -> ParseResult<'a, StringLiteral<'a>>
where
    I: Iterator<Item = &'a u8>,
{
    // So far we have consumed the `"""` and that's it.
    let mut parsed_chars = 3;
    let mut prev_byte = b'"';
    let mut quotes_seen = 0;

    // start at 3 to omit the opening `"`.
    let mut line_start = 3;

    let mut lines: Vec<'a, &'a str> = Vec::new_in(arena);

    for byte in bytes {
        parsed_chars += 1;

        // Potentially end the string (unless this is an escaped `"`!)
        match byte {
            b'"' if prev_byte != b'\\' => {
                if quotes_seen == 2 {
                    // three consecutive qoutes, end string

                    // Subtract 3 from parsed_chars so we omit the closing `"`.
                    let line_bytes = &state.bytes[line_start..(parsed_chars - 3)];

                    return match parse_utf8(line_bytes) {
                        Ok(line) => {
                            let state = state.advance_without_indenting(parsed_chars)?;

                            lines.push(line);

                            Ok((StringLiteral::Block(arena.alloc(lines)), state))
                        }
                        Err(reason) => state.fail(reason),
                    };
                }
                quotes_seen += 1;
            }
            b'\n' => {
                // note this includes the newline
                let line_bytes = &state.bytes[line_start..parsed_chars];

                match parse_utf8(line_bytes) {
                    Ok(line) => {
                        lines.push(line);

                        quotes_seen = 0;
                        line_start = parsed_chars;
                    }
                    Err(reason) => {
                        return state.fail(reason);
                    }
                }
            }
            _ => {
                quotes_seen = 0;
            }
        }

        prev_byte = *byte;
    }

    // We ran out of characters before finding 3 closing quotes
    Err(unexpected_eof(
        parsed_chars,
        // TODO custom BlockStringLiteral?
        Attempting::StringLiteral,
        state,
    ))
}
