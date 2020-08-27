use crate::ast::{Attempting, StrLiteral, StrSegment};
use crate::parser::{parse_utf8, unexpected, unexpected_eof, ParseResult, Parser, State};
use bumpalo::collections::vec::Vec;
use bumpalo::Bump;

pub fn parse<'a>() -> impl Parser<'a, StrLiteral<'a>> {
    use StrLiteral::*;

    move |arena: &'a Bump, state: State<'a>| {
        let mut bytes = state.bytes.iter();
        // String literals must start with a quote.
        // If this doesn't, it must not be a string literal!
        match bytes.next() {
            Some(&byte) => {
                if byte != b'"' {
                    return Err(unexpected(0, state, Attempting::StrLiteral));
                }
            }
            None => {
                return Err(unexpected_eof(0, Attempting::StrLiteral, state));
            }
        }

        // The current segment begins right after the opening quotation mark.
        let mut cur_segment = &state.bytes[1..];

        enum EscapeState {
            None,
            Unicode,
            Interpolation,
        }

        // At the parsing stage we keep the entire raw string, because the formatter
        // needs the raw string. (For example, so it can "remember" whether you
        // wrote \u{...} or the actual unicode character itself.)
        //
        // Since we're keeping the entire raw string, all we need to track is
        // how many characters we've parsed. So far, that's 1 (the opening `"`).
        let mut total_parsed_chars = 1;
        let mut segment_parsed_chars = 0;
        let mut segments = Vec::new_in(arena);
        let mut escape_state = EscapeState::None;

        // pub enum StrSegment<'a> {
        //     Plaintext(&'a str),    // e.g. "foo"
        //     Unicode(&'a str),      // e.g. "00A0" in "\u(00A0)"
        //     Interpolated(&'a str), // e.g. "name" in "Hi, \(name)!"
        //     EscapedChar(char),     // e.g. '\n' in "Hello!\n"
        // }

        while let Some(&byte) = bytes.next() {
            segment_parsed_chars += 1;

            // Potentially end the string (unless this is an escaped `"`!)
            match byte {
                b'"' => {
                    // If we aren't escaping, then this is the end of the string!
                    if let EscapeState::None = escape_state {
                        let (literal, state) = if total_parsed_chars == 1 && segments.is_empty() {
                            match bytes.next() {
                                Some(b'"') => {
                                    // If the very first three chars were all `"`,
                                    // then this literal begins with `"""`
                                    // and is a block string.
                                    return parse_block_string(arena, state, &mut bytes);
                                }
                                _ => (PlainLine(""), state.advance_without_indenting(2)?),
                            }
                        } else {
                            // Subtract 1 from parsed_chars so we omit the closing `"`.
                            let string_bytes = &cur_segment[0..(segment_parsed_chars - 1)];

                            match parse_utf8(string_bytes) {
                                Ok(string) => {
                                    total_parsed_chars += segment_parsed_chars;

                                    let state =
                                        state.advance_without_indenting(total_parsed_chars)?;

                                    if segments.is_empty() {
                                        // We only had one segment.
                                        (StrLiteral::PlainLine(string), state)
                                    } else {
                                        // We had multiple segments! Parse the
                                        // current one and add it to the list.
                                        segments.push(StrSegment::Plaintext(string));

                                        (LineWithEscapes(segments.into_bump_slice()), state)
                                    }
                                }
                                Err(reason) => {
                                    return state.fail(reason);
                                }
                            }
                        };

                        return Ok((literal, state));
                    } else {
                        // We are escaping, so this is an error. (If it were an
                        // escaped single character like \" then we would have
                        // handled that scenario already.)
                        return Err(unexpected(
                            state.bytes.len() - 1,
                            state,
                            Attempting::StrLiteral,
                        ));
                    }
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
                        Attempting::StrLiteral,
                    ));
                }
                b')' => {
                    // All escape sequences end in a close paren, so we don't
                    // need to pay for a conditional here. If it was an escape,
                    // then we want to set it to None, and if it wasn't an
                    // escape, then setting it from None to None is harmless!
                    // (And likely cheaper than a conditional.)
                    escape_state = EscapeState::None;
                }
                b'\\' => {
                    // This is the start of a new escape
                    if let EscapeState::None = escape_state {
                        match bytes.next() {
                            Some(b'(') => {
                                // This is an interpolated variable
                                escape_state = EscapeState::Interpolation;
                                todo!("Parse interpolated ident");
                            }
                            Some(b'u') => {
                                escape_state = EscapeState::Unicode;
                                // This is an escaped unicode character
                                todo!("Parse '(' and then parse escaped unicode character");
                            }
                            Some(ch @ b'\n') | Some(ch @ b'\t') | Some(ch @ b'\r')
                            | Some(ch @ b'"') | Some(ch @ b'\\') => {
                                // Record the current segment so we can begin a new one.
                                match parse_utf8(cur_segment) {
                                    Ok(string) => {
                                        segments.push(StrSegment::Plaintext(string));
                                    }
                                    Err(reason) => {
                                        return state.fail(reason);
                                    }
                                }

                                // Record the escaped char.
                                segments.push(StrSegment::EscapedChar(*ch as char));

                                // We're now done escaping.
                                escape_state = EscapeState::None;

                                // Advance past the segment we just added, and
                                // also past the escaped char we just added.
                                //
                                // +2 because we just parsed a backslash and
                                // one other char after it.
                                cur_segment = &cur_segment[(segment_parsed_chars + 2)..];

                                // Reset segment_parsed_chars to 0 because we're now
                                // parsing the beginning of a new segment.
                                segment_parsed_chars = 0;
                            }
                            _ => {
                                // Invalid escape! A backslash must be followed
                                // by either an open paren or else one of the
                                // escapable characters (\n, \t, \", \\, etc)
                                return Err(unexpected(
                                    state.bytes.len() - 1,
                                    state,
                                    Attempting::StrLiteral,
                                ));
                            }
                        }
                    } else {
                        // Can't have a \ inside an escape!
                        return Err(unexpected(
                            state.bytes.len() - 1,
                            state,
                            Attempting::StrLiteral,
                        ));
                    }
                }
                _ => {
                    // All other characters need no special handling.
                }
            }
        }

        // We ran out of characters before finding a closed quote
        Err(unexpected_eof(
            total_parsed_chars,
            Attempting::StrLiteral,
            state.clone(),
        ))
    }
}

fn parse_block_string<'a, I>(
    arena: &'a Bump,
    state: State<'a>,
    bytes: &mut I,
) -> ParseResult<'a, StrLiteral<'a>>
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
                        Ok(_line) => {
                            // let state = state.advance_without_indenting(parsed_chars)?;

                            // lines.push(line);

                            // Ok((StrLiteral::Block(lines.into_bump_slice()), state))
                            todo!("TODO finish making block strings accept escapes");
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
        // TODO custom BlockStrLiteral?
        Attempting::StrLiteral,
        state,
    ))
}
