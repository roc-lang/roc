use crate::ast::{Attempting, StrLiteral, StrSegment};
use crate::expr;
use crate::parser::{
    allocated, ascii_char, loc, parse_utf8, unexpected, unexpected_eof, ParseResult, Parser, State,
};
use bumpalo::collections::vec::Vec;
use bumpalo::Bump;
use roc_region::all::{Located, Region};

pub fn parse<'a>() -> impl Parser<'a, StrLiteral<'a>> {
    use StrLiteral::*;

    move |arena: &'a Bump, mut state: State<'a>| {
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

        // Advance past the opening quotation mark.
        state = state.advance_without_indenting(1)?;

        // At the parsing stage we keep the entire raw string, because the formatter
        // needs the raw string. (For example, so it can "remember" whether you
        // wrote \u{...} or the actual unicode character itself.)
        //
        // Since we're keeping the entire raw string, all we need to track is
        // how many characters we've parsed. So far, that's 1 (the opening `"`).
        let mut segment_parsed_bytes = 0;
        let mut segments = Vec::new_in(arena);

        macro_rules! escaped_char {
            ($ch:expr) => {
                // Record the escaped char.
                segments.push(StrSegment::EscapedChar($ch));

                // Advance past the segment we just added
                state = state.advance_without_indenting(segment_parsed_bytes)?;

                // Reset the segment
                segment_parsed_bytes = 0;
            };
        }

        macro_rules! end_segment {
            ($transform:expr) => {
                // Don't push anything if the string would be empty.
                if segment_parsed_bytes > 1 {
                    // This function is always called after we just parsed
                    // something which signalled that we should end the
                    // current segment - so use segment_parsed_bytes - 1 here,
                    // to exclude that char we just parsed.
                    let string_bytes = &state.bytes[0..(segment_parsed_bytes - 1)];

                    match parse_utf8(string_bytes) {
                        Ok(string) => {
                            state = state.advance_without_indenting(string.len())?;

                            segments.push($transform(string));
                        }
                        Err(reason) => {
                            return state.fail(reason);
                        }
                    }
                }

                // Depending on where this macro is used, in some
                // places this is unused.
                #[allow(unused_assignments)]
                {
                    // This function is always called after we just parsed
                    // something which signalled that we should end the
                    // current segment.
                    segment_parsed_bytes = 1;
                }
            };
        }

        while let Some(&byte) = bytes.next() {
            // This is for the byte we just grabbed from the iterator.
            segment_parsed_bytes += 1;

            match byte {
                b'"' => {
                    // This is the end of the string!
                    if segment_parsed_bytes == 1 && segments.is_empty() {
                        match bytes.next() {
                            Some(b'"') => {
                                // If the very first three chars were all `"`,
                                // then this literal begins with `"""`
                                // and is a block string.
                                return parse_block_string(arena, state, &mut bytes);
                            }
                            _ => {
                                return Ok((PlainLine(""), state.advance_without_indenting(2)?));
                            }
                        }
                    } else {
                        end_segment!(StrSegment::Plaintext);

                        let expr = if segments.len() == 1 {
                            // We had exactly one segment, so this is a candidate
                            // to be StrLiteral::Plaintext
                            match segments.pop().unwrap() {
                                StrSegment::Plaintext(string) => StrLiteral::PlainLine(string),
                                other => {
                                    let vec = bumpalo::vec![in arena; other];

                                    StrLiteral::Line(vec.into_bump_slice())
                                }
                            }
                        } else {
                            Line(segments.into_bump_slice())
                        };

                        // Advance the state 1 to account for the closing `"`
                        return Ok((expr, state.advance_without_indenting(1)?));
                    };
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
                b'\\' => {
                    // We're about to begin an escaped segment of some sort!
                    //
                    // Record the current segment so we can begin a new one.
                    // End it right before the `\` char we just parsed.
                    end_segment!(StrSegment::Plaintext);

                    // This is for the byte we're about to parse.
                    segment_parsed_bytes += 1;

                    // This is the start of a new escape. Look at the next byte
                    // to figure out what type of escape it is.
                    match bytes.next() {
                        Some(b'(') => {
                            // Advance past the `\(` before using the expr parser
                            state = state.advance_without_indenting(2)?;

                            let original_byte_count = state.bytes.len();

                            // This is an interpolated variable.
                            // Parse an arbitrary expression, then give a
                            // canonicalization error if that expression variant
                            // is not allowed inside a string interpolation.
                            let (loc_expr, new_state) =
                                skip_second!(loc(allocated(expr::expr(0))), ascii_char(')'))
                                    .parse(arena, state)?;

                            // Advance the iterator past the expr we just parsed.
                            for _ in 0..(original_byte_count - new_state.bytes.len()) {
                                bytes.next();
                            }

                            segments.push(StrSegment::Interpolated(loc_expr));

                            // Reset the segment
                            segment_parsed_bytes = 0;
                            state = new_state;
                        }
                        Some(b'u') => {
                            // This is an escaped unicode character
                            if let Some(b'(') = bytes.next() {
                                segment_parsed_bytes += 1;
                            } else {
                                // Whenever we encounter `\u` it must be followed
                                // by a `(` char!
                                return Err(unexpected(0, state, Attempting::StrLiteral));
                            }

                            while let Some(&byte) = bytes.next() {
                                segment_parsed_bytes += 1;

                                if (byte as char).is_ascii_hexdigit() {
                                    // This is the most common case.
                                } else if byte == b')' {
                                    // Add the segment
                                    end_segment!(|string: &'a str| {
                                        let value = &string[0..string.len() - 1];

                                        StrSegment::Unicode(Located {
                                            region: Region::zero(), // TODO calculate the right region
                                            value,
                                        })
                                    });

                                    // We're done parsing digits now.
                                    break;
                                } else {
                                    // Unicode escapes must all be digits!
                                    return Err(unexpected(0, state, Attempting::StrLiteral));
                                }
                            }
                        }
                        Some(b'\\') => {
                            escaped_char!('\\');
                        }
                        Some(b'"') => {
                            escaped_char!('"');
                        }
                        Some(b'r') => {
                            escaped_char!('\r');
                        }
                        Some(b't') => {
                            escaped_char!('\t');
                        }
                        Some(b'n') => {
                            escaped_char!('\n');
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
                }
                _ => {
                    // All other characters need no special handling.
                }
            }
        }

        // We ran out of characters before finding a closed quote
        Err(unexpected_eof(
            state.bytes.len(),
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
                            // state = state.advance_without_indenting(parsed_chars)?;

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
