use crate::ast::{EscapedChar, StrLiteral, StrSegment};
use crate::expr;
use crate::parser::Progress::*;
use crate::parser::{allocated, loc, specialize_ref, word1, BadInputError, EString, Parser};
use crate::state::State;
use bumpalo::collections::vec::Vec;
use bumpalo::Bump;

/// One or more ASCII hex digits. (Useful when parsing unicode escape codes,
/// which must consist entirely of ASCII hex digits.)
fn ascii_hex_digits<'a>() -> impl Parser<'a, &'a str, EString<'a>> {
    move |arena, state: State<'a>| {
        let mut buf = bumpalo::collections::String::new_in(arena);

        for &byte in state.bytes().iter() {
            if (byte as char).is_ascii_hexdigit() {
                buf.push(byte as char);
            } else if buf.is_empty() {
                // We didn't find any hex digits!
                return Err((NoProgress, EString::CodePtEnd(state.pos()), state));
            } else {
                let state = state.advance(buf.len());

                return Ok((MadeProgress, buf.into_bump_str(), state));
            }
        }

        Err((NoProgress, EString::CodePtEnd(state.pos()), state))
    }
}

macro_rules! advance_state {
    ($state:expr, $n:expr) => {
        Ok($state.advance($n))
    };
}

pub fn parse<'a>() -> impl Parser<'a, StrLiteral<'a>, EString<'a>> {
    use StrLiteral::*;

    move |arena: &'a Bump, mut state: State<'a>| {
        let is_multiline;
        let mut bytes;

        if state.bytes().starts_with(b"\"\"\"") {
            // we will be parsing a multi-string
            is_multiline = true;
            bytes = state.bytes()[3..].iter();
            state = advance_state!(state, 3)?;
        } else if state.bytes().starts_with(b"\"") {
            // we will be parsing a single-string
            is_multiline = false;
            bytes = state.bytes()[1..].iter();
            state = advance_state!(state, 1)?;
        } else {
            return Err((NoProgress, EString::Open(state.pos()), state));
        }

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
                state = advance_state!(state, segment_parsed_bytes)?;

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
                    let string_bytes = &state.bytes()[0..(segment_parsed_bytes - 1)];

                    match std::str::from_utf8(string_bytes) {
                        Ok(string) => {
                            state = advance_state!(state, string.len())?;

                            segments.push($transform(string));
                        }
                        Err(_) => {
                            return Err((
                                MadeProgress,
                                EString::Space(BadInputError::BadUtf8, state.pos()),
                                state,
                            ));
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
                    if segment_parsed_bytes == 1 && segments.is_empty() {
                        // special case of the empty string
                        if is_multiline {
                            if bytes.as_slice().starts_with(b"\"\"") {
                                return Ok((MadeProgress, Block(&[]), advance_state!(state, 3)?));
                            } else {
                                // this quote is in a block string
                                continue;
                            }
                        } else {
                            // This is the end of the string!
                            // Advance 1 for the close quote
                            return Ok((MadeProgress, PlainLine(""), advance_state!(state, 1)?));
                        }
                    } else {
                        // the string is non-empty, which means we need to convert any previous segments
                        // and the current segment into a string literal
                        if is_multiline {
                            if bytes.as_slice().starts_with(b"\"\"") {
                                end_segment!(StrSegment::Plaintext);

                                let expr = if segments.len() == 1 {
                                    // We had exactly one segment, so this is a candidate
                                    // to be StrLiteral::Plaintext
                                    match segments.pop().unwrap() {
                                        StrSegment::Plaintext(string) => {
                                            StrLiteral::PlainLine(string)
                                        }
                                        other => StrLiteral::Line(arena.alloc([other])),
                                    }
                                } else {
                                    Block(arena.alloc([segments.into_bump_slice()]))
                                };

                                return Ok((MadeProgress, expr, advance_state!(state, 3)?));
                            } else {
                                // this quote is in a block string
                                continue;
                            }
                        } else {
                            end_segment!(StrSegment::Plaintext);

                            let expr = if segments.len() == 1 {
                                // We had exactly one segment, so this is a candidate
                                // to be StrLiteral::Plaintext
                                match segments.pop().unwrap() {
                                    StrSegment::Plaintext(string) => StrLiteral::PlainLine(string),
                                    other => StrLiteral::Line(arena.alloc([other])),
                                }
                            } else {
                                Line(segments.into_bump_slice())
                            };

                            // Advance the state 1 to account for the closing `"`
                            return Ok((MadeProgress, expr, advance_state!(state, 1)?));
                        }
                    };
                }
                b'\n' => {
                    if is_multiline {
                        continue;
                    } else {
                        // This is a single-line string, which cannot have newlines!
                        // Treat this as an unclosed string literal, and consume
                        // all remaining chars. This will mask all other errors, but
                        // it should make it easiest to debug; the file will be a giant
                        // error starting from where the open quote appeared.
                        return Err((MadeProgress, EString::EndlessSingle(state.pos()), state));
                    }
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
                            state = advance_state!(state, 2)?;

                            let original_byte_count = state.bytes().len();

                            // This is an interpolated variable.
                            // Parse an arbitrary expression, then give a
                            // canonicalization error if that expression variant
                            // is not allowed inside a string interpolation.
                            let (_progress, loc_expr, new_state) = skip_second!(
                                specialize_ref(EString::Format, loc(allocated(expr::expr_help(0)))),
                                word1(b')', EString::FormatEnd)
                            )
                            .parse(arena, state)?;

                            // Advance the iterator past the expr we just parsed.
                            for _ in 0..(original_byte_count - new_state.bytes().len()) {
                                bytes.next();
                            }

                            segments.push(StrSegment::Interpolated(loc_expr));

                            // Reset the segment
                            segment_parsed_bytes = 0;
                            state = new_state;
                        }
                        Some(b'u') => {
                            // Advance past the `\u` before using the expr parser
                            state = advance_state!(state, 2)?;

                            let original_byte_count = state.bytes().len();

                            // Parse the hex digits, surrounded by parens, then
                            // give a canonicalization error if the digits form
                            // an invalid unicode code point.
                            let (_progress, loc_digits, new_state) = between!(
                                word1(b'(', EString::CodePtOpen),
                                loc(ascii_hex_digits()),
                                word1(b')', EString::CodePtEnd)
                            )
                            .parse(arena, state)?;

                            // Advance the iterator past the expr we just parsed.
                            for _ in 0..(original_byte_count - new_state.bytes().len()) {
                                bytes.next();
                            }

                            segments.push(StrSegment::Unicode(loc_digits));

                            // Reset the segment
                            segment_parsed_bytes = 0;
                            state = new_state;
                        }
                        Some(b'\\') => {
                            escaped_char!(EscapedChar::Backslash);
                        }
                        Some(b'"') => {
                            escaped_char!(EscapedChar::Quote);
                        }
                        Some(b'r') => {
                            escaped_char!(EscapedChar::CarriageReturn);
                        }
                        Some(b't') => {
                            escaped_char!(EscapedChar::Tab);
                        }
                        Some(b'n') => {
                            escaped_char!(EscapedChar::Newline);
                        }
                        _ => {
                            // Invalid escape! A backslash must be followed
                            // by either an open paren or else one of the
                            // escapable characters (\n, \t, \", \\, etc)
                            return Err((MadeProgress, EString::UnknownEscape(state.pos()), state));
                        }
                    }
                }
                _ => {
                    // All other characters need no special handling.
                }
            }
        }

        // We ran out of characters before finding a closed quote
        Err((
            MadeProgress,
            if is_multiline {
                EString::EndlessMulti(state.pos())
            } else {
                EString::EndlessSingle(state.pos())
            },
            state,
        ))
    }
}
