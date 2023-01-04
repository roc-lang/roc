use crate::ast::{EscapedChar, StrLiteral, StrSegment};
use crate::expr;
use crate::parser::Progress::{self, *};
use crate::parser::{
    allocated, loc, reset_min_indent, specialize_ref, word1, BadInputError, EString, Parser,
};
use crate::state::State;
use bumpalo::collections::vec::Vec;
use bumpalo::Bump;

/// One or more ASCII hex digits. (Useful when parsing unicode escape codes,
/// which must consist entirely of ASCII hex digits.)
fn ascii_hex_digits<'a>() -> impl Parser<'a, &'a str, EString<'a>> {
    move |arena, mut state: State<'a>, _min_indent: u32| {
        let mut buf = bumpalo::collections::String::new_in(arena);

        for &byte in state.bytes().iter() {
            if (byte as char).is_ascii_hexdigit() {
                buf.push(byte as char);
            } else if buf.is_empty() {
                // We didn't find any hex digits!
                return Err((NoProgress, EString::CodePtEnd(state.pos())));
            } else {
                state.advance_mut(buf.len());

                return Ok((MadeProgress, buf.into_bump_str(), state));
            }
        }

        Err((NoProgress, EString::CodePtEnd(state.pos())))
    }
}

pub fn parse_single_quote<'a>() -> impl Parser<'a, &'a str, EString<'a>> {
    move |arena: &'a Bump, mut state: State<'a>, _min_indent: u32| {
        if state.consume_mut("\'") {
            // we will be parsing a single-quote-string
        } else {
            return Err((NoProgress, EString::Open(state.pos())));
        }

        // Handle back slaches in byte literal
        // - starts with a backslash and used as an escape character. ex: '\n', '\t'
        // - single quote floating (un closed single quote) should be an error
        match state.bytes().first() {
            Some(b'\\') => {
                state.advance_mut(1);
                match state.bytes().first() {
                    Some(&ch) => {
                        state.advance_mut(1);
                        if (ch == b'n' || ch == b'r' || ch == b't' || ch == b'\'' || ch == b'\\')
                            && (state.bytes().first() == Some(&b'\''))
                        {
                            state.advance_mut(1);
                            let test = match ch {
                                b'n' => '\n',
                                b't' => '\t',
                                b'r' => '\r',
                                // since we checked the current char between the single quotes we
                                // know they are valid UTF-8, allowing us to use 'from_u32_unchecked'
                                _ => unsafe { char::from_u32_unchecked(ch as u32) },
                            };

                            return Ok((MadeProgress, &*arena.alloc_str(&test.to_string()), state));
                        }
                        // invalid error, backslah escaping something we do not recognize
                        return Err((NoProgress, EString::CodePtEnd(state.pos())));
                    }
                    None => {
                        // no close quote found
                        return Err((NoProgress, EString::CodePtEnd(state.pos())));
                    }
                }
            }
            Some(_) => {
                // do nothing for other characters, handled below
            }
            None => return Err((NoProgress, EString::CodePtEnd(state.pos()))),
        }

        let mut bytes = state.bytes().iter();
        let mut end_index = 1;

        // Copy paste problem in mono

        loop {
            match bytes.next() {
                Some(b'\'') => {
                    break;
                }
                Some(_) => end_index += 1,
                None => {
                    return Err((NoProgress, EString::Open(state.pos())));
                }
            }
        }

        if end_index == 1 {
            // no progress was made
            // this case is a double single quote, ex: ''
            // not supporting empty single quotes
            return Err((NoProgress, EString::Open(state.pos())));
        }

        if end_index > (std::mem::size_of::<u32>() + 1) {
            // bad case: too big to fit into u32
            return Err((NoProgress, EString::Open(state.pos())));
        }

        // happy case -> we have some bytes that will fit into a u32
        // ending up w/ a slice of bytes that we want to convert into an integer
        let raw_bytes = &state.bytes()[0..end_index - 1];

        state.advance_mut(end_index);
        match std::str::from_utf8(raw_bytes) {
            Ok(string) => Ok((MadeProgress, string, state)),
            Err(_) => {
                // invalid UTF-8
                return Err((NoProgress, EString::CodePtEnd(state.pos())));
            }
        }
    }
}

fn consume_indent(mut state: State, mut indent: u32) -> Result<State, (Progress, EString)> {
    while indent > 0 {
        match state.bytes().first() {
            Some(b' ') => {
                state.advance_mut(1);
                indent -= 1;
            }
            None | Some(b'\n') => {
                break;
            }
            Some(_) => {
                return Err((
                    MadeProgress,
                    EString::MultilineInsufficientIndent(state.pos()),
                ));
            }
        }
    }

    Ok(state)
}

fn utf8<'a>(state: State<'a>, string_bytes: &'a [u8]) -> Result<&'a str, (Progress, EString<'a>)> {
    std::str::from_utf8(string_bytes).map_err(|_| {
        // Note Based on where this `utf8` function is used, the fact that we know the whole string
        // in the parser is valid utf8, and barring bugs in the parser itself
        // (e.g. where we accidentally split a multibyte utf8 char), this error _should_ actually be unreachable.
        (
            MadeProgress,
            EString::Space(BadInputError::BadUtf8, state.pos()),
        )
    })
}

pub fn parse<'a>() -> impl Parser<'a, StrLiteral<'a>, EString<'a>> {
    use StrLiteral::*;

    move |arena: &'a Bump, mut state: State<'a>, min_indent: u32| {
        let is_multiline;

        let indent = state.column();

        let start_state;

        if state.consume_mut("\"\"\"") {
            start_state = state.clone();

            // we will be parsing a multi-line string
            is_multiline = true;

            if state.consume_mut("\n") {
                state = consume_indent(state, indent)?;
            }
        } else if state.consume_mut("\"") {
            start_state = state.clone();

            // we will be parsing a single-line string
            is_multiline = false;
        } else {
            return Err((NoProgress, EString::Open(state.pos())));
        }

        let mut bytes = state.bytes().iter();

        let mut segment_parsed_bytes = 0;
        let mut segments = Vec::new_in(arena);

        macro_rules! escaped_char {
            ($ch:expr) => {
                // Record the escaped char.
                segments.push(StrSegment::EscapedChar($ch));

                // Advance past the segment we just added
                state.advance_mut(segment_parsed_bytes);

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
                            state.advance_mut(string.len());

                            segments.push($transform(string));
                        }
                        Err(_) => {
                            return Err((
                                MadeProgress,
                                EString::Space(BadInputError::BadUtf8, state.pos()),
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
                                return Ok((MadeProgress, Block(&[]), state.advance(3)));
                            } else {
                                // this quote is in a block string
                                continue;
                            }
                        } else {
                            // This is the end of the string!
                            // Advance 1 for the close quote
                            return Ok((MadeProgress, PlainLine(""), state.advance(1)));
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

                                return Ok((MadeProgress, expr, state.advance(3)));
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
                            return Ok((MadeProgress, expr, state.advance(1)));
                        }
                    };
                }
                b'\n' => {
                    if is_multiline {
                        let without_newline = &state.bytes()[0..(segment_parsed_bytes - 1)];
                        let with_newline = &state.bytes()[0..segment_parsed_bytes];

                        state.advance_mut(segment_parsed_bytes);
                        state = consume_indent(state, indent)?;
                        bytes = state.bytes().iter();

                        if state.bytes().starts_with(b"\"\"\"") {
                            // ending the string; don't use the last newline
                            segments
                                .push(StrSegment::Plaintext(utf8(state.clone(), without_newline)?));
                        } else {
                            segments
                                .push(StrSegment::Plaintext(utf8(state.clone(), with_newline)?));
                        }

                        segment_parsed_bytes = 0;

                        continue;
                    } else {
                        // This is a single-line string, which cannot have newlines!
                        // Treat this as an unclosed string literal, and consume
                        // all remaining chars. This will mask all other errors, but
                        // it should make it easiest to debug; the file will be a giant
                        // error starting from where the open quote appeared.
                        return Err((MadeProgress, EString::EndlessSingle(start_state.pos())));
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
                            state.advance_mut(2);

                            let original_byte_count = state.bytes().len();

                            // This is an interpolated variable.
                            // Parse an arbitrary expression, then give a
                            // canonicalization error if that expression variant
                            // is not allowed inside a string interpolation.
                            let (_progress, loc_expr, new_state) = skip_second!(
                                specialize_ref(
                                    EString::Format,
                                    loc(allocated(reset_min_indent(expr::expr_help())))
                                ),
                                word1(b')', EString::FormatEnd)
                            )
                            .parse(arena, state, min_indent)?;

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
                            state.advance_mut(2);

                            let original_byte_count = state.bytes().len();

                            // Parse the hex digits, surrounded by parens, then
                            // give a canonicalization error if the digits form
                            // an invalid unicode code point.
                            let (_progress, loc_digits, new_state) = between!(
                                word1(b'(', EString::CodePtOpen),
                                loc(ascii_hex_digits()),
                                word1(b')', EString::CodePtEnd)
                            )
                            .parse(arena, state, min_indent)?;

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
                            return Err((MadeProgress, EString::UnknownEscape(state.pos())));
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
                EString::EndlessMulti(start_state.pos())
            } else {
                EString::EndlessSingle(start_state.pos())
            },
        ))
    }
}
