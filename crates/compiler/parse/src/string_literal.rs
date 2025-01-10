use crate::ast::{EscapedChar, SingleQuoteLiteral, Spaceable, StrLiteral, StrSegment};
use crate::blankspace::space0_e;
use crate::expr;
use crate::parser::Progress::{self, *};
use crate::parser::{
    allocated, and, between, byte, loc, reset_min_indent, skip_second, specialize_err_ref, then,
    BadInputError, ESingleQuote, EString, Parser,
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

pub enum StrLikeLiteral<'a> {
    SingleQuote(SingleQuoteLiteral<'a>),
    Str(StrLiteral<'a>),
}

pub fn parse_str_literal<'a>() -> impl Parser<'a, StrLiteral<'a>, EString<'a>> {
    then(
        loc(parse_str_like_literal()),
        |_arena, state, progress, str_like| match str_like.value {
            StrLikeLiteral::SingleQuote(_) => Err((
                progress,
                EString::ExpectedDoubleQuoteGotSingleQuote(str_like.region.start()),
            )),
            StrLikeLiteral::Str(str_literal) => Ok((progress, str_literal, state)),
        },
    )
}

pub fn parse_str_like_literal<'a>() -> impl Parser<'a, StrLikeLiteral<'a>, EString<'a>> {
    move |arena: &'a Bump, mut state: State<'a>, min_indent: u32| {
        let is_multiline;
        let is_single_quote;

        let indent = state.column();

        let start_state;

        if state.consume_mut("\"\"\"") {
            start_state = state.clone();

            // we will be parsing a multi-line string
            is_multiline = true;
            is_single_quote = false;

            if state.consume_mut("\n") {
                state = consume_indent(state, indent)?;
            }
        } else if state.consume_mut("\"") {
            start_state = state.clone();

            // we will be parsing a single-line string
            is_multiline = false;
            is_single_quote = false;
        } else if state.consume_mut("'") {
            start_state = state.clone();

            is_multiline = false;
            is_single_quote = true;
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

        let mut preceded_by_dollar = false;

        while let Some(&one_byte) = bytes.next() {
            // This is for the byte we just grabbed from the iterator.
            segment_parsed_bytes += 1;

            match one_byte {
                b'"' if !is_single_quote => {
                    preceded_by_dollar = false;
                    if segment_parsed_bytes == 1 && segments.is_empty() {
                        // special case of the empty string
                        if is_multiline {
                            if bytes.as_slice().starts_with(b"\"\"") {
                                return Ok((
                                    MadeProgress,
                                    StrLikeLiteral::Str(StrLiteral::Block(&[])),
                                    state.advance(3),
                                ));
                            } else {
                                // this quote is in a block string
                                continue;
                            }
                        } else {
                            // This is the end of the string!
                            // Advance 1 for the close quote
                            return Ok((
                                MadeProgress,
                                StrLikeLiteral::Str(StrLiteral::PlainLine("")),
                                state.advance(1),
                            ));
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
                                    StrLiteral::Block(arena.alloc([segments.into_bump_slice()]))
                                };

                                return Ok((
                                    MadeProgress,
                                    StrLikeLiteral::Str(expr),
                                    state.advance(3),
                                ));
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
                                StrLiteral::Line(segments.into_bump_slice())
                            };

                            // Advance the state 1 to account for the closing `"`
                            return Ok((MadeProgress, StrLikeLiteral::Str(expr), state.advance(1)));
                        }
                    };
                }
                b'\'' if is_single_quote => {
                    end_segment!(StrSegment::Plaintext);

                    let expr = if segments.len() == 1 {
                        // We had exactly one segment, so this is a candidate
                        // to be SingleQuoteLiteral::Plaintext
                        match segments.pop().unwrap() {
                            StrSegment::Plaintext(string) => SingleQuoteLiteral::PlainLine(string),
                            other => {
                                let o = other.try_into().map_err(|e| {
                                    (
                                        MadeProgress,
                                        EString::InvalidSingleQuote(e, start_state.pos()),
                                    )
                                })?;
                                SingleQuoteLiteral::Line(arena.alloc([o]))
                            }
                        }
                    } else {
                        let mut new_segments = Vec::with_capacity_in(segments.len(), arena);
                        for segment in segments {
                            let segment = segment.try_into().map_err(|e| {
                                (
                                    MadeProgress,
                                    EString::InvalidSingleQuote(e, start_state.pos()),
                                )
                            })?;
                            new_segments.push(segment);
                        }

                        SingleQuoteLiteral::Line(new_segments.into_bump_slice())
                    };

                    // Validate that the string is a valid char literal.
                    // Note that currently, we accept anything that:
                    // * Is between 1 and 5 bytes long
                    //   -> utf-8 encoding is trivial to extend to 5 bytes, even tho 4 is the technical max
                    //   -> TODO: do we want to change this?
                    // * Decodes as valid UTF-8
                    //   -> Might be a single code point, or multiple code points
                    //   -> TODO: do we want to change this?

                    // Simply by decoding this, it's guaranteed to be valid utf-8
                    let text = expr.to_str_in(arena).map_err(|e| (MadeProgress, e))?;

                    if text.len() > 5 {
                        return Err((
                            MadeProgress,
                            EString::InvalidSingleQuote(ESingleQuote::TooLong, start_state.pos()),
                        ));
                    }

                    if text.is_empty() {
                        return Err((
                            MadeProgress,
                            EString::InvalidSingleQuote(ESingleQuote::Empty, start_state.pos()),
                        ));
                    }

                    // Advance the state 1 to account for the closing `'`
                    return Ok((
                        MadeProgress,
                        StrLikeLiteral::SingleQuote(expr),
                        state.advance(1),
                    ));
                }
                b'\n' => {
                    preceded_by_dollar = false;
                    if is_multiline {
                        let without_newline = &state.bytes()[0..(segment_parsed_bytes - 1)];
                        let with_newline = &state.bytes()[0..segment_parsed_bytes];

                        state.advance_mut(segment_parsed_bytes - 1);
                        state = state.advance_newline();
                        state = consume_indent(state, indent)?;
                        state = state.mark_current_indent();
                        bytes = state.bytes().iter();

                        if state.bytes().starts_with(b"\"\"\"") {
                            // ending the string; don't use the last newline
                            if !without_newline.is_empty() {
                                segments.push(StrSegment::Plaintext(utf8(
                                    state.clone(),
                                    without_newline,
                                )?));
                            }
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
                        return Err((MadeProgress, EString::EndlessSingleLine(start_state.pos())));
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
                        Some(b'u') => {
                            // Advance past the `\u` before using the expr parser
                            state.advance_mut(2);

                            let original_byte_count = state.bytes().len();

                            // Parse the hex digits, surrounded by parens, then
                            // give a canonicalization error if the digits form
                            // an invalid unicode code point.
                            let (_progress, loc_digits, new_state) = between(
                                byte(b'(', EString::CodePtOpen),
                                loc(ascii_hex_digits()),
                                byte(b')', EString::CodePtEnd),
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
                            escaped_char!(EscapedChar::DoubleQuote);
                        }
                        Some(b'\'') => {
                            escaped_char!(EscapedChar::SingleQuote);
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
                        Some(b'$') => {
                            escaped_char!(EscapedChar::Dollar);
                        }
                        _ => {
                            // Invalid escape! A backslash must be followed
                            // by one of these escapable characters:
                            // (\n, \t, \", \\, etc)
                            return Err((MadeProgress, EString::UnknownEscape(state.pos())));
                        }
                    }
                }
                b'(' | b'{' if preceded_by_dollar && !is_single_quote => {
                    let old_style_interpolation_block = one_byte == b'(';

                    // We're about to begin string interpolation!
                    //
                    // End the previous segment so we can begin a new one.
                    // Retroactively end it right before the `$` char we parsed.
                    // (We can't use end_segment! here because it ends it right after
                    // the just-parsed character, which here would be '{' rather than '$')
                    // Don't push anything if the string would be empty.
                    if segment_parsed_bytes > 2 {
                        // exclude the 2 chars we just parsed, namely '$' and '{'
                        let string_bytes = &state.bytes()[0..(segment_parsed_bytes - 2)];

                        match std::str::from_utf8(string_bytes) {
                            Ok(string) => {
                                state.advance_mut(string.len());

                                segments.push(StrSegment::Plaintext(string));
                            }
                            Err(_) => {
                                return Err((
                                    MadeProgress,
                                    EString::Space(BadInputError::BadUtf8, state.pos()),
                                ));
                            }
                        }
                    }

                    // Advance past the `${`
                    state.advance_mut(2);

                    let original_byte_count = state.bytes().len();

                    // Parse an arbitrary expression, followed by '}' or ')'
                    let terminating_char = if old_style_interpolation_block {
                        b')'
                    } else {
                        b'}'
                    };
                    let (_progress, (mut loc_expr, sp), new_state) = and(
                        specialize_err_ref(
                            EString::Format,
                            loc(allocated(reset_min_indent(expr::expr_help())))
                                .trace("str_interpolation"),
                        ),
                        skip_second(
                            space0_e(EString::FormatEnd),
                            byte(terminating_char, EString::FormatEnd),
                        ),
                    )
                    .parse(arena, state, min_indent)?;

                    if !sp.is_empty() {
                        loc_expr.value = arena.alloc(loc_expr.value.after(sp));
                    }

                    // Advance the iterator past the expr we just parsed.
                    for _ in 0..(original_byte_count - new_state.bytes().len()) {
                        bytes.next();
                    }

                    segments.push(StrSegment::Interpolated(loc_expr));

                    // Reset the segment
                    segment_parsed_bytes = 0;
                    state = new_state;
                }
                _ => {
                    // All other characters need no special handling.
                }
            }

            // iff the '$' is followed by '{', this is string interpolation.
            // We'll check for the '{' on the next iteration of the loop.
            preceded_by_dollar = one_byte == b'$';
        }

        // We ran out of characters before finding a closed quote
        Err((
            MadeProgress,
            if is_single_quote {
                EString::EndlessSingleQuote(start_state.pos())
            } else if is_multiline {
                EString::EndlessMultiLine(start_state.pos())
            } else {
                EString::EndlessSingleLine(start_state.pos())
            },
        ))
    }
}
