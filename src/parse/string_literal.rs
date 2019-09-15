use bumpalo::collections::string::String;
use bumpalo::collections::vec::Vec;
use bumpalo::Bump;
use parse::ast::{Attempting, Expr};
use parse::ident;
use parse::parser::{unexpected, unexpected_eof, Fail, Parser, State};
use parse::problems::{Problem, Problems};
use region::{Loc, Region};
use std::char;
use std::iter::Peekable;

pub fn string_literal<'a>() -> impl Parser<'a, Expr<'a>> {
    move |arena: &'a Bump, state: State<'a>| {
        let mut problems = std::vec::Vec::new();
        let mut chars = state.input.chars().peekable();

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

        // If we have precisely an empty string here, don't bother allocating
        // a buffer; instead, return EmptyStr immediately.
        if chars.peek() == Some(&'"') {
            return Ok((
                Expr::EmptyStr,
                // 2 because `""` has length 2
                state.advance_without_indenting(2)?,
            ));
        }

        // Stores the accumulated string characters
        let mut buf = String::new_in(arena);

        // This caches the total string length of interpolated_pairs. Every
        // time we add a new pair to interpolated_pairs, we increment this
        // by the sum of whatever we parsed in order to obtain that pair.
        let mut buf_col_offset: usize = 0;

        // Stores interpolated identifiers, if any.
        let mut interpolated_pairs = Vec::new_in(arena);

        while let Some(ch) = chars.next() {
            match ch {
                // If it's a backslash, escape things.
                '\\' => match chars.next() {
                    Some(next_ch) => {
                        if let Some(ident) = handle_escaped_char(
                            arena,
                            &state,
                            next_ch,
                            &mut chars,
                            &mut buf,
                            &mut problems,
                        )? {
                            // +2 for `\(` and then another +1 for `)` at the end
                            let parsed_length = buf.len() + 2 + ident.len() + 1;

                            // It's okay if casting fails in this section, because
                            // we're going to check for line length overflow at the
                            // end anyway. That will render this region useless,
                            // but the user wasn't going to see this region
                            // anyway if the line length overflowed.
                            let start_line = state.line;

                            // Subtract ident length and another 1 for the `)`
                            let start_col = state.column
                                + buf_col_offset as u16
                                + (parsed_length - ident.len() - 1) as u16;
                            let ident_region = Region {
                                start_line,
                                start_col,
                                end_line: start_line,
                                end_col: start_col + ident.len() as u16 - 1,
                            };
                            let loc_ident = Loc {
                                region: ident_region,
                                value: ident,
                            };

                            // Push the accumulated string into the pairs list,
                            // along with the ident that came after it.
                            interpolated_pairs.push((buf.into_bump_str(), loc_ident));

                            // Reset the buffer so we start working on a new string.
                            buf = String::new_in(arena);

                            // Advance the cached offset of how many chars we've parsed,
                            // so the next time we see an interpolated ident, we can
                            // correctly calculate its region.
                            buf_col_offset += parsed_length;
                        }
                    }
                    None => {
                        // We ran out of characters before finding a closed quote;
                        // let the loop finish normally, so we end up returning
                        // the error that the string was not terminated.
                        //
                        // (There's the separate problem of a trailing backslash,
                        // but often that will get fixed in the course of
                        // addressing the missing closed quote.)
                        ()
                    }
                },
                '"' => {
                    // We found a closed quote; this is the end of the string!
                    let len_with_quotes = buf.len() + 2;
                    let expr = if problems.is_empty() {
                        let final_str = buf.into_bump_str();

                        if interpolated_pairs.is_empty() {
                            Expr::Str(final_str)
                        } else {
                            let tuple_ref =
                                arena.alloc((interpolated_pairs.into_bump_slice(), final_str));

                            Expr::InterpolatedStr(tuple_ref)
                        }
                    } else {
                        Expr::MalformedStr(problems.into_boxed_slice())
                    };

                    let next_state = state.advance_without_indenting(len_with_quotes)?;

                    return Ok((expr, next_state));
                }
                '\t' => {
                    // Report the problem and continue. Tabs are syntax errors,
                    // but maybe the rest of the string is fine!
                    problems.push(loc_char(Problem::Tab, &state, buf.len()));
                }
                '\r' => {
                    // Carriage returns aren't allowed in string literals,
                    // but maybe the rest of the string is fine!
                    problems.push(loc_char(Problem::CarriageReturn, &state, buf.len()));
                }
                '\n' => {
                    // We hit a newline before a close quote.
                    // We can't safely assume where the string was supposed
                    // to end, so this is an unrecoverable error.
                    return Err(unexpected('\n', 0, state, Attempting::StringLiteral));
                }
                normal_char => buf.push(normal_char),
            }
        }

        // We ran out of characters before finding a closed quote
        Err(unexpected_eof(
            buf.len(),
            Attempting::StringLiteral,
            state.clone(),
        ))
    }
}

fn loc_char<'a, V>(value: V, state: &State<'a>, buf_len: usize) -> Loc<V> {
    let start_line = state.line;
    let start_col = state.column + buf_len as u16;
    let end_line = start_line;
    // All invalid chars should have a length of 1
    let end_col = state.column + 1;

    let region = Region {
        start_line,
        start_col,
        end_line,
        end_col,
    };

    Loc { region, value }
}

fn loc_escaped_char<'a, V>(value: V, state: &State<'a>, buf_len: usize) -> Loc<V> {
    let start_line = state.line;
    let start_col = state.column + buf_len as u16;
    let end_line = start_line;
    // escapes should all be 2 chars long
    let end_col = state.column + 1;

    let region = Region {
        start_line,
        start_col,
        end_line,
        end_col,
    };

    Loc { region, value }
}

fn loc_escaped_unicode<'a, V>(
    value: V,
    state: &State<'a>,
    buf_len: usize,
    hex_str_len: usize,
) -> Loc<V> {
    let start_line = state.line;
    // +1 due to the `"` which precedes buf.
    let start_col = state.column + buf_len as u16 + 1;
    let end_line = start_line;
    // +3 due to the `\u{` and another + 1 due to the `}`
    // -1 to prevent overshooting because end col is inclusive.
    let end_col = start_col + 3 + hex_str_len as u16 + 1 - 1;

    let region = Region {
        start_line,
        start_col,
        end_line,
        end_col,
    };

    Loc { region, value }
}

#[inline(always)]
fn handle_escaped_char<'a, I>(
    arena: &'a Bump,
    state: &State<'a>,
    ch: char,
    chars: &mut Peekable<I>,
    buf: &mut String<'a>,
    problems: &mut Problems,
) -> Result<Option<&'a str>, (Fail, State<'a>)>
where
    I: Iterator<Item = char>,
{
    match ch {
        '\\' => buf.push('\\'),
        '"' => buf.push('"'),
        't' => buf.push('\t'),
        'n' => buf.push('\n'),
        'r' => buf.push('\r'),
        '0' => buf.push('\0'), // We explicitly support null characters, as we
        // can't be sure we won't receive them from Rust.
        'u' => handle_escaped_unicode(arena, &state, chars, buf, problems)?,
        '(' => {
            let ident = parse_interpolated_ident(arena, state, chars)?;

            return Ok(Some(ident));
        }
        '\t' => {
            // Report and continue.
            // Tabs are syntax errors, but maybe the rest of the string is fine!
            problems.push(loc_escaped_char(Problem::Tab, &state, buf.len()));
        }
        '\r' => {
            // Report and continue.
            // Carriage returns aren't allowed in string literals,
            // but maybe the rest of the string is fine!
            problems.push(loc_escaped_char(Problem::CarriageReturn, &state, buf.len()));
        }
        '\n' => {
            // Report and bail out.
            // We can't safely assume where the string was supposed to end.
            problems.push(loc_escaped_char(
                Problem::NewlineInLiteral,
                &state,
                buf.len(),
            ));

            return Err(unexpected_eof(
                buf.len(),
                Attempting::UnicodeEscape,
                state.clone(),
            ));
        }
        _ => {
            // Report and continue.
            // An unsupported escaped char (e.g. \q) shouldn't halt parsing.
            problems.push(loc_escaped_char(
                Problem::UnsupportedEscapedChar,
                &state,
                buf.len(),
            ));
        }
    }

    Ok(None)
}

#[inline(always)]
fn handle_escaped_unicode<'a, I>(
    arena: &'a Bump,
    state: &State<'a>,
    chars: &mut Peekable<I>,
    buf: &mut String<'a>,
    problems: &mut Problems,
) -> Result<(), (Fail, State<'a>)>
where
    I: Iterator<Item = char>,
{
    // \u{00A0} is how you specify a Unicode code point,
    // so we should always see a '{' next.
    if chars.next() != Some('{') {
        let start_line = state.line;
        // +1 due to the `"` which precedes buf
        let start_col = state.column + 1 + buf.len() as u16;
        let end_line = start_line;

        // All we parsed was `\u`, so end on the column after `\`'s column.
        let end_col = start_col + 1;

        let region = Region {
            start_line,
            start_col,
            end_line,
            end_col,
        };

        problems.push(Loc {
            region,
            value: Problem::NoUnicodeDigits,
        });

        // The rest of the string literal might be fine. Keep parsing!
        return Ok(());
    }

    // Record the point in the string literal where we started parsing `\u`
    let start_of_unicode = buf.len();

    // Stores the accumulated unicode digits
    let mut hex_str = String::new_in(arena);

    // TODO don't bail out on invalid unicode sequences!
    // Instead, parse successfully as a Problem - like,
    // this string has a problem with it, but that doesn't
    // mean we have to fail parsing.
    while let Some(hex_char) = chars.next() {
        match hex_char {
            '}' => {
                // Done! Validate and add it to the buffer.
                match u32::from_str_radix(&hex_str, 16) {
                    Ok(code_pt) => {
                        if code_pt > 0x10FFFF {
                            let start_line = state.line;
                            // +1 due to the `"` which precedes buf
                            // +3 due to the `\u{` which precedes the hex digits
                            let start_col = state.column + 1 + buf.len() as u16 + 3;
                            let end_line = start_line;

                            // We want to underline only the number. That's the error!
                            // -1 because we want to end on the last digit, not
                            // overshoot it.
                            let end_col = start_col + hex_str.len() as u16 - 1;

                            let region = Region {
                                start_line,
                                start_col,
                                end_line,
                                end_col,
                            };

                            problems.push(Loc {
                                region,
                                value: Problem::UnicodeCodePointTooLarge,
                            });
                        } else {
                            // If it all checked out, add it to
                            // the main buffer.
                            match char::from_u32(code_pt) {
                                Some(ch) => buf.push(ch),
                                None => {
                                    problems.push(loc_escaped_unicode(
                                        Problem::InvalidUnicodeCodePoint,
                                        &state,
                                        start_of_unicode,
                                        hex_str.len(),
                                    ));
                                }
                            }
                        }
                    }
                    Err(_) => {
                        let problem = if hex_str.is_empty() {
                            Problem::NoUnicodeDigits
                        } else {
                            Problem::NonHexCharsInUnicodeCodePoint
                        };

                        problems.push(loc_escaped_unicode(
                            problem,
                            &state,
                            start_of_unicode,
                            hex_str.len(),
                        ));
                    }
                }

                // We are now done processing the unicode portion of the string,
                // so exit the loop without further advancing the iterator.
                return Ok(());
            }
            '\t' => {
                // Report and continue.
                // Tabs are syntax errors, but maybe the rest of the string is fine!
                problems.push(loc_escaped_unicode(
                    Problem::Tab,
                    &state,
                    start_of_unicode,
                    hex_str.len(),
                ));
            }
            '\r' => {
                // Report and continue.
                // Carriage returns aren't allowed in string literals,
                // but maybe the rest of the string is fine!
                problems.push(loc_escaped_unicode(
                    Problem::CarriageReturn,
                    &state,
                    start_of_unicode,
                    hex_str.len(),
                ));
            }
            '\n' => {
                // Report and bail out.
                // We can't safely assume where the string was supposed to end.
                problems.push(loc_escaped_unicode(
                    Problem::NewlineInLiteral,
                    &state,
                    start_of_unicode,
                    hex_str.len(),
                ));

                return Err(unexpected_eof(
                    buf.len(),
                    Attempting::UnicodeEscape,
                    state.clone(),
                ));
            }
            normal_char => hex_str.push(normal_char),
        }

        // If we're about to hit the end of the string, and we didn't already
        // complete parsing a valid unicode escape sequence, this is a malformed
        // escape sequence - it wasn't terminated!
        if chars.peek() == Some(&'"') {
            // Record a problem and exit the loop early, so the string literal
            // parsing logic can consume the quote and do its job as normal.
            let start_line = state.line;
            // +1 due to the `"` which precedes buf.
            let start_col = state.column + buf.len() as u16 + 1;
            let end_line = start_line;
            // +3 due to the `\u{`
            // -1 to prevent overshooting because end col is inclusive.
            let end_col = start_col + 3 + hex_str.len() as u16 - 1;

            let region = Region {
                start_line,
                start_col,
                end_line,
                end_col,
            };

            problems.push(Loc {
                region,
                value: Problem::MalformedEscapedUnicode,
            });

            return Ok(());
        }
    }

    Ok(())
}

#[inline(always)]
fn parse_interpolated_ident<'a, I>(
    arena: &'a Bump,
    state: &State<'a>,
    chars: &mut Peekable<I>,
) -> Result<&'a str, (Fail, State<'a>)>
where
    I: Iterator<Item = char>,
{
    // This will return Err on invalid identifiers like "if"
    let ((string, next_char), state) = ident::parse_into(arena, chars, state.clone())?;

    // Make sure we got a closing ) to end the interpolation.
    match next_char {
        Some(')') => Ok(string),
        Some(ch) => Err(unexpected(ch, 0, state, Attempting::InterpolatedString)),
        None => Err(unexpected_eof(0, Attempting::InterpolatedString, state)),
    }
}
