use bumpalo::collections::string::String;
use bumpalo::Bump;
use parse::ast::{Attempting, Expr};
use parse::parser::{Parser, State};
use parse::problems::{Problem, Problems};
use region::{Loc, Region};
use std::char;
use std::iter::Peekable;

pub fn string_literal<'a, 'p>() -> impl Parser<'a, 'p, Expr<'a>>
where
    'p: 'a,
{
    move |arena: &'a Bump,
          state: &'a State<'a>,
          problems: &'p mut Problems,
          attempting: Attempting| {
        let initial_problems = problems.len();
        let mut chars = state.input.chars().peekable();

        // String literals must start with a quote.
        // If this doesn't, it must not be a string literal!
        if chars.next() != Some('"') {
            return Err((state.clone(), attempting));
        }

        // If we have precisely an empty string here, don't bother allocating
        // a buffer; instead, return EmptyStr immediately.
        if chars.peek() == Some(&'"') {
            return Ok((
                State {
                    input: &state.input[2..],
                    column: state.column + 2, // +2 because `""` has length 2

                    ..state.clone()
                },
                Expr::EmptyStr,
            ));
        }

        // Stores the accumulated string characters
        let mut buf = String::new_in(arena);

        while let Some(ch) = chars.next() {
            match ch {
                // If it's a backslash, escape things.
                '\\' => match chars.next() {
                    Some(next_ch) => {
                        handle_escaped_char(arena, state, next_ch, &mut chars, &mut buf, problems)?
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
                    let expr = if problems.len() <= initial_problems {
                        Expr::Str(buf.into_bump_str())
                    } else {
                        // Only include the new problems in the Expr.
                        let relevant_problems = &problems[initial_problems..];

                        Expr::MalformedStr(relevant_problems)
                    };

                    return Ok((
                        State {
                            input: &state.input[len_with_quotes..],
                            column: state.column + len_with_quotes as u32,

                            ..state.clone()
                        },
                        expr,
                    ));
                }
                '\t' => {
                    // TODO report the problem and continue.
                    // Tabs are syntax errors, but maybe the rest of the
                    // string is fine!
                    panic!("TODO string had a tab character in it.");
                }
                '\r' => {
                    // TODO report the problem and continue.
                    // Carriage returns aren't allowed in string literals,
                    // but maybe the rest of the string is fine!
                    panic!("TODO string had a tab character in it.");
                }
                '\n' => {
                    // TODO report the problem and then return Err.
                    // We can't safely assume where the string was supposed
                    // to end, so this is an unrecoverable error.
                    panic!("TODO string missing closing quote.");
                }
                normal_char => buf.push(normal_char),
            }
        }

        // We ran out of characters before finding a closed quote
        Err((state.clone(), Attempting::StringLiteral))
    }
}

fn escaped_char_problem<'a, 'p>(
    problems: &'p mut Problems,
    problem: Problem,
    state: &'a State<'a>,
    buf_len: usize,
) {
    let start_line = state.line;
    let start_col = state.column + buf_len as u32;
    let end_line = start_line;
    // escapes should all be 2 chars long
    let end_col = state.column + 1;

    let region = Region {
        start_line,
        start_col,
        end_line,
        end_col,
    };

    problems.push(Loc {
        region,
        value: problem,
    });
}

fn escaped_unicode_problem<'a, 'p>(
    problems: &'p mut Problems,
    problem: Problem,
    state: &'a State<'a>,
    buf_len: usize,
    hex_str_len: usize,
) {
    let start_line = state.line;
    // +1 due to the `"` which precedes buf.
    let start_col = state.column + buf_len as u32 + 1;
    let end_line = start_line;
    // +3 due to the `\u{` and another + 1 due to the `}`
    // -1 to prevent overshooting because end col is inclusive.
    let end_col = start_col + 3 + hex_str_len as u32 + 1 - 1;

    let region = Region {
        start_line,
        start_col,
        end_line,
        end_col,
    };

    problems.push(Loc {
        region,
        value: problem,
    });
}

#[inline(always)]
fn handle_escaped_char<'a, 'p, I>(
    arena: &'a Bump,
    state: &'a State<'a>,
    ch: char,
    chars: &mut Peekable<I>,
    buf: &mut String<'a>,
    problems: &'p mut Problems,
) -> Result<(), (State<'a>, Attempting)>
where
    I: Iterator<Item = char>,
{
    match ch {
        '\\' => buf.push('\\'),
        '"' => buf.push('"'),
        't' => buf.push('\t'),
        'n' => buf.push('\n'),
        'r' => buf.push('\r'),
        'u' => handle_escaped_unicode(arena, state, chars, buf, problems)?,
        '(' => panic!("TODO handle string interpolation"),
        '\t' => {
            // Report and continue.
            // Tabs are syntax errors, but maybe the rest of the string is fine!
            escaped_char_problem(problems, Problem::Tab, state, buf.len());
        }
        '\r' => {
            // Report and continue.
            // Carriage returns aren't allowed in string literals,
            // but maybe the rest of the string is fine!
            escaped_char_problem(problems, Problem::CarriageReturn, state, buf.len());
        }
        '\n' => {
            // Report and bail out.
            // We can't safely assume where the string was supposed to end.
            escaped_char_problem(problems, Problem::NewlineInLiteral, state, buf.len());

            return Err((state.clone(), Attempting::UnicodeEscape));
        }
        _ => {
            // Report and continue.
            // An unsupported escaped char (e.g. \q) shouldn't halt parsing.
            escaped_char_problem(problems, Problem::UnsupportedEscapedChar, state, buf.len());
        }
    }

    Ok(())
}

#[inline(always)]
fn handle_escaped_unicode<'a, 'p, I>(
    arena: &'a Bump,
    state: &'a State<'a>,
    chars: &mut Peekable<I>,
    buf: &mut String<'a>,
    problems: &'p mut Problems,
) -> Result<(), (State<'a>, Attempting)>
where
    I: Iterator<Item = char>,
{
    // \u{00A0} is how you specify a Unicode code point,
    // so we should always see a '{' next.
    if chars.next() != Some('{') {
        let start_line = state.line;
        // +1 due to the `"` which precedes buf
        let start_col = state.column + 1 + buf.len() as u32;
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
                            let start_col = state.column + 1 + buf.len() as u32 + 3;
                            let end_line = start_line;

                            // We want to underline only the number. That's the error!
                            // -1 because we want to end on the last digit, not
                            // overshoot it.
                            let end_col = start_col + hex_str.len() as u32 - 1;

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
                                    escaped_unicode_problem(
                                        problems,
                                        Problem::InvalidUnicodeCodePoint,
                                        state,
                                        start_of_unicode,
                                        hex_str.len(),
                                    );
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

                        escaped_unicode_problem(
                            problems,
                            problem,
                            state,
                            start_of_unicode,
                            hex_str.len(),
                        );
                    }
                }

                // We are now done processing the unicode portion of the string,
                // so exit the loop without further advancing the iterator.
                return Ok(());
            }
            '\t' => {
                // Report and continue.
                // Tabs are syntax errors, but maybe the rest of the string is fine!
                escaped_unicode_problem(
                    problems,
                    Problem::Tab,
                    state,
                    start_of_unicode,
                    hex_str.len(),
                );
            }
            '\r' => {
                // Report and continue.
                // Carriage returns aren't allowed in string literals,
                // but maybe the rest of the string is fine!
                escaped_unicode_problem(
                    problems,
                    Problem::CarriageReturn,
                    state,
                    start_of_unicode,
                    hex_str.len(),
                );
            }
            '\n' => {
                // Report and bail out.
                // We can't safely assume where the string was supposed to end.
                escaped_unicode_problem(
                    problems,
                    Problem::NewlineInLiteral,
                    state,
                    start_of_unicode,
                    hex_str.len(),
                );

                return Err((state.clone(), Attempting::UnicodeEscape));
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
            let start_col = state.column + buf.len() as u32 + 1;
            let end_line = start_line;
            // +3 due to the `\u{`
            // -1 to prevent overshooting because end col is inclusive.
            let end_col = start_col + 3 + hex_str.len() as u32 - 1;

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
