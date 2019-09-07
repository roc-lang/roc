use bumpalo::collections::string::String;
use bumpalo::Bump;
use parse::ast::{Attempting, Expr};
use parse::parser::{unexpected, unexpected_eof, ParseResult, Parser, State};
use parse::problems::Problem;
use std::char;

pub fn number_literal<'a>() -> impl Parser<'a, Expr<'a>> {
    move |arena: &'a Bump, state: State<'a>| {
        let mut chars = state.input.chars();

        match chars.next() {
            Some(first_ch) => {
                // Number literals must start with either an '-' or a digit.
                if first_ch == '-' || first_ch.is_ascii_digit() {
                    parse_number_literal(first_ch, &mut chars, arena, state)
                } else {
                    Err(unexpected(
                        first_ch,
                        first_ch.len_utf8(),
                        state,
                        Attempting::NumberLiteral,
                    ))
                }
            }
            None => Err(unexpected_eof(0, state.attempting, state)),
        }
    }
}

#[inline(always)]
fn parse_number_literal<'a, I>(
    first_ch: char,
    chars: &mut I,
    arena: &'a Bump,
    state: State<'a>,
) -> ParseResult<'a, Expr<'a>>
where
    I: Iterator<Item = char>,
{
    let mut before_decimal = String::with_capacity_in(1, arena);
    let mut after_decimal = String::new_in(arena);
    let mut has_decimal_point = false;
    let mut chars_skipped = 0;

    // Put the first character into the buffer, even if all we've parsed so
    // far is a minus sign.
    //
    // We have to let i64::parse handle the minus sign (if it's there), because
    // otherwise if we ask it to parse i64::MIN.to_string() as a positive i64,
    // it errors because that positive number doesn't fit in an i64!
    before_decimal.push(first_ch);

    while let Some(next_ch) = chars.next() {
        match next_ch {
            digit if next_ch.is_ascii_digit() => {
                if has_decimal_point {
                    after_decimal.push(digit);
                } else {
                    before_decimal.push(digit);
                }
            }
            '_' => {
                // Underscores are allowed, and disregarded.
                chars_skipped += 1;
            }
            '.' => {
                if has_decimal_point {
                    // You only get one decimal point!
                    let len = before_decimal.len() + after_decimal.len() + chars_skipped;

                    return Err(unexpected('.', len, state, Attempting::NumberLiteral));
                } else {
                    chars_skipped += 1;
                    has_decimal_point = true;
                }
            }
            invalid_char => {
                if before_decimal.is_empty() {
                    // No digits! We likely parsed a minus sign that's actually an operator.
                    let len = before_decimal.len() + after_decimal.len() + chars_skipped;
                    return Err(unexpected(
                        invalid_char,
                        len,
                        state,
                        Attempting::NumberLiteral,
                    ));
                }

                // We hit an invalid number literal character; we're done!
                break;
            }
        }
    }

    // At this point we have a number, and will definitely succeed.
    // If the number is malformed (too large to fit), we'll succeed with
    // an appropriate Expr which records that.
    let expr = if has_decimal_point {
        let mut f64_buf = String::with_capacity_in(
            before_decimal.len()
            // +1 for the decimal point itself
            + 1
            + after_decimal.len(),
            arena,
        );

        f64_buf.push_str(&before_decimal);
        f64_buf.push('.');
        f64_buf.push_str(&after_decimal);

        match f64_buf.parse::<f64>() {
            Ok(float) => Expr::Float(float),
            Err(_) => Expr::MalformedNumber(Problem::TooLarge),
        }
    } else {
        match before_decimal.parse::<i64>() {
            Ok(int_val) => Expr::Int(int_val),
            Err(_) => Expr::MalformedNumber(Problem::TooLarge),
        }
    };

    let total_chars_parsed = before_decimal.len() + chars_skipped;
    let state = state.advance_without_indenting(total_chars_parsed)?;

    Ok((expr, state))
}
