use parse::ast::{Attempting, Expr};
use parse::parser::{unexpected, unexpected_eof, ParseResult, Parser, State};
use std::char;

pub fn number_literal<'a>() -> impl Parser<'a, Expr<'a>> {
    move |_arena, state: State<'a>| {
        let mut chars = state.input.chars();

        match chars.next() {
            Some(first_ch) => {
                // Number literals must start with either an '-' or a digit.
                if first_ch == '-' || first_ch.is_ascii_digit() {
                    parse_number_literal(first_ch, &mut chars, state)
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
    state: State<'a>,
) -> ParseResult<'a, Expr<'a>>
where
    I: Iterator<Item = char>,
{
    use self::LiteralType::*;

    let mut typ = Int;

    // We already parsed 1 character (which may have been a minus sign).
    let mut chars_parsed = 1;

    for next_ch in chars {
        let err_unexpected = || {
            Err(unexpected(
                next_ch,
                chars_parsed,
                state.clone(),
                Attempting::NumberLiteral,
            ))
        };

        // Returns true iff so far we have parsed the given char and no other chars.
        let so_far_parsed = |ch| chars_parsed == 1 && first_ch == ch;

        // We don't support negative escaped ints (e.g. 0x01 is supported but -0x01 is not).
        // If you want that, do something like (negate 0x01).
        //
        // I'm open to changing this policy (that is, allowing support for
        // negative escaped ints), but it'll complicate parsing logic and seems
        // nonessential, so I'm leaving it out for now.
        if next_ch == '.' {
            if typ == Float {
                // You only get one decimal point!
                return err_unexpected();
            } else {
                typ = Float;
            }
        } else if next_ch == 'x' {
            if so_far_parsed('0') {
                typ = Hex;
            } else {
                return err_unexpected();
            }
        } else if next_ch == 'b' {
            if so_far_parsed('0') {
                typ = Binary;
            } else {
                return err_unexpected();
            }
        } else if next_ch == 'o' {
            if so_far_parsed('0') {
                typ = Octal;
            } else {
                return err_unexpected();
            }
        } else if !next_ch.is_ascii_digit() && next_ch != '_' {
            if so_far_parsed('-') {
                // No digits! We likely parsed a minus sign that's actually an operator.
                return err_unexpected();
            } else {
                // We hit an invalid number literal character; we're done!
                break;
            }
        }

        chars_parsed += 1;
    }

    // At this point we have a number, and will definitely succeed.
    // If the number is malformed (outside the supported range),
    // we'll succeed with an appropriate Expr which records that.
    let expr = match typ {
        Int => Expr::Int(&state.input[0..chars_parsed]),
        Float => Expr::Float(&state.input[0..chars_parsed]),
        // For these we trim off the 0x/0o/0b part
        Hex => Expr::HexInt(&state.input[2..chars_parsed - 1]),
        Binary => Expr::BinaryInt(&state.input[2..chars_parsed - 1]),
        Octal => Expr::OctalInt(&state.input[2..chars_parsed - 1]),
    };

    let next_state = state.advance_without_indenting(chars_parsed)?;

    Ok((expr, next_state))
}

#[derive(Debug, PartialEq, Eq)]
enum LiteralType {
    Int,
    Float,
    Hex,
    Octal,
    Binary,
}
