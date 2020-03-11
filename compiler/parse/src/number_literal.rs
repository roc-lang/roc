use crate::ast::{Attempting, Base, Expr};
use crate::parser::{unexpected, unexpected_eof, ParseResult, Parser, State};
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

    let mut typ = Num;

    // We already parsed 1 character (which may have been a minus sign).
    let mut bytes_parsed = 1;
    let mut prev_ch = first_ch;
    let mut has_parsed_digits = first_ch.is_ascii_digit();

    for next_ch in chars {
        let err_unexpected = || {
            Err(unexpected(
                next_ch,
                bytes_parsed,
                state.clone(),
                Attempting::NumberLiteral,
            ))
        };

        let is_potentially_non_base10 = || {
            (bytes_parsed == 1 && first_ch == '0')
                || (bytes_parsed == 2 && first_ch == '-' && prev_ch == '0')
        };

        if next_ch == '.' {
            if typ == Float {
                // You only get one decimal point!
                return err_unexpected();
            } else {
                typ = Float;
            }
        } else if next_ch == 'x' {
            if is_potentially_non_base10() {
                typ = Hex;
            } else {
                return err_unexpected();
            }
        } else if next_ch == 'b' && typ == Num {
            // We have to check for typ == Num because otherwise we get a false
            // positive here when parsing a hex literal that happens to have
            // a 'b' in it, e.g. 0xbbbb
            if is_potentially_non_base10() {
                typ = Binary;
            } else {
                return err_unexpected();
            }
        } else if next_ch == 'o' {
            if is_potentially_non_base10() {
                typ = Octal;
            } else {
                return err_unexpected();
            }
        } else if next_ch.is_ascii_digit() {
            has_parsed_digits = true;
        } else if next_ch != '_' &&
            // ASCII alphabetic chars (like 'a' and 'f') are allowed in Hex int literals.
            // We parse them in any int literal, so we can give a more helpful error
            // in canonicalization (e.g. "the character 'f' is not allowed in Octal literals"
            // or "the character 'g' is outside the range of valid Hex literals")
            !next_ch.is_ascii_alphabetic()
        {
            if has_parsed_digits {
                // We hit an invalid number literal character; we're done!
                break;
            } else {
                // No digits! We likely parsed a minus sign that's actually an operator.
                return err_unexpected();
            }
        }

        // Since we only consume characters in the ASCII range for number literals,
        // this will always be exactly 1. There's no need to call next_ch.utf8_len().
        bytes_parsed += 1;
        prev_ch = next_ch;
    }

    let from_base = |base| {
        let is_negative = first_ch == '-';
        let string = if is_negative {
            &state.input[3..bytes_parsed]
        } else {
            &state.input[2..bytes_parsed]
        };

        Expr::NonBase10Int {
            is_negative,
            string,
            base,
        }
    };

    // At this point we have a number, and will definitely succeed.
    // If the number is malformed (outside the supported range),
    // we'll succeed with an appropriate Expr which records that.
    let expr = match typ {
        Num => Expr::Num(&state.input[0..bytes_parsed]),
        Float => Expr::Float(&state.input[0..bytes_parsed]),
        // For these we trim off the 0x/0o/0b part
        Hex => from_base(Base::Hex),
        Octal => from_base(Base::Octal),
        Binary => from_base(Base::Binary),
    };

    let next_state = state.advance_without_indenting(bytes_parsed)?;

    Ok((expr, next_state))
}

#[derive(Debug, PartialEq, Eq)]
enum LiteralType {
    Num,
    Float,
    Hex,
    Octal,
    Binary,
}
