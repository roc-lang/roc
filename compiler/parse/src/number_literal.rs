use crate::ast::{Attempting, Base, Expr};
use crate::parser::{parse_utf8, unexpected, unexpected_eof, ParseResult, Parser, State};
use std::char;
use std::str::from_utf8_unchecked;

pub fn number_literal<'a>() -> impl Parser<'a, Expr<'a>> {
    move |_arena, state: State<'a>| {
        let bytes = &mut state.bytes.iter();

        match bytes.next() {
            Some(&first_byte) => {
                // Number literals must start with either an '-' or a digit.
                if first_byte == '-' as u8 || (first_byte as char).is_ascii_digit() {
                    parse_number_literal(first_byte as char, bytes, state)
                } else {
                    Err(unexpected(1, state, Attempting::NumberLiteral))
                }
            }
            None => Err(unexpected_eof(0, state.attempting, state)),
        }
    }
}

#[inline(always)]
fn parse_number_literal<'a, I>(
    first_ch: char,
    bytes: &mut I,
    state: State<'a>,
) -> ParseResult<'a, Expr<'a>>
where
    I: Iterator<Item = &'a u8>,
{
    use self::LiteralType::*;

    let mut typ = Num;

    // We already parsed 1 character (which may have been a minus sign).
    let mut bytes_parsed = 1;
    let mut prev_byte = first_ch as u8;
    let mut has_parsed_digits = first_ch.is_ascii_digit();

    for &next_byte in bytes {
        let err_unexpected = || {
            Err(unexpected(
                bytes_parsed,
                state.clone(),
                Attempting::NumberLiteral,
            ))
        };

        let is_potentially_non_base10 = || {
            (bytes_parsed == 1 && first_ch == '0')
                || (bytes_parsed == 2 && first_ch == '-' && prev_byte == '0' as u8)
        };

        match next_byte as char {
            '.' => {
                if typ == Float {
                    // You only get one decimal point!
                    return err_unexpected();
                } else {
                    typ = Float;
                }
            }
            'x' => {
                if is_potentially_non_base10() {
                    typ = Hex;
                } else {
                    return err_unexpected();
                }
            }
            'b' if typ == Num => {
                // We have to check for typ == Num because otherwise we get a false
                // positive here when parsing a hex literal that happens to have
                // a 'b' in it, e.g. 0xbbbb
                if is_potentially_non_base10() {
                    typ = Binary;
                } else {
                    return err_unexpected();
                }
            }
            'o' => {
                if is_potentially_non_base10() {
                    typ = Octal;
                } else {
                    return err_unexpected();
                }
            }
            next_ch if next_ch.is_ascii_digit() => {
                has_parsed_digits = true;
            }
            next_ch
                if next_ch != '_' &&
            // ASCII alphabetic chars (like 'a' and 'f') are allowed in Hex int literals.
            // We parse them in any int literal, so we can give a more helpful error
            // in canonicalization (e.g. "the character 'f' is not allowed in Octal literals"
            // or "the character 'g' is outside the range of valid Hex literals")
            !next_ch.is_ascii_alphabetic() =>
            {
                if has_parsed_digits {
                    // We hit an invalid number literal character; we're done!
                    break;
                } else {
                    // No digits! We likely parsed a minus sign that's actually an operator.
                    return err_unexpected();
                }
            }
            _ => {}
        }

        // Since we only consume characters in the ASCII range for number literals,
        // this will always be exactly 1. There's no need to call next_ch.utf8_len().
        bytes_parsed += 1;
        prev_byte = next_byte;
    }

    // At this point we have a number, and will definitely succeed.
    // If the number is malformed (outside the supported range),
    // we'll succeed with an appropriate Expr which records that.
    match typ {
        Num => Ok((
            // SAFETY: it's safe to use from_utf8_unchecked here, because we've
            // already validated that this range contains only ASCII digits
            Expr::Num(unsafe { from_utf8_unchecked(&state.bytes[0..bytes_parsed]) }),
            state.advance_without_indenting(bytes_parsed)?,
        )),
        Float => Ok((
            // SAFETY: it's safe to use from_utf8_unchecked here, because we've
            // already validated that this range contains only ASCII digits
            Expr::Float(unsafe { from_utf8_unchecked(&state.bytes[0..bytes_parsed]) }),
            state.advance_without_indenting(bytes_parsed)?,
        )),
        // For these we trim off the 0x/0o/0b part
        Hex => from_base(Base::Hex, first_ch, bytes_parsed, state),
        Octal => from_base(Base::Octal, first_ch, bytes_parsed, state),
        Binary => from_base(Base::Binary, first_ch, bytes_parsed, state),
    }
}

#[derive(Debug, PartialEq, Eq)]
enum LiteralType {
    Num,
    Float,
    Hex,
    Octal,
    Binary,
}

fn from_base<'a>(
    base: Base,
    first_ch: char,
    bytes_parsed: usize,
    state: State<'a>,
) -> ParseResult<'a, Expr<'a>> {
    let is_negative = first_ch == '-';
    let bytes = if is_negative {
        &state.bytes[3..bytes_parsed]
    } else {
        &state.bytes[2..bytes_parsed]
    };

    match parse_utf8(bytes) {
        Ok(string) => Ok((
            Expr::NonBase10Int {
                is_negative,
                string,
                base,
            },
            state.advance_without_indenting(bytes_parsed)?,
        )),
        Err(reason) => state.fail(reason),
    }
}
