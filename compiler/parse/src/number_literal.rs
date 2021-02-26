use crate::ast::{Base, Expr};
use crate::parser::{parse_utf8, Number, ParseResult, Parser, Progress, State, SyntaxError};
use std::char;
use std::str::from_utf8_unchecked;

pub fn number_literal<'a>() -> impl Parser<'a, Expr<'a>, Number> {
    move |_arena, state: State<'a>| {
        match state.bytes.get(0) {
            Some(first_byte) if *first_byte == b'-' => {
                // drop the minus
                parse_number_base(true, &state.bytes[1..], state)
            }
            Some(first_byte) if (*first_byte as char).is_ascii_digit() => {
                parse_number_base(false, &state.bytes, state)
            }
            _ => {
                // this is not a number at all
                Err((Progress::NoProgress, Number::End, state))
            }
        }
    }
}

fn parse_number_base<'a>(
    is_negated: bool,
    bytes: &'a [u8],
    state: State<'a>,
) -> ParseResult<'a, Expr<'a>, Number> {
    match bytes.get(0..2) {
        Some(b"0b") => chomp_number_base(Base::Binary, is_negated, &bytes[2..], state),
        Some(b"0o") => chomp_number_base(Base::Octal, is_negated, &bytes[2..], state),
        Some(b"0x") => chomp_number_base(Base::Hex, is_negated, &bytes[2..], state),
        _ => chomp_number_dec(is_negated, bytes, state),
    }
}

fn chomp_number_base<'a>(
    base: Base,
    is_negative: bool,
    bytes: &'a [u8],
    state: State<'a>,
) -> ParseResult<'a, Expr<'a>, Number> {
    let (_is_float, chomped) = chomp_number(bytes);

    match parse_utf8(&bytes[0..chomped]) {
        Ok(string) => match state.advance_without_indenting(chomped + 2 + is_negative as usize) {
            Ok(new) => {
                // all is well
                Ok((
                    Progress::MadeProgress,
                    Expr::NonBase10Int {
                        is_negative,
                        string,
                        base,
                    },
                    new,
                ))
            }
            Err((_, SyntaxError::LineTooLong(_), new)) => {
                // the only error we care about in this context
                Err((Progress::MadeProgress, Number::LineTooLong, new))
            }
            Err(_) => unreachable!("we know advancing will succeed if there is space on the line"),
        },

        Err(_) => unreachable!("no invalid utf8 could have been chomped"),
    }
}

fn chomp_number_dec<'a>(
    is_negative: bool,
    bytes: &'a [u8],
    state: State<'a>,
) -> ParseResult<'a, Expr<'a>, Number> {
    let (is_float, chomped) = chomp_number(bytes);

    if is_negative && chomped == 0 {
        // we're probably actually looking at unary negation here
        return Err((Progress::NoProgress, Number::End, state));
    }

    if !bytes.get(0).copied().unwrap_or_default().is_ascii_digit() {
        // we're probably actually looking at unary negation here
        return Err((Progress::NoProgress, Number::End, state));
    }

    let string = unsafe { from_utf8_unchecked(&state.bytes[0..chomped + is_negative as usize]) };

    match state.advance_without_indenting(chomped + is_negative as usize) {
        Ok(new) => {
            // all is well
            Ok((
                Progress::MadeProgress,
                if is_float {
                    Expr::Float(string)
                } else {
                    Expr::Num(string)
                },
                new,
            ))
        }
        Err((_, SyntaxError::LineTooLong(_), new)) => {
            // the only error we care about in this context
            Err((Progress::MadeProgress, Number::LineTooLong, new))
        }
        Err(_) => unreachable!("we know advancing will succeed if there is space on the line"),
    }
}

fn chomp_number<'a>(mut bytes: &'a [u8]) -> (bool, usize) {
    let start_bytes_len = bytes.len();
    let mut is_float = false;

    while let Some(byte) = bytes.get(0) {
        match byte {
            b'.' => {
                // skip, fix multiple `.`s in canonicalization
                is_float = true;
                bytes = &bytes[1..];
            }
            b'e' => {
                // maybe scientific notation?
                match bytes.get(1) {
                    Some(b'-') => {
                        is_float = true;
                        bytes = &bytes[2..];
                    }
                    Some(c) if (*c as char).is_ascii_digit() => {
                        is_float = true;
                        bytes = &bytes[2..];
                    }
                    _ => {
                        bytes = &bytes[1..];
                    }
                }
            }
            b'_' => {
                // skip
                bytes = &bytes[1..];
            }
            _ if byte.is_ascii_digit() || byte.is_ascii_alphabetic() => {
                // valid digits (alphabetic in hex digits, and the `e` in `12e26` scientific notation
                bytes = &bytes[1..];
            }
            _ => {
                // not a valid digit; we're done
                return (is_float, start_bytes_len - bytes.len());
            }
        }
    }

    // if the above loop exits, we must be dealing with an empty slice
    // therefore we parsed all of the bytes in the input
    (is_float, start_bytes_len)
}
