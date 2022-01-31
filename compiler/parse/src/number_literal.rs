use crate::ast::{Base, FloatWidth, NumWidth, NumericBound};
use crate::parser::{ENumber, ParseResult, Parser, Progress};
use crate::state::State;

#[derive(Debug, Copy, Clone)]
pub enum NumLiteral<'a> {
    Float(&'a str, NumericBound<FloatWidth>),
    Num(&'a str, NumericBound<NumWidth>),
    NonBase10Int {
        string: &'a str,
        base: Base,
        is_negative: bool,
        bound: NumericBound<NumWidth>,
    },
}

pub fn positive_number_literal<'a>() -> impl Parser<'a, NumLiteral<'a>, ENumber> {
    move |_arena, state: State<'a>| {
        match state.bytes().get(0) {
            Some(first_byte) if (*first_byte as char).is_ascii_digit() => {
                parse_number_base(false, state.bytes(), state)
            }
            _ => {
                // this is not a number at all
                Err((Progress::NoProgress, ENumber::End, state))
            }
        }
    }
}

pub fn number_literal<'a>() -> impl Parser<'a, NumLiteral<'a>, ENumber> {
    move |_arena, state: State<'a>| {
        match state.bytes().get(0) {
            Some(first_byte) if *first_byte == b'-' => {
                // drop the minus
                parse_number_base(true, &state.bytes()[1..], state)
            }
            Some(first_byte) if (*first_byte as char).is_ascii_digit() => {
                parse_number_base(false, state.bytes(), state)
            }
            _ => {
                // this is not a number at all
                Err((Progress::NoProgress, ENumber::End, state))
            }
        }
    }
}

fn parse_number_base<'a>(
    is_negated: bool,
    bytes: &'a [u8],
    state: State<'a>,
) -> ParseResult<'a, NumLiteral<'a>, ENumber> {
    let number = match bytes.get(0..2) {
        Some(b"0b") => chomp_number_base(Base::Binary, is_negated, &bytes[2..], state),
        Some(b"0o") => chomp_number_base(Base::Octal, is_negated, &bytes[2..], state),
        Some(b"0x") => chomp_number_base(Base::Hex, is_negated, &bytes[2..], state),
        _ => chomp_number_dec(is_negated, bytes, state),
    };
    number.and_then(|(_, literal, state)| parse_number_suffix(literal, state))
}

fn parse_number_suffix<'a>(
    literal: NumLiteral<'a>,
    state: State<'a>,
) -> ParseResult<'a, NumLiteral<'a>, ENumber> {
    match literal {
        NumLiteral::Float(s, _) => {
            let (bound, state) = match get_float_suffix(state.bytes()) {
                Some((bound, n)) => (NumericBound::Exact(bound), state.advance(n)),
                None => (NumericBound::None, state),
            };
            Ok((Progress::MadeProgress, NumLiteral::Float(s, bound), state))
        }
        NumLiteral::Num(s, _) => {
            let (bound, state) = match get_int_suffix(state.bytes()) {
                Some((bound, n)) => (NumericBound::Exact(bound), state.advance(n)),
                None => (NumericBound::None, state),
            };
            Ok((Progress::MadeProgress, NumLiteral::Num(s, bound), state))
        }
        NumLiteral::NonBase10Int {
            string,
            base,
            is_negative,
            bound: _,
        } => {
            let (bound, state) = match get_int_suffix(state.bytes()) {
                Some((bound, n)) => (NumericBound::Exact(bound), state.advance(n)),
                None => (NumericBound::None, state),
            };
            Ok((
                Progress::MadeProgress,
                NumLiteral::NonBase10Int {
                    string,
                    base,
                    is_negative,
                    bound,
                },
                state,
            ))
        }
    }
}

macro_rules! parse_num_suffix {
    ($bytes:expr, $($suffix:expr, $width:expr)*) => {
        $(
            {
                let len = $suffix.len();
                if $bytes.starts_with($suffix)
                    && {
                        let next = $bytes[len..].get(0);
                        match next { Some(c) => !(c.is_ascii_digit() || c.is_ascii_alphabetic()), None => true, }
                    }
                {
                    return Some(($width, len))
                }
            }
        )*
    }
}

fn get_int_suffix<'a>(bytes: &'a [u8]) -> Option<(NumWidth, usize)> {
    parse_num_suffix! {
        bytes,
        b"u8", NumWidth::U8
        b"u16", NumWidth::U16
        b"u32", NumWidth::U32
        b"u64", NumWidth::U64
        b"u128", NumWidth::U128
        b"i8", NumWidth::I8
        b"i16", NumWidth::I16
        b"i32", NumWidth::I32
        b"i64", NumWidth::I64
        b"i128", NumWidth::I128
        b"nat", NumWidth::Nat
        b"dec", NumWidth::Dec
    }
    None
}

fn get_float_suffix<'a>(bytes: &'a [u8]) -> Option<(FloatWidth, usize)> {
    parse_num_suffix! {
        bytes,
        b"f32", FloatWidth::F32
        b"f64", FloatWidth::F64
    }
    None
}

fn chomp_number_base<'a>(
    base: Base,
    is_negative: bool,
    bytes: &'a [u8],
    state: State<'a>,
) -> ParseResult<'a, NumLiteral<'a>, ENumber> {
    let (_is_float, chomped) = chomp_number(bytes);

    let string = unsafe { std::str::from_utf8_unchecked(&bytes[..chomped]) };

    let new = state.advance(chomped + 2 + is_negative as usize);

    Ok((
        Progress::MadeProgress,
        NumLiteral::NonBase10Int {
            is_negative,
            string,
            base,
            bound: NumericBound::None,
        },
        new,
    ))
}

fn chomp_number_dec<'a>(
    is_negative: bool,
    bytes: &'a [u8],
    state: State<'a>,
) -> ParseResult<'a, NumLiteral<'a>, ENumber> {
    let (is_float, chomped) = chomp_number(bytes);

    if is_negative && chomped == 0 {
        // we're probably actually looking at unary negation here
        return Err((Progress::NoProgress, ENumber::End, state));
    }

    if !bytes.get(0).copied().unwrap_or_default().is_ascii_digit() {
        // we're probably actually looking at unary negation here
        return Err((Progress::NoProgress, ENumber::End, state));
    }

    let string =
        unsafe { std::str::from_utf8_unchecked(&state.bytes()[0..chomped + is_negative as usize]) };

    let new = state.advance(chomped + is_negative as usize);

    Ok((
        Progress::MadeProgress,
        if is_float {
            NumLiteral::Float(string, NumericBound::None)
        } else {
            NumLiteral::Num(string, NumericBound::None)
        },
        new,
    ))
}

fn chomp_number(mut bytes: &[u8]) -> (bool, usize) {
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
            _ if byte.is_ascii_digit() => {
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
