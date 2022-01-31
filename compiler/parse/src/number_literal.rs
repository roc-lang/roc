use crate::ast::{Base, NumericBound};
use crate::parser::{ENumber, ParseResult, Parser, Progress};
use crate::state::State;
use roc_module::numeric::{FloatWidth, IntWidth, NumWidth};

#[derive(Debug, Copy, Clone)]
pub enum NumLiteral<'a> {
    Float(&'a str, NumericBound<FloatWidth>),
    Int(&'a str, NumericBound<IntWidth>),
    Num(&'a str, NumericBound<NumWidth>),
    NonBase10Int {
        string: &'a str,
        base: Base,
        is_negative: bool,
        bound: NumericBound<IntWidth>,
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
    match bytes.get(0..2) {
        Some(b"0b") => chomp_number_base(Base::Binary, is_negated, &bytes[2..], state),
        Some(b"0o") => chomp_number_base(Base::Octal, is_negated, &bytes[2..], state),
        Some(b"0x") => chomp_number_base(Base::Hex, is_negated, &bytes[2..], state),
        _ => chomp_number_dec(is_negated, bytes, state),
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

fn get_int_suffix<'a>(bytes: &'a [u8]) -> Option<(IntWidth, usize)> {
    parse_num_suffix! {
        bytes,
        b"u8", IntWidth::U8
        b"u16", IntWidth::U16
        b"u32", IntWidth::U32
        b"u64", IntWidth::U64
        b"u128", IntWidth::U128
        b"i8", IntWidth::I8
        b"i16", IntWidth::I16
        b"i32", IntWidth::I32
        b"i64", IntWidth::I64
        b"i128", IntWidth::I128
        b"nat", IntWidth::Nat
    }
    None
}

fn get_float_suffix<'a>(bytes: &'a [u8]) -> Option<(FloatWidth, usize)> {
    parse_num_suffix! {
        bytes,
        b"dec", FloatWidth::Dec
        b"f32", FloatWidth::F32
        b"f64", FloatWidth::F64
    }
    None
}

fn get_num_suffix<'a>(bytes: &'a [u8]) -> Option<(NumWidth, usize)> {
    (get_int_suffix(bytes).map(|(iw, l)| (NumWidth::Int(iw), l)))
        .or_else(|| get_float_suffix(bytes).map(|(fw, l)| (NumWidth::Float(fw), l)))
}

fn chomp_number_base<'a>(
    base: Base,
    is_negative: bool,
    bytes: &'a [u8],
    state: State<'a>,
) -> ParseResult<'a, NumLiteral<'a>, ENumber> {
    let (_, (_is_float, bound, chomped), state) =
        chomp_number(bytes, state, is_negative, base == Base::Hex)?;

    let (bound, chomped_number) = if let Some((bound, chomped_before_suffix)) = bound {
        (Some(bound), chomped_before_suffix)
    } else {
        (None, chomped)
    };

    let string = unsafe { std::str::from_utf8_unchecked(&bytes[..chomped_number]) };

    let new = state.advance(chomped + 2 + is_negative as usize);

    match bound {
        None => Ok((
            Progress::MadeProgress,
            NumLiteral::NonBase10Int {
                is_negative,
                string,
                base,
                bound: NumericBound::None { width_variable: () },
            },
            new,
        )),
        Some(NumWidth::Int(iw)) => Ok((
            Progress::MadeProgress,
            NumLiteral::NonBase10Int {
                is_negative,
                string,
                base,
                bound: NumericBound::Exact(iw),
            },
            new,
        )),
        Some(NumWidth::Float(_)) => Err((Progress::MadeProgress, ENumber::End, state)),
    }
}

fn chomp_number_dec<'a>(
    is_negative: bool,
    bytes: &'a [u8],
    state: State<'a>,
) -> ParseResult<'a, NumLiteral<'a>, ENumber> {
    let (_, (is_float, bound, chomped), state) = chomp_number(bytes, state, is_negative, false)?;

    if !bytes.get(0).copied().unwrap_or_default().is_ascii_digit() {
        // we're probably actually looking at unary negation here
        return Err((Progress::NoProgress, ENumber::End, state));
    }

    let (bound, chomped_number) = if let Some((bound, chomped_before_suffix)) = bound {
        (Some(bound), chomped_before_suffix)
    } else {
        (None, chomped)
    };

    let string = unsafe {
        std::str::from_utf8_unchecked(&state.bytes()[0..chomped_number + is_negative as usize])
    };

    let new = state.advance(chomped + is_negative as usize);

    match (is_float, bound) {
        (true, None) => Ok((
            Progress::MadeProgress,
            NumLiteral::Float(string, NumericBound::None { width_variable: () }),
            new,
        )),
        (false, None) => Ok((
            Progress::MadeProgress,
            NumLiteral::Num(string, NumericBound::None { width_variable: () }),
            new,
        )),
        (_, Some(NumWidth::Float(fw))) => Ok((
            Progress::MadeProgress,
            NumLiteral::Float(string, NumericBound::Exact(fw)),
            new,
        )),
        (false, Some(NumWidth::Int(iw))) => Ok((
            Progress::MadeProgress,
            NumLiteral::Int(string, NumericBound::Exact(iw)),
            new,
        )),
        (true, Some(NumWidth::Int(_))) => Err((Progress::MadeProgress, ENumber::End, state)),
    }
}

fn chomp_number<'a>(
    mut bytes: &'a [u8],
    state: State<'a>,
    is_negative: bool,
    hex: bool,
) -> ParseResult<'a, (bool, Option<(NumWidth, usize)>, usize), ENumber> {
    let start_bytes_len = bytes.len();
    let mut is_float = false;
    let mut suffix_and_chomped_before = None;

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
            _ if byte.is_ascii_hexdigit() && hex => {
                bytes = &bytes[1..];
            }
            _ if byte.is_ascii_whitespace() || byte.is_ascii_punctuation() => {
                // not a valid digit; we're done
                return Ok((
                    Progress::MadeProgress,
                    (
                        is_float,
                        suffix_and_chomped_before,
                        start_bytes_len - bytes.len(),
                    ),
                    state,
                ));
            }
            _ => {
                // This might be a suffix; try that first.
                let parsed_suffix = if suffix_and_chomped_before.is_none() {
                    get_num_suffix(bytes)
                } else {
                    None
                };

                if let Some((bound, advanced_by)) = parsed_suffix {
                    suffix_and_chomped_before = Some((bound, start_bytes_len - bytes.len()));
                    bytes = &bytes[advanced_by..];
                    continue;
                }

                // Okay, this number is invalid.

                if start_bytes_len - bytes.len() == 0 && is_negative {
                    // We're probably actually looking at unary negation here. Reset the progress.
                    return Err((Progress::NoProgress, ENumber::End, state));
                }

                return Err((Progress::MadeProgress, ENumber::End, state));
            }
        }
    }

    // if the above loop exits, we must be dealing with an empty slice
    // therefore we parsed all of the bytes in the input
    Ok((
        Progress::MadeProgress,
        (is_float, suffix_and_chomped_before, start_bytes_len),
        state,
    ))
}
