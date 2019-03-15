use expr::Operator;
use expr::Expr;

use std::char;
use std::iter;

use combine::parser::char::{char, string, letter, alpha_num, spaces, digit, hex_digit, HexDigit};
use combine::parser::repeat::{many, count_min_max};
use combine::parser::item::{any, satisfy, satisfy_map, value};
use combine::{choice, many1, parser, Parser, optional, between, unexpected_any};
use combine::error::{Consumed, ParseError};
use combine::stream::{Stream};

pub enum Problem {
    // Number problems
    DoubleDecimalPoint, NoDigitsBeforeDecimalPoint, DoubleMinusSign
}


pub fn parse(text: &str) -> Result<Expr, Problem> {
    panic!("TODO");
}

pub fn expr<I>() -> impl Parser<Input = I, Output = Expr>
where I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>
{
    // TODO change to expr() to reproduce rust compiler bug
    expr_()
}

// This macro allows recursive parsers
parser! {
    #[inline(always)]
    fn expr_[I]()(I) -> Expr
        where [ I: Stream<Item = char> ]
    {
        choice((
            number_literal(),
            ident(),
        )).skip(spaces()).and(
            // Optionally follow the expression with an operator,
            //
            // e.g. In the expression (1 + 2), the subexpression 1
            // is followed by the operator + and another subexpression, 2
            optional(
                operator()
                    .skip(spaces())
                    .and(expr()
            )
        )).map(|(v1, maybe_op)| {
            match maybe_op {
                None => v1,
                Some((op, v2)) => {
                    Expr::CallOperator(Box::new(v1), op, Box::new(v2))
                },
            }
        })
    }
}

pub fn operator<I>() -> impl Parser<Input = I, Output = Operator>
where I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>
{
    choice((
        char('+').map(|_| Operator::Plus),
        char('-').map(|_| Operator::Minus),
        char('*').map(|_| Operator::Star),
    ))
}

pub fn ident<I>() -> impl Parser<Input = I, Output = Expr>
where I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>
{
    char('.').map(|_| Expr::Int(1))
}

pub fn string_literal<I>() -> impl Parser<Input = I, Output = Expr>
where I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>
{
    between(char('"'), char('"'), many(string_body()))
        .map(|str| Expr::String(str))
}

pub fn char_literal<I>() -> impl Parser<Input = I, Output = Expr>
where I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>
{
    between(char('\''), char('\''), char_body())
        .map(|ch| Expr::Char(ch))
}


fn unicode_code_pt<I>() -> impl Parser<Input = I, Output = char>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    // You can put up to 6 hex digits inside \u{...}
    // e.g. \u{00A0} or \u{101010}
    // They must be no more than 10FFFF
    let hex_code_pt =
        count_min_max::<Vec<char>, HexDigit<I>>(1, 6, hex_digit())
        .then(|hex_digits| {
            let hex_str:String = hex_digits.into_iter().collect();

            match u32::from_str_radix(&hex_str, 16) {
                Ok(code_pt) => {
                    match char::from_u32(code_pt) {
                        Some(ch) => value(ch).left(),
                        None => unexpected_any("Invalid Unicode code point").right()
                    }
                },
                Err(_) => {
                    unexpected_any("Invalid hex code - Unicode code points must be specified using hexadecimal characters (the numbers 0-9 and letters A-F)").right()
                }
            }
        });

    char('u').with(between(char('{'), char('}'), hex_code_pt))
}

fn string_body<I>() -> impl Parser<Input = I, Output = char>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    parser(|input: &mut I| {
        let (parsed_char, consumed) = try!(any().parse_lazy(input).into());
        let mut escaped = satisfy_map(|escaped_char| {
            // NOTE! When modifying this, revisit char_body too!
            // Their implementations are similar but not the same.
            match escaped_char {
                '"' => Some('"'),
                '\\' => Some('\\'),
                't' => Some('\t'),
                'n' => Some('\n'),
                'r' => Some('\r'),
                _ => None,
            }
        });

        match parsed_char {
            '\\' => {
                consumed.combine(|_| {
                    // Try to parse basic backslash-escaped literals
                    // e.g. \t, \n, \r
                    escaped.parse_stream(input).or_else(|_|
                        // If we didn't find any of those, try \u{...}
                        unicode_code_pt().parse_stream(input)
                    )
                })
            },
            '"' => {
                // We should never consume a quote unless
                // it's prefixed by a backslash
                Err(Consumed::Empty(I::Error::empty(input.position()).into()))
            },
            _ => Ok((parsed_char, consumed)),
        }
    })
}

fn char_body<I>() -> impl Parser<Input = I, Output = char>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    parser(|input: &mut I| {
        let (parsed_char, consumed) = try!(any().parse_lazy(input).into());
        let mut escaped = satisfy_map(|escaped_char| {
            // NOTE! When modifying this, revisit string_body too!
            // Their implementations are similar but not the same.
            match escaped_char {
                '\'' => Some('\''),
                '\\' => Some('\\'),
                't' => Some('\t'),
                'n' => Some('\n'),
                'r' => Some('\r'),
                _ => None,
            }
        });

        match parsed_char {
            '\\' => {
                consumed.combine(|_| {
                    // Try to parse basic backslash-escaped literals
                    // e.g. \t, \n, \r
                    escaped.parse_stream(input).or_else(|_|
                        // If we didn't find any of those, try \u{...}
                        unicode_code_pt().parse_stream(input)
                    )
                })
            },
            '\'' => {
                // We should never consume a single quote unless
                // it's prefixed by a backslash
                Err(Consumed::Empty(I::Error::empty(input.position()).into()))
            },
            _ => Ok((parsed_char, consumed)),
        }
    })
}

pub fn number_literal<I>() -> impl Parser<Input = I, Output = Expr>
where I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>
{
    // Digits before the decimal point can be space-separated
    // e.g. one million can be written as 1 000 000 
    let digits_before_decimal = many1::<Vec<_>, _>(digit().skip(optional(char(' '))));
    let digits_after_decimal =  many1::<Vec<_>, _>(digit());

    optional(char('-'))
        .and(digits_before_decimal)
        .and(optional(char('.').with(digits_after_decimal)))
        .map(|((maybe_minus, int_digits), decimals): ((Option<char>, Vec<char>), Option<Vec<char>>)| {
            let is_positive = maybe_minus.is_none();

            // TODO check length of digits and make sure not to overflow
            let int_str: String = int_digits.into_iter().collect();
            let int_val = int_str.parse::<i64>().unwrap();

            match decimals {
                None => {
                    if is_positive {
                        Expr::Int(int_val as i64)
                    } else {
                        Expr::Int(-int_val as i64)
                    }
                },
                Some(nums) => {
                    let decimal_str: String = nums.into_iter().collect();
                    // calculate numerator and denominator
                    // e.g. 123.45 == 12345 / 100
                    let denom = (10 as i64).pow(decimal_str.len() as u32);
                    let decimal = decimal_str.parse::<u32>().unwrap();
                    let numerator = (int_val * denom) + (decimal as i64);

                    if is_positive {
                        Expr::Ratio(numerator, denom as u64)
                    } else {
                        Expr::Ratio(-numerator, denom as u64)
                    }
                }
            }
        })
}

// pub fn parse_expr(state: &mut State) -> Result<Expr, Problem> {

//     let digits = chomp_digits(state);

//     if digits.is_empty() {
//         Err(Problem::InvalidNumber)
//     } else {
//         // TODO store these in a bigint, and handle overflow.
//         let num = digits.parse::<u32>().unwrap();

//         if decimal_point


//         Ok(Expr::Int(num))
//     }
// }

// enum Parsed {
//     Expr(Expr),
//     Malformed(Problem),
//     NotFound
// }


// #[inline]
// fn number_parser() -> {
//     let has_minus_sign = false;
//     let decimal_point_index: usize = 0;
//     let len: usize = 0;

//     for ch in state.text.chars() {
//         if ch.is_ascii_digit() {
//             len += 1;
//         } else if ch == '-' {
//             if has_minus_sign {
//                 if len == 1 {
//                     return Malformed(DoubleMinusSign);
//                 } else {
//                     // This second minus sign is a subtraction operator.
//                     // We've reached the end of the number!
//                     break;
//                 }
//             } else {
//                 has_minus_sign = true;
//                 len += 1;
//             }
//         } else if ch == '.' {
//            if len == 0 {
//                return Malformed(NoDigitsBeforeDecimalPoint);
//            } else if decimal_point_index != 0 {
//                return Malformed(DoubleDecimalPoint);
//            } else {
//                 // This might be a valid decimal number!
//                 decimal_point_index = len;

//                 len += 1;
//            }
//         }
//     }

//     state.col += len;

//     if decimal_point_index == 0 {
//         // This is an integer.
//         Expr(Expr::Int(parse_int(&state.text[..len])))
//     } else {
//         // This is a decimal.
//         let before_decimal_pt = &state.text[..decimal_point_index];
//         let after_decimal_pt = &state.text[(decimal_point_index + 1)..];

//         let numerator_str = before_decimal_pt.to_owned();
//         numerator_str.push_str(after_decimal_pt);

//         let numerator = parse_int(&numerator_str);
//         let denominator = 10 * after_decimal_pt.len() as u64;

//         Expr(Expr::Ratio(numerator, denominator))
//     }
// }

// #[inline]
// fn parse_int(text: &str) -> i64 {
//     // TODO parse as BigInt
//     text.parse::<i64>().unwrap()
// }

