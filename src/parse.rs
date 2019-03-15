use expr::Operator;
use expr::Expr;

use self::Problem::*;
use std::char;
use std::string;

use combine::parser::char::{char, letter, spaces, digit};
use combine::{attempt, between, choice, many1, parser, sep_by, Parser, optional};
use combine::error::{ParseError, ParseResult};
use combine::stream::{Stream, Positioned};
use combine::stream::state::State;

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
            // TODO check length of nums and build it up into an i62 if possible
            let int_str: String = int_digits.into_iter().collect();
            let int_val = int_str.parse::<i64>().unwrap();

            match decimals {
                None => {
                    if maybe_minus == None {
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

                    if maybe_minus == None {
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

