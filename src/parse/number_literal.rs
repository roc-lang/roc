use bumpalo::collections::string::String;
use bumpalo::Bump;
use parse::ast::{Attempting, Expr};
use parse::parser::{ParseResult, Parser, State};
use parse::problems::{Problem, Problems};
use region::{Loc, Region};
use std::char;
use std::iter::Peekable;

// pub fn number_literal<'a>() -> impl Parser<'a, Expr<'a>> {
//     move |arena: &'a Bump, state: State<'a>, attempting: Attempting| {
//         let mut chars = state.input.chars();

//         match chars.next() {
//             Some(first_ch) => {
//                 if first_ch == '-' {
//                     parse_number_literal(Sign::Negative, first_ch, &mut chars, arena, state)
//                 } else if first_ch.is_ascii_digit() {
//                     parse_number_literal(Sign::Positive, first_ch, &mut chars, arena, state)
//                 } else {
//                     Err((state, attempting))
//                 }
//             }
//             None => Err((state, attempting)),
//         }
//     }
// }

// // Confirm that it starts with a digit; otherwise, it's potentially an identifier!
// look_ahead(digit())
//     .with(digits_before_decimal())
//     .and(optional(char('.').with(digits_after_decimal())))
//     .then(|(int_digits, decimals): (Vec<char>, Option<Vec<char>>)| {
//         // TODO check length of digits and make sure not to overflow
//         let int_str: String = int_digits.into_iter().collect();

//         match (int_str.parse::<i64>(), decimals) {
//             (Ok(int_val), None) => value(Expr::Int(int_val as i64)).right(),
//             (Ok(int_val), Some(nums)) => {
//                 let decimal_str: String = nums.into_iter().collect();

//                 match format!("{}.{}", int_str, decimal_str).parse::<f64>() {
//                     Ok(float) => value(Expr::Float(float)).right(),
//                     Err(_) => unexpected_any(
//                         "non-digit characters after decimal point in a number literal",
//                     )
//                     .left(),
//                 }
//             }
//             (Err(_), _) => unexpected_any(
//                 "looked like a number literal but was actually malformed identifier",
//             )
//             .left(),
//         }
//     })
// }

//#[inline(always)]
//fn parse_number_literal<'a, I>(
//    sign: Sign,
//    first_ch: char,
//    chars: &'a mut I,
//    arena: &'a Bump,
//    state: State<'a>,
//) -> ParseResult<'a, Expr<'a>>
//where
//    I: Iterator<Item = char>,
//{
//    let mut digits_before_decimal = String::with_capacity_in(1, arena);
//    let mut digits_after_decimal = String::new_in(arena);

//    if sign == Sign::Positive {
//        digits_before_decimal.push(first_ch);
//    }

//    while let Some(next_ch) = chars.next() {
//        if next_ch == '_' {
//            if !digits_after_decimal.is_empty() {
//                //
//                return Err((state, Attempting::NumberLiteral));
//            }
//        } else if first_ch.is_ascii_digit() {
//            buf.push(next_output);
//        }
//    }
//    Err((state, Attempting::NumberLiteral))
//}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Sign {
    Positive,
    Negative,
}

// pub fn underscore_separated_digits<'a>() -> impl Parser<'a, Expr<'a>> {
//     move |arena: &'a Bump, state: State<'a>, attempting: Attempting| {
// {
//     // Digits before the decimal point in a numeric literal can be
//     // underscore-separated, e.g. one million can be written as 1_000_000
//     many1::<Vec<_>, _>(alpha_num().skip(optional(attempt(char('_').skip(
//         // Don't mistake keywords like `then` and `else` for
//         // space-separated digits!
//         not_followed_by(choice((string("then"), string("else"), string("when")))),
//     )))))
// }
