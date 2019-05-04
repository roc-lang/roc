use expr::Operator;
use expr::Expr;

use std::char;

use combine::parser::char::{char, space, spaces, digit, hex_digit, HexDigit, alpha_num};
use combine::parser::repeat::{many, count_min_max};
use combine::parser::item::{any, satisfy_map, value};
use combine::{choice, many1, parser, Parser, optional, between, unexpected_any};
use combine::error::{Consumed, ParseError};
use combine::stream::{Stream};


pub const ERR_EMPTY_CHAR: &'static str = "EMPTY_CHAR";

pub fn expr<I>() -> impl Parser<Input = I, Output = Expr>
where I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>
{
    expr_body()
}

// This macro allows recursive parsers
parser! {
    #[inline(always)]
    fn expr_body[I]()(I) -> Expr
        where [ I: Stream<Item = char> ]
    {
        choice((
            parenthetical_expr(),
            string_literal(),
            number_literal(),
            char_literal(),
            func_or_var(),
        )).skip(spaces()).and(
            // Optionally follow the expression with an operator,
            //
            // e.g. In the expression (1 + 2), the subexpression 1
            // is followed by the operator + and another subexpression, 2
            optional(
                operator()
                    .skip(spaces())
                    .and(expr())
            )
        ).skip(spaces()).map(|(v1, opt_op)| {
            match opt_op {
                None => v1,
                Some((op, v2)) => {
                    Expr::Operator(Box::new(v1), op, Box::new(v2))
                },
            }
        })
    }
}

pub fn parenthetical_expr<I>() -> impl Parser<Input = I, Output = Expr>
where I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>
{
    between(char('('), char(')'), 
            spaces().with(expr()).skip(spaces())
        ).and(
        // Parenthetical expressions can optionally be followed by
        // whitespace and an expr, meaning this is function application!
        optional(
            many1::<Vec<_>, _>(space())
                .with(expr())
        )
    ).map(|(expr1, opt_expr2)|
        match opt_expr2 {
            None => expr1,
            Some(expr2) => {
                Expr::Apply(Box::new(expr1), Box::new(expr2))
            },
        }
    )
}

pub fn operator<I>() -> impl Parser<Input = I, Output = Operator>
where I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>
{
    choice((
        char('+').map(|_| Operator::Plus),
        char('-').map(|_| Operator::Minus),
        char('*').map(|_| Operator::Star),
        char('/').map(|_| Operator::Slash),
    ))
}

pub fn func_or_var<I>() -> impl Parser<Input = I, Output = Expr>
where I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>
{
    ident()
        .and(optional(
            many1::<Vec<_>, _>(space())
                .with(expr())
        )).map(|pair|
            match pair {
                ( Ok(str), Some(arg) ) => Expr::Func(str, Box::new(arg)),
                ( Ok(str), None ) => Expr::Var(str),
                ( Err(_ident_problem), _ ) => Expr::SyntaxProblem("TODO put _ident_problem here".to_owned())
            }
        )
}

pub enum IdentProblem {
    InvalidFirstChar(String),
    ReservedKeyword(String),
}

pub fn ident<I>() -> impl Parser<Input = I, Output = Result<String, IdentProblem>>
where I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>
{
    // Identifiers must begin with a lowercase letter, but can have any
    // combination of letters or numbers afterwards.
    // No underscores, dashes, or apostrophes.
    many::<Vec<_>, _>(alpha_num())
        .map(|chars: Vec<char>| {
            let valid_start_char = chars[0].is_lowercase();
            let ident_str:String = chars.into_iter().collect();

             if valid_start_char {
                 if ident_str == "if" {
                    Err(IdentProblem::ReservedKeyword(ident_str.to_owned()))
                 } else {
                    Ok(ident_str)
                 }
             } else {
                Err(IdentProblem::ReservedKeyword(ident_str.to_owned()))
             }
        })
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
    between(char('\''), char('\''), char_body().expected(ERR_EMPTY_CHAR))
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
                    if code_pt > 0x10FFFF {
                        unexpected_any("Invalid Unicode code point. It must be no more than \\u{10FFFF}.").right()
                    } else {
                        match char::from_u32(code_pt) {
                            Some(ch) => value(ch).left(),
                            None => unexpected_any("Invalid Unicode code point.").right()
                        }
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
                // We should never consume a double quote unless
                // it's preceded by a backslash
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
                // it's preceded by a backslash
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
        .map(|((opt_minus, int_digits), decimals): ((Option<char>, Vec<char>), Option<Vec<char>>)| {
            let is_positive = opt_minus.is_none();

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
                        Expr::Frac(numerator, denom as u64)
                    } else {
                        Expr::Frac(-numerator, denom as u64)
                    }
                }
            }
        })
}

