use expr::Operator;
use expr::Expr;

use std::char;
use parse_state::{IndentablePosition};

use combine::parser::char::{char, string, spaces, digit, hex_digit, HexDigit, alpha_num};
use combine::parser::repeat::{many, count_min_max};
use combine::parser::item::{any, satisfy_map, value, position};
use combine::parser::combinator::{look_ahead, not_followed_by};
use combine::{attempt, choice, eof, many1, parser, Parser, optional, between, unexpected_any};
use combine::error::{Consumed, ParseError};
use combine::stream::{Stream, Positioned};


pub const ERR_EMPTY_CHAR: &'static str = "EMPTY_CHAR";

pub fn expr<I>() -> impl Parser<Input = I, Output = Expr>
where I: Stream<Item = char, Position = IndentablePosition>,
    I::Error: ParseError<I::Item, I::Range, I::Position>
{
    spaces().with(expr_body(0)).skip(whitespace_or_eof())
}

fn indentation<I>() -> impl Parser<Input = I, Output = i32>
where I: Stream<Item = char, Position = IndentablePosition>,
    I::Error: ParseError<I::Item, I::Range, I::Position> ,
    I: Positioned
{
    position().map(|pos: IndentablePosition| (pos.indent_col))
}

fn whitespace_or_eof<I>() -> impl Parser<Input = I, Output = ()>
where I: Stream<Item = char, Position = IndentablePosition>,
    I::Error: ParseError<I::Item, I::Range, I::Position> {
    choice((
        spaces1(),
        eof().with(value(()))
    ))
}

fn whitespace<I>() -> impl Parser<Input = I, Output = ()>
where I: Stream<Item = char, Position = IndentablePosition>,
    I::Error: ParseError<I::Item, I::Range, I::Position> {
    many::<Vec<_>, _>(choice((char(' '), char('\n')))).with(value(()))
}

fn whitespace1<I>() -> impl Parser<Input = I, Output = ()>
where I: Stream<Item = char, Position = IndentablePosition>,
    I::Error: ParseError<I::Item, I::Range, I::Position> {
    many1::<Vec<_>, _>(choice((char(' '), char('\n')))).with(value(()))
}


fn spaces1<I>() -> impl Parser<Input = I, Output = ()>
where I: Stream<Item = char, Position = IndentablePosition>,
    I::Error: ParseError<I::Item, I::Range, I::Position> {
    // TODO we immediately discard this Vec, so revise this to not use many1 (and thus
    // not allocate one in the first place) - maybe use skip_until and not_followed_by?
    many1::<Vec<_>, _>(choice((char(' '), char('\n')))).with(value(()))
}

fn indented_spaces<I>(min_indent: i32) -> impl Parser<Input = I, Output = ()>
where I: Stream<Item = char, Position = IndentablePosition>,
    I::Error: ParseError<I::Item, I::Range, I::Position> {
    many::<Vec<_>, _>(indented_space(min_indent)).with(value(()))
}

fn indented_spaces1<I>(min_indent: i32) -> impl Parser<Input = I, Output = ()>
where I: Stream<Item = char, Position = IndentablePosition>,
    I::Error: ParseError<I::Item, I::Range, I::Position> {
    // TODO we immediately discard this Vec, so revise this to not use many1 (and thus
    // not allocate one in the first place)
    many1::<Vec<_>, _>(indented_space(min_indent)).with(value(()))
}

fn indented_space<I>(min_indent: i32) -> impl Parser<Input = I, Output = char>
where I: Stream<Item = char, Position = IndentablePosition>,
    I::Error: ParseError<I::Item, I::Range, I::Position> {
    position().then(move |pos: IndentablePosition| {
        if pos.is_indenting || pos.indent_col >= min_indent {
            choice((char(' '), char('\n'))).left()
        } else {
            unexpected_any("bad indentation on let-expression").right()
        }
    })
}

// This macro allows recursive parsers
parser! {
    #[inline(always)]
    fn expr_body[I](min_indent_ref: i32)(I) -> Expr
        where [ I: Stream<Item = char, Position = IndentablePosition> ]
    {
        // TODO figure out why min_indent_ref has the type &mut i32
        let min_indent = *min_indent_ref;

        choice((
            parenthetical_expr(min_indent),
            string_literal(),
            number_literal(),
            char_literal(),
            if_expr(min_indent),
            let_expr(min_indent),
            func_or_var(min_indent),
        ))
        .and(
            // Optionally follow the expression with an operator,
            //
            // e.g. In the expression (1 + 2), the subexpression 1
            // is followed by the operator + and another subexpression, 2
            optional(
                attempt(
                    indented_spaces(min_indent)
                        .with(operator())
                        .skip(indented_spaces(min_indent))
                        .and(expr_body(min_indent))
                        .skip(indented_spaces(min_indent))
                )
            )
        ).map(|(expr1, opt_op)| {
            match opt_op {
                None => expr1,
                Some((op, expr2)) => {
                    Expr::Operator(Box::new(expr1), op, Box::new(expr2))
                },
            }
        })
    }
}

pub fn if_expr<I>(min_indent: i32) -> impl Parser<Input = I, Output = Expr>
where I: Stream<Item = char, Position = IndentablePosition>,
    I::Error: ParseError<I::Item, I::Range, I::Position>
{
    string("if").with(indented_spaces1(min_indent))
        .with(expr_body(min_indent)).skip(indented_spaces1(min_indent))
        .skip(string("then")).skip(indented_spaces1(min_indent))
        .and(expr_body(min_indent)).skip(indented_spaces1(min_indent))
        .skip(string("else")).skip(indented_spaces1(min_indent))
        .and(expr_body(min_indent))
        .map(|((conditional, then_branch), else_branch)|
            Expr::If(
                Box::new(conditional),
                Box::new(then_branch),
                Box::new(else_branch)
            )
        )
}

pub fn parenthetical_expr<I>(min_indent: i32) -> impl Parser<Input = I, Output = Expr>
where I: Stream<Item = char, Position = IndentablePosition>,
    I::Error: ParseError<I::Item, I::Range, I::Position>
{
    between(char('('), char(')'),
            indented_spaces(min_indent).with(expr_body(min_indent)).skip(indented_spaces(min_indent))
        ).and(
        // Parenthetical expressions can optionally be followed by
        // whitespace and an expr, meaning this is function application!
        optional(
            attempt(
                indented_spaces1(min_indent)
                    // Keywords like "then" and "else" are not function application!
                    .skip(not_followed_by(choice((string("then"), string("else")))))
                    .with(expr_body(min_indent))
            )
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
where I: Stream<Item = char, Position = IndentablePosition>,
    I::Error: ParseError<I::Item, I::Range, I::Position>
{
    choice((
        string("==").map(|_| Operator::Equals),
        char('+').map(|_| Operator::Plus),
        char('-').map(|_| Operator::Minus),
        char('*').map(|_| Operator::Star),
        char('/').map(|_| Operator::Slash),
    ))
}

pub fn let_expr<I>(min_indent: i32) -> impl Parser<Input = I, Output = Expr>
where I: Stream<Item = char, Position = IndentablePosition>,
    I::Error: ParseError<I::Item, I::Range, I::Position>
{
    attempt(
        ident().and(indentation()).message("malformed identifier inside declaration")
            .skip(whitespace()).message("whitespace after identifier")
            .and(
                char('=').with(indentation())
                    // If the "=" after the identifier turns out to be
                    // either "==" or "=>" then this is not a declaration!
                    .skip(not_followed_by(choice((char('='), char('>')))))
            )
        )
        .skip(whitespace())
        .then(move |((var_name, original_indent), equals_sign_indent)| {
            if original_indent < min_indent {
                unexpected_any("this declaration is outdented too far").left()
            } else if equals_sign_indent < original_indent /* `<` because '=' should be same indent or greater */ {
                unexpected_any("the = in this declaration seems outdented").left()
            } else {
                expr_body(original_indent + 1 /* declaration body must be indented relative to original decl */)
                    .skip(whitespace1())
                    .and(expr_body(original_indent).and(indentation()))
                .then(move |(var_expr, (in_expr, in_expr_indent))| {
                    if in_expr_indent != original_indent {
                        unexpected_any("the return expression was indented differently from the original declaration").left()
                    } else {
                        value(Expr::Let(var_name.to_owned(), Box::new(var_expr), Box::new(in_expr))).right()
                    }
                }).right()
            }
        })
}

pub fn func_or_var<I>(min_indent: i32) -> impl Parser<Input = I, Output = Expr>
where I: Stream<Item = char, Position = IndentablePosition>,
    I::Error: ParseError<I::Item, I::Range, I::Position>
{
    ident()
        .and(optional(
            attempt(
                indented_spaces1(min_indent)
                .skip(not_followed_by(choice((string("then"), string("else"), string("=")))))
                .with(expr_body(min_indent))
            )
        )).map(|(name, opt_arg)|
            match opt_arg {
                Some(arg) => Expr::Func(name, Box::new(arg)),
                None => Expr::Var(name),
            }
        )
}

pub fn ident<I>() -> impl Parser<Input = I, Output = String>
where I: Stream<Item = char, Position = IndentablePosition>,
    I::Error: ParseError<I::Item, I::Range, I::Position>
{
    // Identifiers must begin with a lowercase letter, but can have any
    // combination of letters or numbers afterwards.
    // No underscores, dashes, or apostrophes.
    many1::<Vec<_>, _>(alpha_num())
        .then(|chars: Vec<char>| {
            let valid_start_char = chars[0].is_lowercase();
            let ident_str:String = chars.into_iter().collect();

            if valid_start_char {
                match ident_str.as_str() {
                    "if" => unexpected_any("Reserved keyword `if`").left(),
                    "then" => unexpected_any("Reserved keyword `then`").left(),
                    _ => value(ident_str).right()
                }
            } else {
                unexpected_any("First character in an identifier that was not a lowercase letter").left()
            }
        })
}

pub fn string_literal<I>() -> impl Parser<Input = I, Output = Expr>
where I: Stream<Item = char, Position = IndentablePosition>,
    I::Error: ParseError<I::Item, I::Range, I::Position>
{
    between(char('"'), char('"'), many(string_body()))
        .map(|str| Expr::String(str))
}

pub fn char_literal<I>() -> impl Parser<Input = I, Output = Expr>
where I: Stream<Item = char, Position = IndentablePosition>,
    I::Error: ParseError<I::Item, I::Range, I::Position>
{
    between(char('\''), char('\''), char_body().expected(ERR_EMPTY_CHAR))
        .map(|ch| Expr::Char(ch))
}


fn unicode_code_pt<I>() -> impl Parser<Input = I, Output = char>
where
    I: Stream<Item = char, Position = IndentablePosition>,
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
    I: Stream<Item = char, Position = IndentablePosition>,
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
    I: Stream<Item = char, Position = IndentablePosition>,
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
where I: Stream<Item = char, Position = IndentablePosition>,
    I::Error: ParseError<I::Item, I::Range, I::Position>
{
    // We expect these to be digits, but read any alphanumeric characters
    // because it could turn out they're malformed identifiers which
    // happen to begin with a number. We'll check for that at the end.
    let digits_after_decimal =  many1::<Vec<_>, _>(alpha_num());

    // Digits before the decimal point can be space-separated
    // e.g. one million can be written as 1 000 000
    let digits_before_decimal = many1::<Vec<_>, _>(
        alpha_num().skip(optional(
                attempt(
                    char(' ').skip(
                        // Don't mistake keywords like `then` and `else` for
                        // space-separated digits!
                        not_followed_by(choice((string("then"), string("else"))))
                    )
                )
        ))
    );

    optional(char('-'))
        // Do this lookahead to decide if we should parse this as a number.
        // This matters because once we commit to parsing it as a number,
        // we may discover non-digit chars, indicating this is actually an
        // invalid identifier. (e.g. "523foo" looks like a number, but turns
        // out to be an invalid identifier on closer inspection.)
        .and(look_ahead(digit()))
        .and(digits_before_decimal)
        .and(optional(char('.').with(digits_after_decimal)))
        .then(|(((opt_minus, _), int_digits), decimals): (((Option<char>, _), Vec<char>), Option<Vec<char>>)| {
            let is_positive = opt_minus.is_none();

            // TODO check length of digits and make sure not to overflow
            let int_str: String = int_digits.into_iter().collect();

            match ( int_str.parse::<i64>(), decimals ) {
                (Ok(int_val), None) => {
                    if is_positive {
                        value(Expr::Int(int_val as i64)).right()
                    } else {
                        value(Expr::Int(-int_val as i64)).right()
                    }
                },
                (Ok(int_val), Some(nums)) => {
                    let decimal_str: String = nums.into_iter().collect();
                    // calculate numerator and denominator
                    // e.g. 123.45 == 12345 / 100
                    let denom = (10 as i64).pow(decimal_str.len() as u32);

                    match decimal_str.parse::<u32>() {
                        Ok(decimal) => {
                            let numerator = (int_val * denom) + (decimal as i64);

                            if is_positive {
                                value(Expr::Frac(numerator, denom as u64)).right()
                            } else {
                                value(Expr::Frac(-numerator, denom as u64)).right()
                            }
                        },
                        Err(_) => {
                            unexpected_any("non-digit characters after decimal point in a number literal").left()
                        }
                    }
                },
                (Err(_), _) =>
                    unexpected_any("looked like a number but was actually malformed ident").left()
            }
        })
}

