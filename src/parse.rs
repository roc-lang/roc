use expr::Expr;

use self::Parsed::*;
use self::Problem::*;
use std::char;
use std::string;

pub enum Problem {
    // Number problems
    DoubleDecimalPoint, NoDigitsBeforeDecimalPoint, DoubleMinusSign
}


pub struct State<'a> {
    row: usize,
    col: usize,
    text: &'a str
}

pub fn parse(text: &str) -> Result<Expr, Problem> {
    let mut state = State { row: 0, col: 0, text };

    parse_expr(&mut state)
}

pub fn parse_expr(state: &mut State) -> Result<Expr, Problem> {

    let digits = chomp_digits(state);

    if digits.is_empty() {
        Err(Problem::InvalidNumber)
    } else {
        // TODO store these in a bigint, and handle overflow.
        let num = digits.parse::<u32>().unwrap();

        if decimal_point


        Ok(Expr::Int(num))
    }
}

enum Parsed {
    Expr(Expr), Malformed(Problem), NotFound
}

fn parse_number<'a>(state: &'a mut State) -> Parsed {
    let has_minus_sign = false;
    let decimal_point_index: usize = 0;
    let len: usize = 0;

    for ch in state.text.chars() {
        if ch.is_ascii_digit() {
            len += 1;
        } else if ch == '-' {
            if has_minus_sign {
                if len == 1 {
                    return Malformed(DoubleMinusSign);
                } else {
                    // This second minus sign is a subtraction operator.
                    // We've reached the end of the number!
                    break;
                }
            } else {
                has_minus_sign = true;
                len += 1;
            }
        } else if ch == '.' {
           if len == 0 {
               return Malformed(NoDigitsBeforeDecimalPoint);
           } else if decimal_point_index != 0 {
               return Malformed(DoubleDecimalPoint);
           } else {
                // This might be a valid decimal number!
                decimal_point_index = len;

                len += 1;
           }
        }
    }

    state.col += len;

    if decimal_point_index == 0 {
        // This is an integer.
        Expr(Expr::Int(parse_int(&state.text[..len])))
    } else {
        // This is a decimal.
        let before_decimal_pt = &state.text[..decimal_point_index];
        let after_decimal_pt = &state.text[(decimal_point_index + 1)..];

        let numerator_str = before_decimal_pt.to_owned();
        numerator_str.push_str(after_decimal_pt);

        let numerator = parse_int(&numerator_str);
        let denominator = 10 * after_decimal_pt.len() as u64;

        Expr(Expr::Ratio(numerator, denominator))
    }
}

#[inline]
fn parse_int(text: &str) -> i64 {
    // TODO parse as BigInt
    text.parse::<i64>().unwrap()
}

