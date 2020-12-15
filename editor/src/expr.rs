use crate::ast::{Expr2, ExprId, FloatVal, IntStyle, IntVal};
use crate::pool::{Pool, PoolStr, PoolVec};
use roc_can::expr::Output;
use roc_can::num::{finish_parsing_base, finish_parsing_float, finish_parsing_int};
use roc_can::scope::Scope;
use roc_module::low_level::LowLevel;
use roc_module::operator::CalledVia;
use roc_module::symbol::{IdentIds, ModuleId, ModuleIds, Symbol};
use roc_parse::ast::StrLiteral;
use roc_region::all::{Located, Region};
use roc_types::subs::{VarStore, Variable};

pub struct Env<'a> {
    var_store: VarStore,
    problems: Vec<()>,
    pool: Pool,
    module_ids: &'a ModuleIds,
}

const ZERO: Region = Region::zero();

pub fn to_expr_id<'a>(
    env: &mut Env<'a>,
    scope: &mut Scope,
    parse_expr: &'a roc_parse::ast::Expr<'a>,
) -> (crate::ast::ExprId, Output) {
    let (expr, output) = to_expr2(env, scope, parse_expr);

    (env.pool.add(expr), output)
}

pub fn to_expr2<'a>(
    env: &mut Env<'a>,
    scope: &mut Scope,
    parse_expr: &'a roc_parse::ast::Expr<'a>,
) -> (crate::ast::Expr2, Output) {
    use roc_parse::ast::Expr::*;
    match parse_expr {
        Float(string) => {
            match finish_parsing_float(string) {
                Ok(float) => {
                    let expr = Expr2::Float {
                        number: FloatVal::F64(float),
                        var: env.var_store.fresh(),
                    };

                    (expr, Output::default())
                }
                Err((_raw, _error)) => {
                    // emit runtime error
                    //                    let runtime_error = InvalidFloat(error, ZERO, raw.into());
                    //
                    //                    env.problem(Problem::RuntimeError(runtime_error.clone()));
                    //
                    //                    Expr::RuntimeError(runtime_error)
                    todo!()
                }
            }
        }
        Num(string) => {
            match finish_parsing_int(string) {
                Ok(int) => {
                    let expr = Expr2::SmallInt {
                        number: IntVal::I64(int),
                        var: env.var_store.fresh(),
                        // TODO non-hardcode
                        style: IntStyle::Decimal,
                        text: PoolStr::new(string, &mut env.pool),
                    };

                    (expr, Output::default())
                }
                Err((_raw, _error)) => {
                    // emit runtime error
                    //                    let runtime_error = InvalidFloat(error, ZERO, raw.into());
                    //
                    //                    env.problem(Problem::RuntimeError(runtime_error.clone()));
                    //
                    //                    Expr::RuntimeError(runtime_error)
                    todo!()
                }
            }
        }
        NonBase10Int {
            string,
            base,
            is_negative,
        } => {
            match finish_parsing_base(string, *base, *is_negative) {
                Ok(int) => {
                    let expr = Expr2::SmallInt {
                        number: IntVal::I64(int),
                        var: env.var_store.fresh(),
                        // TODO non-hardcode
                        style: IntStyle::from_base(*base),
                        text: PoolStr::new(string, &mut env.pool),
                    };

                    (expr, Output::default())
                }
                Err((_raw, _error)) => {
                    // emit runtime error
                    //                    let runtime_error = InvalidFloat(error, ZERO, raw.into());
                    //
                    //                    env.problem(Problem::RuntimeError(runtime_error.clone()));
                    //
                    //                    Expr::RuntimeError(runtime_error)
                    todo!()
                }
            }
        }

        Str(literal) => flatten_str_literal(env, scope, &literal),

        //    /// string literals of length up to 30B
        //    SmallStr(ArrayString<U30>), // 31B
        //    /// string literals of length 31B or more
        //    Str(PoolStr), // 8B
        _ => todo!(),
    }
}

fn flatten_str_literal<'a>(
    env: &mut Env<'a>,
    scope: &mut Scope,
    literal: &StrLiteral<'a>,
) -> (Expr2, Output) {
    use roc_parse::ast::StrLiteral::*;

    match literal {
        PlainLine(str_slice) => {
            // TODO use smallstr
            let expr = Expr2::Str(PoolStr::new(str_slice, &mut env.pool));

            (expr, Output::default())
        }
        Line(segments) => flatten_str_lines(env, scope, &[segments]),
        Block(lines) => flatten_str_lines(env, scope, lines),
    }
}

enum StrSegment {
    Interpolation(Expr2),
    Plaintext(PoolStr),
}

fn flatten_str_lines<'a>(
    env: &mut Env<'a>,
    scope: &mut Scope,
    lines: &[&[roc_parse::ast::StrSegment<'a>]],
) -> (Expr2, Output) {
    use roc_parse::ast::StrSegment::*;

    let mut buf = String::new();
    let mut segments = Vec::new();
    let mut output = Output::default();

    for line in lines {
        for segment in line.iter() {
            match segment {
                Plaintext(string) => {
                    buf.push_str(string);
                }
                Unicode(loc_hex_digits) => match u32::from_str_radix(loc_hex_digits.value, 16) {
                    Ok(code_pt) => match std::char::from_u32(code_pt) {
                        Some(ch) => {
                            buf.push(ch);
                        }
                        None => {
                            //                            env.problem(Problem::InvalidUnicodeCodePoint(loc_hex_digits.region));
                            //
                            //                            return (
                            //                                Expr::RuntimeError(RuntimeError::InvalidUnicodeCodePoint(
                            //                                    loc_hex_digits.region,
                            //                                )),
                            //                                output,
                            //                            );
                            todo!()
                        }
                    },
                    Err(_) => {
                        //                        env.problem(Problem::InvalidHexadecimal(loc_hex_digits.region));
                        //
                        //                        return (
                        //                            Expr::RuntimeError(RuntimeError::InvalidHexadecimal(
                        //                                loc_hex_digits.region,
                        //                            )),
                        //                            output,
                        //                        );
                        todo!()
                    }
                },
                Interpolated(loc_expr) => {
                    if roc_can::expr::is_valid_interpolation(loc_expr.value) {
                        // Interpolations desugar to Str.concat calls
                        output.references.calls.insert(Symbol::STR_CONCAT);

                        if !buf.is_empty() {
                            segments.push(StrSegment::Plaintext(PoolStr::new(&buf, &mut env.pool)));

                            buf = String::new();
                        }

                        let (loc_expr, new_output) = to_expr2(env, scope, loc_expr.value);

                        output.union(new_output);

                        segments.push(StrSegment::Interpolation(loc_expr));
                    } else {
                        //                        env.problem(Problem::InvalidInterpolation(loc_expr.region));
                        //
                        //                        return (
                        //                            Expr::RuntimeError(RuntimeError::InvalidInterpolation(loc_expr.region)),
                        //                            output,
                        //                        );
                        todo!()
                    }
                }
                EscapedChar(escaped) => buf.push(roc_can::expr::unescape_char(escaped)),
            }
        }
    }

    if !buf.is_empty() {
        segments.push(StrSegment::Plaintext(PoolStr::new(&buf, &mut env.pool)));
    }

    (desugar_str_segments(env, segments), output)
}

/// Resolve stirng interpolations by desugaring a sequence of StrSegments
/// into nested calls to Str.concat
fn desugar_str_segments<'a>(env: &mut Env<'a>, segments: Vec<StrSegment>) -> Expr2 {
    use StrSegment::*;

    let pool = &mut env.pool;
    let var_store = &mut env.var_store;

    let mut iter = segments.into_iter().rev();
    let mut expr = match iter.next() {
        Some(Plaintext(pool_str)) => Expr2::Str(pool_str),
        Some(Interpolation(expr_id)) => expr_id,
        None => {
            // No segments? Empty string!

            let pool_str = PoolStr::new("", pool);
            Expr2::Str(pool_str)
        }
    };

    for seg in iter {
        let new_expr = match seg {
            Plaintext(string) => Expr2::Str(string),
            Interpolation(expr_id) => expr_id,
        };

        let concat_expr_id = pool.add(Expr2::Var(Symbol::STR_CONCAT));

        let args = vec![
            (var_store.fresh(), pool.add(new_expr)),
            (var_store.fresh(), pool.add(expr)),
        ];
        let args = PoolVec::new(args.into_iter(), pool);

        let new_call = Expr2::Call {
            args,
            expr: concat_expr_id,
            expr_var: var_store.fresh(),
            fn_var: var_store.fresh(),
            closure_var: var_store.fresh(),
            called_via: CalledVia::Space,
        };

        expr = new_call
    }

    expr
}
