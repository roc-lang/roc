use roc_module::{operator::CalledVia, symbol::Symbol};
use roc_parse::ast::StrLiteral;

use crate::{
    ast_error::{ASTResult, UnexpectedASTNode},
    lang::{core::expr::expr_to_expr2::expr_to_expr2, env::Env, scope::Scope},
    mem_pool::{pool::Pool, pool_str::PoolStr, pool_vec::PoolVec},
};

use super::expr::{
    expr2::{Expr2, ExprId},
    output::Output,
};

pub(crate) fn flatten_str_literal<'a>(
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
                            //                            env.problem(Problem::InvalidUnicodeCodePt(loc_hex_digits.region));
                            //
                            //                            return (
                            //                                Expr::RuntimeError(RuntimeError::InvalidUnicodeCodePt(
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

                        let (loc_expr, new_output) =
                            expr_to_expr2(env, scope, loc_expr.value, loc_expr.region);

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

/// Resolve string interpolations by desugaring a sequence of StrSegments
/// into nested calls to Str.concat
fn desugar_str_segments(env: &mut Env, segments: Vec<StrSegment>) -> Expr2 {
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
            expr_id: concat_expr_id,
            expr_var: var_store.fresh(),
            fn_var: var_store.fresh(),
            closure_var: var_store.fresh(),
            called_via: CalledVia::Space,
        };

        expr = new_call
    }

    expr
}

pub fn update_str_expr(
    node_id: ExprId,
    new_char: char,
    insert_index: usize,
    pool: &mut Pool,
) -> ASTResult<()> {
    let str_expr = pool.get_mut(node_id);

    enum Either {
        MyString(String),
        MyPoolStr(PoolStr),
        Done,
    }

    let insert_either = match str_expr {
        Expr2::SmallStr(arr_string) => {
            // TODO make sure this works for unicode "characters"
            let insert_res = arr_string.try_insert(insert_index as u8, new_char);

            match insert_res {
                Ok(_) => Either::Done,
                _ => {
                    let mut new_string = arr_string.as_str().to_string();
                    new_string.insert(insert_index, new_char);

                    Either::MyString(new_string)
                }
            }
        }
        Expr2::Str(old_pool_str) => Either::MyPoolStr(*old_pool_str),
        other => UnexpectedASTNode {
            required_node_type: "SmallStr or Str",
            encountered_node_type: format!("{:?}", other),
        }
        .fail()?,
    };

    match insert_either {
        Either::MyString(new_string) => {
            let new_pool_str = PoolStr::new(&new_string, pool);

            pool.set(node_id, Expr2::Str(new_pool_str))
        }
        Either::MyPoolStr(old_pool_str) => {
            let mut new_string = old_pool_str.as_str(pool).to_owned();

            new_string.insert(insert_index, new_char);

            let new_pool_str = PoolStr::new(&new_string, pool);

            pool.set(node_id, Expr2::Str(new_pool_str))
        }
        Either::Done => (),
    }

    Ok(())
}
