use roc_error_macros::internal_error;
use roc_module::{called_via::CalledVia, symbol::Symbol};
use roc_parse::ast::StrLiteral;

use crate::{
    ast_error::{ASTResult, UnexpectedASTNodeSnafu},
    lang::{
        core::expr::{
            expr2::{ArrString, ARR_STRING_CAPACITY},
            expr_to_expr2::expr_to_expr2,
        },
        env::Env,
        scope::Scope,
    },
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
            let expr = Expr2::Str(PoolStr::new(str_slice, env.pool));

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
                            segments.push(StrSegment::Plaintext(PoolStr::new(&buf, env.pool)));

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
                EscapedChar(escaped) => buf.push(escaped.unescape()),
            }
        }
    }

    if !buf.is_empty() {
        segments.push(StrSegment::Plaintext(PoolStr::new(&buf, env.pool)));
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
        MyArrString(ArrString),
        OldPoolStr(PoolStr),
        NewPoolStr(PoolStr),
    }

    let insert_either = match str_expr {
        Expr2::SmallStr(arr_string) => {
            if arr_string.len() < arr_string.capacity() {
                let mut new_bytes: [u8; ARR_STRING_CAPACITY] = Default::default();
                let arr_bytes = arr_string.as_str().as_bytes();
                new_bytes[..insert_index].copy_from_slice(&arr_bytes[..insert_index]);
                new_bytes[insert_index] = new_char as u8;
                new_bytes[insert_index + 1..arr_bytes.len() + 1]
                    .copy_from_slice(&arr_bytes[insert_index..]);

                let new_str = unsafe {
                    // all old characters have been checked on file load, new_char has been checked inside editor/src/editor/mvc/ed_update.rs
                    std::str::from_utf8_unchecked(&new_bytes[..arr_bytes.len() + 1])
                };

                let new_arr_string = match ArrString::from(new_str) {
                    Ok(arr_string) => arr_string,
                    Err(e) => {
                        internal_error!("Failed to build valid ArrayString from str: {:?}", e)
                    }
                };

                Either::MyArrString(new_arr_string)
            } else {
                let mut new_string = arr_string.as_str().to_owned();

                new_string.insert(insert_index, new_char);

                let new_pool_str = PoolStr::new(&new_string, pool);
                Either::NewPoolStr(new_pool_str)
            }
        }
        Expr2::Str(old_pool_str) => Either::OldPoolStr(*old_pool_str),
        other => UnexpectedASTNodeSnafu {
            required_node_type: "SmallStr or Str",
            encountered_node_type: format!("{other:?}"),
        }
        .fail()?,
    };

    match insert_either {
        Either::MyArrString(arr_string) => {
            pool.set(node_id, Expr2::SmallStr(arr_string));
        }
        Either::OldPoolStr(old_pool_str) => {
            let mut new_string = old_pool_str.as_str(pool).to_owned();

            new_string.insert(insert_index, new_char);

            let new_pool_str = PoolStr::new(&new_string, pool);

            pool.set(node_id, Expr2::Str(new_pool_str))
        }
        Either::NewPoolStr(new_pool_str) => pool.set(node_id, Expr2::Str(new_pool_str)),
    }

    Ok(())
}
