use bumpalo::Bump;
use roc_can::expr::{IntValue, Recursive};
use roc_can::num::{
    finish_parsing_base, finish_parsing_float, finish_parsing_num, ParsedNumResult,
};
use roc_can::operator::desugar_expr;
use roc_collections::all::MutSet;
use roc_module::symbol::Symbol;
use roc_parse::{ast::Expr, pattern::PatternType};
use roc_problem::can::{Problem, RuntimeError};
use roc_region::all::{Loc, Region};

use super::{expr2::Expr2, output::Output};
use crate::canonicalization::canonicalize::{
    canonicalize_fields, canonicalize_lookup, canonicalize_when_branch, CanonicalizeRecordProblem,
};
use crate::lang::core::declaration::decl_to_let;
use crate::lang::core::def::def::{canonicalize_defs, sort_can_defs};
use crate::lang::core::expr::expr2::ClosureExtra;
use crate::lang::core::pattern::to_pattern2;
use crate::lang::core::str::flatten_str_literal;
use crate::mem_pool::shallow_clone::ShallowClone;
use crate::{
    lang::{
        core::expr::expr2::{ExprId, FloatVal, IntStyle, IntVal},
        env::Env,
        scope::Scope,
    },
    mem_pool::{pool_str::PoolStr, pool_vec::PoolVec},
};

pub fn loc_expr_to_expr2<'a>(
    arena: &'a Bump,
    loc_expr: Loc<Expr<'a>>,
    env: &mut Env<'a>,
    scope: &mut Scope,
    region: Region,
) -> (Expr2, Output) {
    let desugared_loc_expr = desugar_expr(arena, arena.alloc(loc_expr));

    expr_to_expr2(env, scope, arena.alloc(desugared_loc_expr.value), region)
}

const ZERO: Region = Region::zero();

pub fn expr_to_expr2<'a>(
    env: &mut Env<'a>,
    scope: &mut Scope,
    parse_expr: &'a roc_parse::ast::Expr<'a>,
    region: Region,
) -> (Expr2, self::Output) {
    use roc_parse::ast::Expr::*;
    //dbg!("{:?}", parse_expr);

    match parse_expr {
        Float(string) => {
            match finish_parsing_float(string) {
                Ok((string_without_suffix, float, _bound)) => {
                    let expr = Expr2::Float {
                        number: FloatVal::F64(float),
                        var: env.var_store.fresh(),
                        text: PoolStr::new(string_without_suffix, env.pool),
                    };

                    (expr, Output::default())
                }
                Err((raw, error)) => {
                    // emit runtime error
                    let runtime_error = RuntimeError::InvalidFloat(error, ZERO, raw.into());

                    env.problem(Problem::RuntimeError(runtime_error));
                    //
                    //                    Expr::RuntimeError(runtime_error)
                    todo!()
                }
            }
        }
        Num(string) => {
            match finish_parsing_num(string) {
                Ok((
                    parsed,
                    ParsedNumResult::UnknownNum(int, _) | ParsedNumResult::Int(int, _),
                )) => {
                    let expr = Expr2::SmallInt {
                        number: IntVal::I64(match int {
                            IntValue::U128(_) => todo!(),
                            IntValue::I128(n) => i128::from_ne_bytes(n) as i64, // FIXME
                        }),
                        var: env.var_store.fresh(),
                        // TODO non-hardcode
                        style: IntStyle::Decimal,
                        text: PoolStr::new(parsed, env.pool),
                    };

                    (expr, Output::default())
                }
                Ok((parsed, ParsedNumResult::Float(float, _))) => {
                    let expr = Expr2::Float {
                        number: FloatVal::F64(float),
                        var: env.var_store.fresh(),
                        text: PoolStr::new(parsed, env.pool),
                    };

                    (expr, Output::default())
                }
                Err((raw, error)) => {
                    // emit runtime error
                    let runtime_error = RuntimeError::InvalidInt(
                        error,
                        roc_parse::ast::Base::Decimal,
                        ZERO,
                        raw.into(),
                    );

                    env.problem(Problem::RuntimeError(runtime_error));
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
                Ok((int, _bound)) => {
                    let expr = Expr2::SmallInt {
                        number: IntVal::I64(match int {
                            IntValue::U128(_) => todo!(),
                            IntValue::I128(n) => i128::from_ne_bytes(n) as i64, // FIXME
                        }),
                        var: env.var_store.fresh(),
                        // TODO non-hardcode
                        style: IntStyle::from_base(*base),
                        text: PoolStr::new(string, env.pool),
                    };

                    (expr, Output::default())
                }
                Err((raw, error)) => {
                    // emit runtime error
                    let runtime_error = RuntimeError::InvalidInt(error, *base, ZERO, raw.into());

                    env.problem(Problem::RuntimeError(runtime_error));
                    //
                    //                    Expr::RuntimeError(runtime_error)
                    todo!()
                }
            }
        }

        Str(literal) => flatten_str_literal(env, scope, literal),

        List(items) => {
            let mut output = Output::default();
            let output_ref = &mut output;

            let elems: PoolVec<ExprId> = PoolVec::with_capacity(items.len() as u32, env.pool);

            for (node_id, item) in elems.iter_node_ids().zip(items.iter()) {
                let (expr, sub_output) = expr_to_expr2(env, scope, &item.value, item.region);

                output_ref.union(sub_output);

                let expr_id = env.pool.add(expr);
                env.pool[node_id] = expr_id;
            }

            let expr = Expr2::List {
                elem_var: env.var_store.fresh(),
                elems,
            };

            (expr, output)
        }

        Tag(tag) => {
            // a tag without any arguments
            (
                Expr2::Tag {
                    name: PoolStr::new(tag, env.pool),
                    variant_var: env.var_store.fresh(),
                    ext_var: env.var_store.fresh(),
                    arguments: PoolVec::empty(env.pool),
                },
                Output::default(),
            )
        }

        RecordUpdate {
            fields,
            update: loc_update,
        } => {
            let (can_update, update_out) =
                expr_to_expr2(env, scope, &loc_update.value, loc_update.region);

            if let Expr2::Var(symbol) = &can_update {
                match canonicalize_fields(env, scope, fields.items) {
                    Ok((can_fields, mut output)) => {
                        output.references.union_mut(update_out.references);

                        let answer = Expr2::Update {
                            record_var: env.var_store.fresh(),
                            ext_var: env.var_store.fresh(),
                            symbol: *symbol,
                            updates: can_fields,
                        };

                        (answer, output)
                    }
                    Err(CanonicalizeRecordProblem::InvalidOptionalValue {
                        field_name: _,
                        field_region: _,
                        record_region: _,
                    }) => {
                        //                        let runtime_error = roc_problem::can::RuntimeError::InvalidOptionalValue {
                        //                            field_name,
                        //                            field_region,
                        //                            record_region,
                        //                        };
                        //
                        //                        env.problem(Problem::RuntimeError(runtime_error));

                        todo!()
                    }
                }
            } else {
                // only (optionally qualified) variables can be updated, not arbitrary expressions

                //                let error = roc_problem::can::RuntimeError::InvalidRecordUpdate {
                //                    region: can_update.region,
                //                };
                //
                //                let answer = Expr::RuntimeError(error.clone());
                //
                //                env.problems.push(Problem::RuntimeError(error));
                //
                //                (answer, Output::default())
                todo!("{:?}", &can_update)
            }
        }

        Record(fields) => {
            if fields.is_empty() {
                (Expr2::EmptyRecord, Output::default())
            } else {
                match canonicalize_fields(env, scope, fields.items) {
                    Ok((can_fields, output)) => (
                        Expr2::Record {
                            record_var: env.var_store.fresh(),
                            fields: can_fields,
                        },
                        output,
                    ),
                    Err(CanonicalizeRecordProblem::InvalidOptionalValue {
                        field_name: _,
                        field_region: _,
                        record_region: _,
                    }) => {
                        //                        let runtime_error = RuntimeError::InvalidOptionalValue {
                        //                            field_name,
                        //                            field_region,
                        //                            record_region,
                        //                        };
                        //
                        //                        env.problem(runtime_error);
                        //                        (
                        //                            Expr::RuntimeError(
                        //                            ),
                        //                            Output::default(),
                        //
                        //                        )
                        todo!()
                    }
                }
            }
        }

        RecordAccess(record_expr, field) => {
            // TODO
            let region = ZERO;
            let (record_expr_id, output) = to_expr_id(env, scope, record_expr, region);

            (
                Expr2::Access {
                    record_var: env.var_store.fresh(),
                    field_var: env.var_store.fresh(),
                    ext_var: env.var_store.fresh(),
                    expr: record_expr_id,
                    field: PoolStr::new(field, env.pool),
                },
                output,
            )
        }

        RecordAccessorFunction(field) => (
            Expr2::Accessor {
                function_var: env.var_store.fresh(),
                record_var: env.var_store.fresh(),
                ext_var: env.var_store.fresh(),
                closure_var: env.var_store.fresh(),
                field_var: env.var_store.fresh(),
                field: PoolStr::new(field, env.pool),
            },
            Output::default(),
        ),

        If(branches, final_else) => {
            let mut new_branches = Vec::with_capacity(branches.len());
            let mut output = Output::default();

            for (condition, then_branch) in branches.iter() {
                let (cond, cond_output) =
                    expr_to_expr2(env, scope, &condition.value, condition.region);

                let (then_expr, then_output) =
                    expr_to_expr2(env, scope, &then_branch.value, then_branch.region);

                output.references.union_mut(cond_output.references);
                output.references.union_mut(then_output.references);

                new_branches.push((env.pool.add(cond), env.pool.add(then_expr)));
            }

            let (else_expr, else_output) =
                expr_to_expr2(env, scope, &final_else.value, final_else.region);

            output.references.union_mut(else_output.references);

            let expr = Expr2::If {
                cond_var: env.var_store.fresh(),
                expr_var: env.var_store.fresh(),
                branches: PoolVec::new(new_branches.into_iter(), env.pool),
                final_else: env.pool.add(else_expr),
            };

            (expr, output)
        }

        When(loc_cond, branches) => {
            // Infer the condition expression's type.
            let cond_var = env.var_store.fresh();
            let (can_cond, mut output) =
                expr_to_expr2(env, scope, &loc_cond.value, loc_cond.region);

            // the condition can never be a tail-call
            output.tail_call = None;

            let can_branches = PoolVec::with_capacity(branches.len() as u32, env.pool);

            for (node_id, branch) in can_branches.iter_node_ids().zip(branches.iter()) {
                let (can_when_branch, branch_references) =
                    canonicalize_when_branch(env, scope, branch, &mut output);

                output.references.union_mut(branch_references);

                env.pool[node_id] = can_when_branch;
            }

            // A "when" with no branches is a runtime error, but it will mess things up
            // if code gen mistakenly thinks this is a tail call just because its condition
            // happened to be one. (The condition gave us our initial output value.)
            if branches.is_empty() {
                output.tail_call = None;
            }

            // Incorporate all three expressions into a combined Output value.
            let expr = Expr2::When {
                expr_var: env.var_store.fresh(),
                cond_var,
                cond: env.pool.add(can_cond),
                branches: can_branches,
            };

            (expr, output)
        }

        Closure(loc_arg_patterns, loc_body_expr) => {
            // The globally unique symbol that will refer to this closure once it gets converted
            // into a top-level procedure for code gen.
            //
            // In the Foo module, this will look something like Foo.$1 or Foo.$2.
            let (symbol, is_anonymous) = match env.closure_name_symbol {
                Some(symbol) => (symbol, false),
                None => (env.gen_unique_symbol(), true),
            };
            env.closure_name_symbol = None;

            // The body expression gets a new scope for canonicalization.
            // Shadow `scope` to make sure we don't accidentally use the original one for the
            // rest of this block, but keep the original around for later diffing.
            let original_scope = scope;
            let mut scope = original_scope.shallow_clone();
            let can_args = PoolVec::with_capacity(loc_arg_patterns.len() as u32, env.pool);
            let mut output = Output::default();

            let mut bound_by_argument_patterns = MutSet::default();

            for (node_id, loc_pattern) in can_args.iter_node_ids().zip(loc_arg_patterns.iter()) {
                let (new_output, can_arg) = to_pattern2(
                    env,
                    &mut scope,
                    roc_parse::pattern::PatternType::FunctionArg,
                    &loc_pattern.value,
                    loc_pattern.region,
                );

                bound_by_argument_patterns
                    .extend(new_output.references.bound_symbols.iter().copied());

                output.union(new_output);

                let pattern_id = env.add(can_arg, loc_pattern.region);
                env.pool[node_id] = (env.var_store.fresh(), pattern_id);
            }

            let (body_expr, new_output) =
                expr_to_expr2(env, &mut scope, &loc_body_expr.value, loc_body_expr.region);

            let mut captured_symbols: MutSet<Symbol> =
                new_output.references.lookups.iter().copied().collect();

            // filter out the closure's name itself
            captured_symbols.remove(&symbol);

            // symbols bound either in this pattern or deeper down are not captured!
            captured_symbols.retain(|s| !new_output.references.bound_symbols.contains(s));
            captured_symbols.retain(|s| !bound_by_argument_patterns.contains(s));

            // filter out top-level symbols
            // those will be globally available, and don't need to be captured
            captured_symbols.retain(|s| !env.top_level_symbols.contains(s));

            // filter out imported symbols
            // those will be globally available, and don't need to be captured
            captured_symbols.retain(|s| s.module_id() == env.home);

            // TODO any Closure that has an empty `captured_symbols` list could be excluded!

            output.union(new_output);

            // filter out aliases
            captured_symbols.retain(|s| !output.references.referenced_aliases.contains(s));

            // filter out functions that don't close over anything
            captured_symbols.retain(|s| !output.non_closures.contains(s));

            // Now that we've collected all the references, check to see if any of the args we defined
            // went unreferenced. If any did, report them as unused arguments.
            for (sub_symbol, region) in scope.symbols() {
                if !original_scope.contains_symbol(sub_symbol) {
                    if !output.references.has_lookup(sub_symbol) {
                        // The body never referenced this argument we declared. It's an unused argument!
                        env.problem(Problem::UnusedArgument(
                            symbol,
                            is_anonymous,
                            sub_symbol,
                            region,
                        ));
                    }

                    // We shouldn't ultimately count arguments as referenced locals. Otherwise,
                    // we end up with weird conclusions like the expression (\x -> x + 1)
                    // references the (nonexistent) local variable x!
                    output.references.lookups.remove(&sub_symbol);
                }
            }

            env.register_closure(symbol, output.references.clone());

            let mut captured_symbols: Vec<_> = captured_symbols
                .into_iter()
                .map(|s| (s, env.var_store.fresh()))
                .collect();

            // sort symbols, so we know the order in which they're stored in the closure record
            captured_symbols.sort();

            // store that this function doesn't capture anything. It will be promoted to a
            // top-level function, and does not need to be captured by other surrounding functions.
            if captured_symbols.is_empty() {
                output.non_closures.insert(symbol);
            }

            let captured_symbols = PoolVec::new(captured_symbols.into_iter(), env.pool);

            let extra = ClosureExtra {
                return_type: env.var_store.fresh(),     // 4B
                captured_symbols,                       // 8B
                closure_type: env.var_store.fresh(),    // 4B
                closure_ext_var: env.var_store.fresh(), // 4B
            };

            (
                Expr2::Closure {
                    function_type: env.var_store.fresh(),
                    uniq_symbol: symbol,
                    recursive: Recursive::NotRecursive,
                    args: can_args,
                    body_id: env.add(body_expr, loc_body_expr.region),
                    extra: env.pool.add(extra),
                },
                output,
            )
        }

        Apply(loc_fn, loc_args, application_style) => {
            // The expression that evaluates to the function being called, e.g. `foo` in
            // (foo) bar baz
            let fn_region = loc_fn.region;

            // Canonicalize the function expression and its arguments
            let (fn_expr, mut output) = expr_to_expr2(env, scope, &loc_fn.value, fn_region);
            // The function's return type
            let args = PoolVec::with_capacity(loc_args.len() as u32, env.pool);

            for (node_id, loc_arg) in args.iter_node_ids().zip(loc_args.iter()) {
                let (arg_expr_id, arg_out) = to_expr_id(env, scope, &loc_arg.value, loc_arg.region);

                env.pool[node_id] = (env.var_store.fresh(), arg_expr_id);

                output.references.union_mut(arg_out.references);
            }

            // Default: We're not tail-calling a symbol (by name), we're tail-calling a function value.
            output.tail_call = None;

            let expr = match fn_expr {
                Expr2::Var(ref symbol) => {
                    output.references.calls.insert(*symbol);

                    // we're tail-calling a symbol by name, check if it's the tail-callable symbol
                    output.tail_call = match &env.tailcallable_symbol {
                        Some(tc_sym) if *tc_sym == *symbol => Some(*symbol),
                        Some(_) | None => None,
                    };

                    // IDEA: Expr2::CallByName?
                    let fn_expr_id = env.add(fn_expr, fn_region);
                    Expr2::Call {
                        args,
                        expr_id: fn_expr_id,
                        expr_var: env.var_store.fresh(),
                        fn_var: env.var_store.fresh(),
                        closure_var: env.var_store.fresh(),
                        called_via: *application_style,
                    }
                }
                Expr2::RuntimeError() => {
                    // We can't call a runtime error; bail out by propagating it!
                    return (fn_expr, output);
                }
                Expr2::Tag {
                    variant_var,
                    ext_var,
                    name,
                    ..
                } => Expr2::Tag {
                    variant_var,
                    ext_var,
                    name,
                    arguments: args,
                },
                _ => {
                    // This could be something like ((if True then fn1 else fn2) arg1 arg2).
                    let fn_expr_id = env.add(fn_expr, fn_region);
                    Expr2::Call {
                        args,
                        expr_id: fn_expr_id,
                        expr_var: env.var_store.fresh(),
                        fn_var: env.var_store.fresh(),
                        closure_var: env.var_store.fresh(),
                        called_via: *application_style,
                    }
                }
            };

            (expr, output)
        }

        Defs(loc_defs, loc_ret) => {
            let (unsorted, mut scope, defs_output, symbols_introduced) = canonicalize_defs(
                env,
                Output::default(),
                scope,
                loc_defs,
                PatternType::DefExpr,
            );

            // The def as a whole is a tail call iff its return expression is a tail call.
            // Use its output as a starting point because its tail_call already has the right answer!
            let (ret_expr, mut output) =
                expr_to_expr2(env, &mut scope, &loc_ret.value, loc_ret.region);

            output
                .introduced_variables
                .union(&defs_output.introduced_variables);

            output.references.union_mut(defs_output.references);

            // Now that we've collected all the references, check to see if any of the new idents
            // we defined went unused by the return expression. If any were unused, report it.
            for (symbol, region) in symbols_introduced {
                if !output.references.has_lookup(symbol) {
                    env.problem(Problem::UnusedDef(symbol, region));
                }
            }

            let (can_defs, output) = sort_can_defs(env, unsorted, output);

            match can_defs {
                Ok(decls) => {
                    let mut expr = ret_expr;

                    for declaration in decls.into_iter().rev() {
                        expr = decl_to_let(env.pool, env.var_store, declaration, expr);
                    }

                    (expr, output)
                }
                Err(_err) => {
                    // TODO: fix this to be something from Expr2
                    // (RuntimeError(err), output)
                    todo!()
                }
            }
        }

        PrecedenceConflict { .. } => {
            //            use roc_problem::can::RuntimeError::*;
            //
            //            let problem = PrecedenceProblem::BothNonAssociative(
            //                *whole_region,
            //                binop1.clone(),
            //                binop2.clone(),
            //            );
            //
            //            env.problem(Problem::PrecedenceProblem(problem.clone()));
            //
            //            (
            //                RuntimeError(InvalidPrecedence(problem, region)),
            //                Output::default(),
            //            )
            todo!()
        }
        MalformedClosure => {
            //            use roc_problem::can::RuntimeError::*;
            //            (RuntimeError(MalformedClosure(region)), Output::default())
            todo!()
        }
        MalformedIdent(_name, _problem) => {
            //            use roc_problem::can::RuntimeError::*;
            //
            //            let problem = MalformedIdentifier((*name).into(), region);
            //            env.problem(Problem::RuntimeError(problem.clone()));
            //
            //            (RuntimeError(problem), Output::default())
            todo!()
        }
        Var {
            module_name, // module_name will only be filled if the original Roc code stated something like `5 + SomeModule.myVar`, module_name will be blank if it was `5 + myVar`
            ident,
        } => canonicalize_lookup(env, scope, module_name, ident, region),

        // Below this point, we shouln't see any of these nodes anymore because
        // operator desugaring should have removed them!
        bad_expr @ ParensAround(_) => {
            panic!(
                "A ParensAround did not get removed during operator desugaring somehow: {:#?}",
                bad_expr
            );
        }
        bad_expr @ SpaceBefore(_, _) => {
            panic!(
                "A SpaceBefore did not get removed during operator desugaring somehow: {:#?}",
                bad_expr
            );
        }
        bad_expr @ SpaceAfter(_, _) => {
            panic!(
                "A SpaceAfter did not get removed during operator desugaring somehow: {:#?}",
                bad_expr
            );
        }
        bad_expr @ BinOps { .. } => {
            panic!(
                "A binary operator chain did not get desugared somehow: {:#?}",
                bad_expr
            );
        }
        bad_expr @ UnaryOp(_, _) => {
            panic!(
                "A unary operator did not get desugared somehow: {:#?}",
                bad_expr
            );
        }

        rest => todo!("not yet implemented {:?}", rest),
    }
}

pub fn to_expr_id<'a>(
    env: &mut Env<'a>,
    scope: &mut Scope,
    parse_expr: &'a roc_parse::ast::Expr<'a>,
    region: Region,
) -> (ExprId, Output) {
    let (expr, output) = expr_to_expr2(env, scope, parse_expr, region);

    (env.add(expr, region), output)
}
