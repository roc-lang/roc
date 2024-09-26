use roc_can::expr::Expr;
use roc_types::subs::Subs;
use std::collections::VecDeque;

use roc_can::expr::{Expr, WhenBranch, ClosureData, Field, FunctionDef};
use roc_types::subs::Subs;
use std::collections::VecDeque;
use roc_region::all::{Loc, Region};

pub fn specialize_expr(expr: Expr, subs: &Subs) -> Expr {
    let mut stack = VecDeque::new();
    stack.push_back(expr);

    let mut result_stack = VecDeque::new();

    while let Some(current_expr) = stack.pop_back() {
        let specialized = match current_expr {
            Expr::When {
                mut loc_cond,
                cond_var,
                expr_var,
                region,
                mut branches,
                branches_cond_var,
                exhaustive,
            } => {
                stack.push_back(*loc_cond.value);
                for branch in branches.iter_mut() {
                    stack.push_back(branch.value.value.clone());
                    if let Some(guard) = &branch.guard {
                        stack.push_back(guard.value.clone());
                    }
                }

                let specialized_cond = result_stack.pop_back().unwrap();
                let specialized_branches: Vec<WhenBranch> = branches
                    .into_iter()
                    .rev()
                    .map(|mut branch| {
                        let specialized_value = result_stack.pop_back().unwrap();
                        let specialized_guard = branch.guard.map(|_| result_stack.pop_back().unwrap());
                        WhenBranch {
                            patterns: branch.patterns,
                            value: Loc::at(branch.value.region, specialized_value),
                            guard: specialized_guard.map(|g| Loc::at(branch.value.region, g)),
                            redundant: branch.redundant,
                        }
                    })
                    .collect();

                Expr::When {
                    loc_cond: Box::new(Loc::at(loc_cond.region, specialized_cond)),
                    cond_var: subs.specialize_var(cond_var),
                    expr_var: subs.specialize_var(expr_var),
                    region,
                    branches: specialized_branches,
                    branches_cond_var: subs.specialize_var(branches_cond_var),
                    exhaustive,
                }
            },
            Expr::If {
                cond_var,
                branch_var,
                mut branches,
                mut final_else,
            } => {
                for (cond, body) in branches.iter_mut() {
                    stack.push_back(cond.value.clone());
                    stack.push_back(body.value.clone());
                }
                stack.push_back(final_else.value.clone());

                let specialized_final_else = result_stack.pop_back().unwrap();
                let specialized_branches: Vec<(Loc<Expr>, Loc<Expr>)> = branches
                    .into_iter()
                    .rev()
                    .map(|(cond, body)| {
                        let specialized_body = result_stack.pop_back().unwrap();
                        let specialized_cond = result_stack.pop_back().unwrap();
                        (
                            Loc::at(cond.region, specialized_cond),
                            Loc::at(body.region, specialized_body),
                        )
                    })
                    .collect();

                Expr::If {
                    cond_var: subs.specialize_var(cond_var),
                    branch_var: subs.specialize_var(branch_var),
                    branches: specialized_branches,
                    final_else: Box::new(Loc::at(final_else.region, specialized_final_else)),
                }
            },
            Expr::LetRec(mut defs, mut body, illegal_cycle_mark) => {
                for def in defs.iter_mut() {
                    stack.push_back(def.loc_expr.value.clone());
                }
                stack.push_back(body.value.clone());

                let specialized_body = result_stack.pop_back().unwrap();
                let specialized_defs: Vec<Def> = defs
                    .into_iter()
                    .rev()
                    .map(|mut def| {
                        let specialized_expr = result_stack.pop_back().unwrap();
                        Def {
                            loc_pattern: def.loc_pattern,
                            loc_expr: Loc::at(def.loc_expr.region, specialized_expr),
                            expr_var: subs.specialize_var(def.expr_var),
                            pattern_vars: def.pattern_vars.into_iter().map(|(k, v)| (k, subs.specialize_var(v))).collect(),
                            annotation: def.annotation.map(|a| a.specialize(subs)),
                        }
                    })
                    .collect();

                Expr::LetRec(
                    specialized_defs,
                    Box::new(Loc::at(body.region, specialized_body)),
                    illegal_cycle_mark,
                )
            },
            Expr::LetNonRec(mut def, mut body) => {
                stack.push_back(def.loc_expr.value.clone());
                stack.push_back(body.value.clone());

                let specialized_body = result_stack.pop_back().unwrap();
                let specialized_expr = result_stack.pop_back().unwrap();

                Expr::LetNonRec(
                    Box::new(Def {
                        loc_pattern: def.loc_pattern,
                        loc_expr: Loc::at(def.loc_expr.region, specialized_expr),
                        expr_var: subs.specialize_var(def.expr_var),
                        pattern_vars: def.pattern_vars.into_iter().map(|(k, v)| (k, subs.specialize_var(v))).collect(),
                        annotation: def.annotation.map(|a| a.specialize(subs)),
                    }),
                    Box::new(Loc::at(body.region, specialized_body)),
                )
            },
            Expr::Call(mut boxed_tuple, mut args, called_via) => {
                stack.push_back(boxed_tuple.1.value.clone());
                for (_, arg) in args.iter_mut() {
                    stack.push_back(arg.value.clone());
                }

                let specialized_args: Vec<(Variable, Loc<Expr>)> = args
                    .into_iter()
                    .rev()
                    .map(|(var, loc_expr)| {
                        let specialized_expr = result_stack.pop_back().unwrap();
                        (subs.specialize_var(var), Loc::at(loc_expr.region, specialized_expr))
                    })
                    .collect();

                let specialized_fn = result_stack.pop_back().unwrap();
                let (fn_var, _, closure_var, expr_var) = *boxed_tuple;

                Expr::Call(
                    Box::new((
                        subs.specialize_var(fn_var),
                        Loc::at(boxed_tuple.1.region, specialized_fn),
                        subs.specialize_var(closure_var),
                        subs.specialize_var(expr_var),
                    )),
                    specialized_args,
                    called_via,
                )
            },
            Expr::Closure(mut closure_data) => {
                stack.push_back(closure_data.loc_body.value.clone());

                let specialized_body = result_stack.pop_back().unwrap();

                Expr::Closure(ClosureData {
                    function_type: subs.specialize_var(closure_data.function_type),
                    closure_type: subs.specialize_var(closure_data.closure_type),
                    return_type: subs.specialize_var(closure_data.return_type),
                    name: closure_data.name,
                    captured_symbols: closure_data.captured_symbols
                        .into_iter()
                        .map(|(s, v)| (s, subs.specialize_var(v)))
                        .collect(),
                    recursive: closure_data.recursive,
                    arguments: closure_data.arguments
                        .into_iter()
                        .map(|(v, am, p)| (subs.specialize_var(v), am, p))
                        .collect(),
                    loc_body: Box::new(Loc::at(closure_data.loc_body.region, specialized_body)),
                })
            },
            Expr::Record { record_var, mut fields } => {
                for (_, field) in fields.iter_mut() {
                    stack.push_back(field.loc_expr.value.clone());
                }

                let specialized_fields: SendMap<Lowercase, Field> = fields
                    .into_iter()
                    .map(|(label, mut field)| {
                        let specialized_expr = result_stack.pop_back().unwrap();
                        (label, Field {
                            var: subs.specialize_var(field.var),
                            region: field.region,
                            loc_expr: Box::new(Loc::at(field.loc_expr.region, specialized_expr)),
                        })
                    })
                    .collect();

                Expr::Record {
                    record_var: subs.specialize_var(record_var),
                    fields: specialized_fields,
                }
            },
            Expr::Tuple { tuple_var, mut elems } => {
                for (_, elem) in elems.iter_mut() {
                    stack.push_back(elem.value.clone());
                }

                let specialized_elems: Vec<(Variable, Box<Loc<Expr>>)> = elems
                    .into_iter()
                    .rev()
                    .map(|(var, elem)| {
                        let specialized_expr = result_stack.pop_back().unwrap();
                        (subs.specialize_var(var), Box::new(Loc::at(elem.region, specialized_expr)))
                    })
                    .collect();

                Expr::Tuple {
                    tuple_var: subs.specialize_var(tuple_var),
                    elems: specialized_elems,
                }
            },
            Expr::RecordAccess { record_var, ext_var, field_var, mut loc_expr, field } => {
                stack.push_back(loc_expr.value.clone());

                let specialized_expr = result_stack.pop_back().unwrap();

                Expr::RecordAccess {
                    record_var: subs.specialize_var(record_var),
                    ext_var: subs.specialize_var(ext_var),
                    field_var: subs.specialize_var(field_var),
                    loc_expr: Box::new(Loc::at(loc_expr.region, specialized_expr)),
                    field,
                }
            },
            Expr::TupleAccess { tuple_var, ext_var, elem_var, mut loc_expr, index } => {
                stack.push_back(loc_expr.value.clone());

                let specialized_expr = result_stack.pop_back().unwrap();

                Expr::TupleAccess {
                    tuple_var: subs.specialize_var(tuple_var),
                    ext_var: subs.specialize_var(ext_var),
                    elem_var: subs.specialize_var(elem_var),
                    loc_expr: Box::new(Loc::at(loc_expr.region, specialized_expr)),
                    index,
                }
            },
            Expr::Tag { tag_union_var, ext_var, name, mut arguments } => {
                for (_, arg) in arguments.iter_mut() {
                    stack.push_back(arg.value.clone());
                }

                let specialized_arguments: Vec<(Variable, Loc<Expr>)> = arguments
                    .into_iter()
                    .rev()
                    .map(|(var, loc_expr)| {
                        let specialized_expr = result_stack.pop_back().unwrap();
                        (subs.specialize_var(var), Loc::at(loc_expr.region, specialized_expr))
                    })
                    .collect();

                Expr::Tag {
                    tag_union_var: subs.specialize_var(tag_union_var),
                    ext_var: subs.specialize_var(ext_var),
                    name,
                    arguments: specialized_arguments,
                }
            },
            Expr::OpaqueRef { opaque_var, name, mut argument, specialized_def_type, type_arguments, lambda_set_variables } => {
                stack.push_back(argument.1.value.clone());

                let specialized_arg_expr = result_stack.pop_back().unwrap();

                Expr::OpaqueRef {
                    opaque_var: subs.specialize_var(opaque_var),
                    name,
                    argument: Box::new((subs.specialize_var(argument.0), Loc::at(argument.1.region, specialized_arg_expr))),
                    specialized_def_type: Box::new(subs.specialize_type(*specialized_def_type)),
                    type_arguments: type_arguments.into_iter().map(|v| subs.specialize_optable_var(v)).collect(),
                    lambda_set_variables,
                }
            },
            Expr::Expect { mut loc_condition, mut loc_continuation, lookups_in_cond } => {
                stack.push_back(loc_condition.value.clone());
                stack.push_back(loc_continuation.value.clone());

                let specialized_continuation = result_stack.pop_back().unwrap();
                let specialized_condition = result_stack.pop_back().unwrap();

                Expr::Expect {
                    loc_condition: Box::new(Loc::at(loc_condition.region, specialized_condition)),
                    loc_continuation: Box::new(Loc::at(loc_continuation.region, specialized_continuation)),
                    lookups_in_cond: lookups_in_cond.into_iter().map(|l| ExpectLookup {
                        symbol: l.symbol,
                        var: subs.specialize_var(l.var),
                        ability_info: l.ability_info,
                    }).collect(),
                }
            },
            Expr::ExpectFx { mut loc_condition, mut loc_continuation, lookups_in_cond } => {
                stack.push_back(loc_condition.value.clone());
                stack.push_back(loc_continuation.value.clone());

                let specialized_continuation = result_stack.pop_back().unwrap();
                let specialized_condition = result_stack.pop_back().unwrap();

                Expr::ExpectFx {
                    loc_condition: Box::new(Loc::at(loc_condition.region, specialized_condition)),
                    loc_continuation: Box::new(Loc::at(loc_continuation.region, specialized_continuation)),
                    lookups_in_cond: lookups_in_cond.into_iter().map(|l| ExpectLookup {
                        symbol: l.symbol,
                        var: subs.specialize_var(l.var),
                        ability_info: l.ability_info,
                    }).collect(),
                }
            },
            Expr::Dbg { source_location, source, mut loc_message, mut loc_continuation, variable, symbol } => {
                stack.push_back(loc_message.value.clone());
                stack.push_back(loc_continuation.value.clone());

                let specialized_continuation = result_stack.pop_back().unwrap();
                let specialized_message = result_stack.pop_back().unwrap();

                Expr::Dbg {
                    source_location,
                    source,
                    loc_message: Box::new(Loc::at(loc_message.region, specialized_message)),
                    loc_continuation: Box::new(Loc::at(loc_continuation.region, specialized_continuation)),
                    variable: subs.specialize_var(variable),
                    symbol,
                }
            },
            // For other expression types that don't contain nested expressions,
            // implement the specialization logic directly here
            Expr::Num(var, str, int_value, num_bound) =>
                Expr::Num(subs.specialize_var(var), str, int_value, num_bound),
            Expr::Int(var1, var2, str, int_value, int_bound) =>
                Expr::Int(subs.specialize_var(var1), subs.specialize_var(var2), str, int_value, int_bound),
            Expr::Float(var1, var2, str, f64, float_bound) =>
                Expr::Float(subs.specialize_var(var1), subs.specialize_var(var2), str, f64, float_bound),
            Expr::Str(str) => Expr::Str(str),
            Expr::SingleQuote(var1, var2, ch, single_quote_bound) =>
                Expr::SingleQuote(subs.specialize_var(var1), subs.specialize_var(var2), ch, single_quote_bound),
            Expr::IngestedFile(path_buf, arc, var) =>
                Expr::IngestedFile(path_buf, arc, subs.specialize_var(var)),
            Expr::Var(symbol, var) =>
                Expr::Var(symbol, subs.specialize_var(var)),
            Expr::ParamsVar { symbol, var, params_symbol, params_var } =>
                Expr::ParamsVar {
                    symbol,
                    var: subs.specialize_var(var),
                    params_symbol,
                    params_var: subs.specialize_var(params_var)
                },
            Expr::AbilityMember(symbol, specialization_i(struct_accessor_data) =>
                Expr::RecordAccessor(StructAccessorData {
                    name: struct_accessor_data.name,
                    function_var: subs.specialize_var(struct_accessor_data.function_var),
                    record_var: subs.specialize_var(struct_accessor_data.record_var),
                    closure_var: subs.specialize_var(struct_accessor_data.closure_var),
                    ext_var: subs.specialize_var(struct_accessor_data.ext_var),
                    field_var: subs.specialize_var(struct_accessor_data.field_var),
                    field: struct_accessor_data.field,
                }),
            Expr::RecordUpdate { record_var, ext_var, symbol, updates } =>
                Expr::RecordUpdate {
                    record_var: subs.specialize_var(record_var),
                    ext_var: subs.specialize_var(ext_var),
                    symbol,
                    updates: updates.into_iter().map(|(k, v)| (k, Field {
                        var: subs.specialize_var(v.var),
                        region: v.region,
                        loc_expr: Box::new(Loc::at(v.loc_expr.region, specialize_expr(v.loc_expr.value, subs))),
                    })).collect(),
                },
            Expr::ZeroArgumentTag { closure_name, variant_var, ext_var, name } =>
                Expr::ZeroArgumentTag {
                    closure_name,
                    variant_var: subs.specialize_var(variant_var),
                    ext_var: subs.specialize_var(ext_var),
                    name,
                },
            Expr::OpaqueWrapFunction(opaque_wrap_function_data) =>
                Expr::OpaqueWrapFunction(OpaqueWrapFunctionData {
                    opaque_name: opaque_wrap_function_data.opaque_name,
                    opaque_var: subs.specialize_var(opaque_wrap_function_data.opaque_var),
                    specialized_def_type: subs.specialize_type(opaque_wrap_function_data.specialized_def_type),
                    type_arguments: opaque_wrap_function_data.type_arguments.into_iter().map(|v| subs.specialize_optable_var(v)).collect(),
                    lambda_set_variables: opaque_wrap_function_data.lambda_set_variables,
                    function_name: opaque_wrap_function_data.function_name,
                    function_var: subs.specialize_var(opaque_wrap_function_data.function_var),
                    argument_var: subs.specialize_var(opaque_wrap_function_data.argument_var),
                    closure_var: subs.specialize_var(opaque_wrap_function_data.closure_var),
                }),
            Expr::TypedHole(var) =>
                Expr::TypedHole(subs.specialize_var(var)),
            Expr::RuntimeError(runtime_error) =>
                Expr::RuntimeError(runtime_error),
        };
        result_stack.push_back(specialized);
    }

    result_stack.pop_back().unwrap()
}
