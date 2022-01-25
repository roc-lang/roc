use roc_collections::all::MutMap;
use roc_problem::can::Problem;
use roc_region::all::{Loc, Region};
use roc_types::subs::Variable;

use crate::{
    lang::{
        core::{
            def::def::References,
            expr::{
                expr2::{Expr2, ExprId, WhenBranch},
                expr_to_expr2::expr_to_expr2,
                output::Output,
                record_field::RecordField,
            },
            pattern::to_pattern2,
        },
        env::Env,
        scope::Scope,
    },
    mem_pool::{pool_str::PoolStr, pool_vec::PoolVec, shallow_clone::ShallowClone},
};

pub(crate) enum CanonicalizeRecordProblem {
    #[allow(dead_code)]
    InvalidOptionalValue {
        field_name: PoolStr,
        field_region: Region,
        record_region: Region,
    },
}

enum FieldVar {
    VarAndExprId(Variable, ExprId),
    OnlyVar(Variable),
}

pub(crate) fn canonicalize_fields<'a>(
    env: &mut Env<'a>,
    scope: &mut Scope,
    fields: &'a [Loc<roc_parse::ast::AssignedField<'a, roc_parse::ast::Expr<'a>>>],
) -> Result<(PoolVec<RecordField>, Output), CanonicalizeRecordProblem> {
    let mut can_fields: MutMap<&'a str, FieldVar> = MutMap::default();
    let mut output = Output::default();

    for loc_field in fields.iter() {
        match canonicalize_field(env, scope, &loc_field.value) {
            Ok(can_field) => {
                match can_field {
                    CanonicalField::LabelAndValue {
                        label,
                        value_expr,
                        value_output,
                        var,
                    } => {
                        let expr_id = env.pool.add(value_expr);

                        let replaced =
                            can_fields.insert(label, FieldVar::VarAndExprId(var, expr_id));

                        if let Some(_old) = replaced {
                            //                    env.problems.push(Problem::DuplicateRecordFieldValue {
                            //                        field_name: label,
                            //                        field_region: loc_field.region,
                            //                        record_region: region,
                            //                        replaced_region: old.region,
                            //                    });
                            todo!()
                        }

                        output.references.union_mut(value_output.references);
                    }
                    CanonicalField::InvalidLabelOnly { label, var } => {
                        let replaced = can_fields.insert(label, FieldVar::OnlyVar(var));

                        if let Some(_old) = replaced {
                            todo!()
                        }
                    }
                }
            }

            Err(CanonicalizeFieldProblem::InvalidOptionalValue {
                field_name: _,
                field_region: _,
            }) => {
                //                env.problem(Problem::InvalidOptionalValue {
                //                    field_name: field_name.clone(),
                //                    field_region,
                //                    record_region: region,
                //                });
                //                return Err(CanonicalizeRecordProblem::InvalidOptionalValue {
                //                    field_name,
                //                    field_region,
                //                    record_region: region,
                //                });
                todo!()
            }
        }
    }

    let pool_vec = PoolVec::with_capacity(can_fields.len() as u32, env.pool);

    for (node_id, (string, field_var)) in pool_vec.iter_node_ids().zip(can_fields.into_iter()) {
        let name = PoolStr::new(string, env.pool);

        match field_var {
            FieldVar::VarAndExprId(var, expr_id) => {
                env.pool[node_id] = RecordField::LabeledValue(name, var, expr_id);
            }
            FieldVar::OnlyVar(var) => {
                env.pool[node_id] = RecordField::InvalidLabelOnly(name, var);
            } // TODO RecordField::LabelOnly
        }
    }

    Ok((pool_vec, output))
}

#[allow(dead_code)]
enum CanonicalizeFieldProblem {
    InvalidOptionalValue {
        field_name: PoolStr,
        field_region: Region,
    },
}

// TODO: the `value_output: Output` field takes _a lot_ of space!
#[allow(clippy::large_enum_variant)]
enum CanonicalField<'a> {
    LabelAndValue {
        label: &'a str,
        value_expr: Expr2,
        value_output: Output,
        var: Variable,
    },
    InvalidLabelOnly {
        label: &'a str,
        var: Variable,
    }, // TODO make ValidLabelOnly
}

fn canonicalize_field<'a>(
    env: &mut Env<'a>,
    scope: &mut Scope,
    field: &'a roc_parse::ast::AssignedField<'a, roc_parse::ast::Expr<'a>>,
) -> Result<CanonicalField<'a>, CanonicalizeFieldProblem> {
    use roc_parse::ast::AssignedField::*;

    match field {
        // Both a label and a value, e.g. `{ name: "blah" }`
        RequiredValue(label, _, loc_expr) => {
            let field_var = env.var_store.fresh();
            let (loc_can_expr, output) =
                expr_to_expr2(env, scope, &loc_expr.value, loc_expr.region);

            Ok(CanonicalField::LabelAndValue {
                label: label.value,
                value_expr: loc_can_expr,
                value_output: output,
                var: field_var,
            })
        }

        OptionalValue(label, _, loc_expr) => Err(CanonicalizeFieldProblem::InvalidOptionalValue {
            field_name: PoolStr::new(label.value, env.pool),
            field_region: Region::span_across(&label.region, &loc_expr.region),
        }),

        // A label with no value, e.g. `{ name }` (this is sugar for { name: name })
        LabelOnly(label) => {
            let field_var = env.var_store.fresh();
            // TODO return ValidLabel if label points to in scope variable
            Ok(CanonicalField::InvalidLabelOnly {
                label: label.value,
                var: field_var,
            })
        }

        SpaceBefore(sub_field, _) | SpaceAfter(sub_field, _) => {
            canonicalize_field(env, scope, sub_field)
        }

        Malformed(_string) => {
            panic!("TODO canonicalize malformed record field");
        }
    }
}

#[inline(always)]
pub(crate) fn canonicalize_when_branch<'a>(
    env: &mut Env<'a>,
    scope: &mut Scope,
    branch: &'a roc_parse::ast::WhenBranch<'a>,
    output: &mut Output,
) -> (WhenBranch, References) {
    let patterns = PoolVec::with_capacity(branch.patterns.len() as u32, env.pool);

    let original_scope = scope;
    let mut scope = original_scope.shallow_clone();

    // TODO report symbols not bound in all patterns
    for (node_id, loc_pattern) in patterns.iter_node_ids().zip(branch.patterns.iter()) {
        let (new_output, can_pattern) = to_pattern2(
            env,
            &mut scope,
            roc_parse::pattern::PatternType::WhenBranch,
            &loc_pattern.value,
            loc_pattern.region,
        );

        output.union(new_output);

        env.set_region(node_id, loc_pattern.region);
        env.pool[node_id] = can_pattern;
    }

    let (value, mut branch_output) =
        expr_to_expr2(env, &mut scope, &branch.value.value, branch.value.region);
    let value_id = env.pool.add(value);
    env.set_region(value_id, branch.value.region);

    let guard = match &branch.guard {
        None => None,
        Some(loc_expr) => {
            let (can_guard, guard_branch_output) =
                expr_to_expr2(env, &mut scope, &loc_expr.value, loc_expr.region);

            let expr_id = env.pool.add(can_guard);
            env.set_region(expr_id, loc_expr.region);

            branch_output.union(guard_branch_output);
            Some(expr_id)
        }
    };

    // Now that we've collected all the references for this branch, check to see if
    // any of the new idents it defined were unused. If any were, report it.
    for (symbol, region) in scope.symbols() {
        let symbol = symbol;

        if !output.references.has_lookup(symbol)
            && !branch_output.references.has_lookup(symbol)
            && !original_scope.contains_symbol(symbol)
        {
            env.problem(Problem::UnusedDef(symbol, region));
        }
    }

    let references = branch_output.references.clone();
    output.union(branch_output);

    (
        WhenBranch {
            patterns,
            body: value_id,
            guard,
        },
        references,
    )
}

pub(crate) fn canonicalize_lookup(
    env: &mut Env<'_>,
    scope: &mut Scope,
    module_name: &str,
    ident: &str,
    region: Region,
) -> (Expr2, Output) {
    use Expr2::*;

    let mut output = Output::default();
    let can_expr = if module_name.is_empty() {
        // Since module_name was empty, this is an unqualified var.
        // Look it up in scope!
        match scope.lookup(&(*ident).into(), region) {
            Ok(symbol) => {
                output.references.lookups.insert(symbol);

                Var(symbol)
            }
            Err(problem) => {
                env.problem(Problem::RuntimeError(problem));

                RuntimeError()
            }
        }
    } else {
        // Since module_name was nonempty, this is a qualified var.
        // Look it up in the env!
        match env.qualified_lookup(module_name, ident, region) {
            Ok(symbol) => {
                output.references.lookups.insert(symbol);

                Var(symbol)
            }
            Err(problem) => {
                // Either the module wasn't imported, or
                // it was imported but it doesn't expose this ident.
                env.problem(Problem::RuntimeError(problem));

                RuntimeError()
            }
        }
    };

    // If it's valid, this ident should be in scope already.

    (can_expr, output)
}
