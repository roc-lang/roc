use std::ops::ControlFlow;

use bumpalo::Bump;
use roc_collections::soa::slice_extend_new;
use roc_error_macros::internal_error;
use roc_types::{
    subs::{
        self, AliasVariables, Content, Descriptor, FlatType, GetSubsSlice, Mark, OptVariable, Rank,
        RecordFields, Subs, TagExt, TupleElems, UnionLabels, Variable,
    },
    types::{RecordField, Uls},
};

use crate::env::SolveEnv;

// TODO: eventually, we could possibly use the arena in Env instead.
pub(crate) fn deep_copy_var_in(
    env: &mut SolveEnv,
    rank: Rank,
    var: Variable,
    arena: &Bump,
) -> Variable {
    let mut visited = bumpalo::collections::Vec::with_capacity_in(256, arena);

    let pool = env.pools.get_mut(rank);

    let var = env.subs.get_root_key(var);
    match deep_copy_var_decision(env.subs, rank, var) {
        ControlFlow::Break(copy) => copy,
        ControlFlow::Continue(copy) => {
            deep_copy_var_help(env.subs, rank, pool, &mut visited, var, copy);

            // we have tracked all visited variables, and can now traverse them
            // in one go (without looking at the UnificationTable) and clear the copy field
            for var in visited {
                env.subs.set_copy_unchecked(var, OptVariable::NONE);
            }

            copy
        }
    }
}

#[inline]
fn has_trivial_copy(subs: &Subs, root_var: Variable) -> Option<Variable> {
    let existing_copy = subs.get_copy_unchecked(root_var);

    if let Some(copy) = existing_copy.into_variable() {
        Some(copy)
    } else if subs.get_rank_unchecked(root_var) != Rank::GENERALIZED {
        Some(root_var)
    } else {
        None
    }
}

#[inline]
fn deep_copy_var_decision(
    subs: &mut Subs,
    max_rank: Rank,
    var: Variable,
) -> ControlFlow<Variable, Variable> {
    let var = subs.get_root_key(var);
    if let Some(copy) = has_trivial_copy(subs, var) {
        ControlFlow::Break(copy)
    } else {
        let copy_descriptor = Descriptor {
            content: Content::Structure(FlatType::EmptyTagUnion),
            rank: max_rank,
            mark: Mark::NONE,
            copy: OptVariable::NONE,
        };

        let copy = subs.fresh(copy_descriptor);

        // Link the original variable to the new variable. This lets us
        // avoid making multiple copies of the variable we are instantiating.
        //
        // Need to do this before recursively copying to avoid looping.
        subs.set_mark_unchecked(var, Mark::NONE);
        subs.set_copy_unchecked(var, copy.into());

        ControlFlow::Continue(copy)
    }
}

fn deep_copy_var_help(
    subs: &mut Subs,
    max_rank: Rank,
    pool: &mut Vec<Variable>,
    visited: &mut bumpalo::collections::Vec<'_, Variable>,
    initial_source: Variable,
    initial_copy: Variable,
) -> Variable {
    use roc_types::subs::Content::*;
    use roc_types::subs::FlatType::*;

    struct DeepCopyVarWork {
        source: Variable,
        copy: Variable,
    }

    let initial = DeepCopyVarWork {
        source: initial_source,
        copy: initial_copy,
    };
    let mut stack = vec![initial];

    macro_rules! work {
        ($variable:expr) => {{
            let var = subs.get_root_key($variable);
            match deep_copy_var_decision(subs, max_rank, var) {
                ControlFlow::Break(copy) => copy,
                ControlFlow::Continue(copy) => {
                    stack.push(DeepCopyVarWork { source: var, copy });

                    copy
                }
            }
        }};
    }

    macro_rules! copy_sequence {
        ($length:expr, $variables:expr) => {{
            let new_variables = subs.reserve_into_vars($length as _);
            for (target_index, var_index) in (new_variables.indices()).zip($variables) {
                let var = subs[var_index];
                let copy_var = work!(var);
                subs.variables[target_index] = copy_var;
            }

            new_variables
        }};
    }

    macro_rules! copy_union {
        ($tags:expr) => {{
            let new_variable_slices = subs.reserve_variable_slices($tags.len());

            let it = (new_variable_slices.indices()).zip($tags.variables());
            for (target_index, index) in it {
                let slice = subs[index];

                let new_variables = copy_sequence!(slice.len(), slice);
                subs.variable_slices[target_index] = new_variables;
            }

            UnionLabels::from_slices($tags.labels(), new_variable_slices)
        }};
    }

    // When generalizing annotations with `Openness` extensions
    // we want to promote them to `Any`, so that usages at
    // specialized sites can grow unboundedly and are not bound to
    // openness-polymorphism.
    macro_rules! copy_tag_ext {
        ($ext:expr) => {
            TagExt::Any(work!($ext.var()))
        };
    }

    while let Some(DeepCopyVarWork { source: var, copy }) = stack.pop() {
        visited.push(var);
        pool.push(copy);

        let content = *subs.get_content_unchecked(var);

        // Now we recursively copy the content of the variable.
        // We have already marked the variable as copied, so we
        // will not repeat this work or crawl this variable again.
        match content {
            Structure(flat_type) => {
                let new_flat_type = match flat_type {
                    Apply(symbol, arguments) => {
                        let new_arguments = copy_sequence!(arguments.len(), arguments);

                        Apply(symbol, new_arguments)
                    }

                    Func(arguments, closure_var, ret_var, fx_var) => {
                        let new_ret_var = work!(ret_var);
                        let new_closure_var = work!(closure_var);
                        let new_fx_var = work!(fx_var);

                        let new_arguments = copy_sequence!(arguments.len(), arguments);

                        Func(new_arguments, new_closure_var, new_ret_var, new_fx_var)
                    }

                    same @ (EmptyRecord | EmptyTagUnion | EffectfulFunc) => same,

                    Record(fields, ext_var) => {
                        let record_fields = {
                            let new_variables =
                                copy_sequence!(fields.len(), fields.iter_variables());

                            // When copying a let-generalized record to a specialized region, rigid
                            // optionals just become optionals.
                            let field_types = subs.get_subs_slice(fields.record_fields());
                            let has_rigid_optional_field = field_types
                                .iter()
                                .any(|f| matches!(f, RecordField::RigidOptional(..)));

                            let new_field_types_start = if has_rigid_optional_field {
                                let field_types = field_types.to_vec();
                                let slice = slice_extend_new(
                                    &mut subs.record_fields,
                                    field_types.into_iter().map(|f| match f {
                                        RecordField::RigidOptional(())
                                        | RecordField::RigidRequired(()) => internal_error!("Rigid optional/required should be generalized to non-rigid by this point"),

                                        RecordField::Demanded(_)
                                        | RecordField::Required(_)
                                        | RecordField::Optional(_) => f,
                                    }),
                                );
                                slice.start()
                            } else {
                                fields.field_types_start
                            };

                            RecordFields {
                                length: fields.length,
                                field_names_start: fields.field_names_start,
                                variables_start: new_variables.start(),
                                field_types_start: new_field_types_start,
                            }
                        };

                        Record(record_fields, work!(ext_var))
                    }

                    Tuple(elems, ext_var) => {
                        let tuple_elems = {
                            let new_variables = copy_sequence!(elems.len(), elems.iter_variables());

                            TupleElems {
                                length: elems.length,
                                variables_start: new_variables.start(),
                                elem_index_start: elems.elem_index_start,
                            }
                        };

                        Tuple(tuple_elems, work!(ext_var))
                    }

                    TagUnion(tags, ext_var) => {
                        let union_tags = copy_union!(tags);

                        TagUnion(union_tags, copy_tag_ext!(ext_var))
                    }

                    FunctionOrTagUnion(tag_name, symbol, ext_var) => {
                        FunctionOrTagUnion(tag_name, symbol, copy_tag_ext!(ext_var))
                    }

                    RecursiveTagUnion(rec_var, tags, ext_var) => {
                        let union_tags = copy_union!(tags);

                        RecursiveTagUnion(work!(rec_var), union_tags, copy_tag_ext!(ext_var))
                    }
                };

                subs.set_content_unchecked(copy, Structure(new_flat_type));
            }

            FlexVar(_) | FlexAbleVar(_, _) | Error | ErasedLambda | Pure | Effectful => {
                subs.set_content_unchecked(copy, content);
            }

            RecursionVar {
                opt_name,
                structure,
            } => {
                let content = RecursionVar {
                    opt_name,
                    structure: work!(structure),
                };

                subs.set_content_unchecked(copy, content);
            }

            RigidVar(name) => {
                subs.set_content_unchecked(copy, FlexVar(Some(name)));
            }

            RigidAbleVar(name, ability) => {
                subs.set_content_unchecked(copy, FlexAbleVar(Some(name), ability));
            }

            Alias(symbol, arguments, real_type_var, kind) => {
                let new_variables =
                    copy_sequence!(arguments.all_variables_len, arguments.all_variables());

                let new_arguments = AliasVariables {
                    variables_start: new_variables.start(),
                    ..arguments
                };

                let new_real_type_var = work!(real_type_var);
                let new_content = Alias(symbol, new_arguments, new_real_type_var, kind);

                subs.set_content_unchecked(copy, new_content);
            }

            LambdaSet(subs::LambdaSet {
                solved,
                recursion_var,
                unspecialized,
                ambient_function: ambient_function_var,
            }) => {
                let lambda_set_var = copy;

                let new_solved = copy_union!(solved);
                let new_rec_var = recursion_var.map(|v| work!(v));
                let new_unspecialized = subs.reserve_uls_slice(unspecialized.len());

                for (new_uls_index, uls_index) in
                    (new_unspecialized.into_iter()).zip(unspecialized.into_iter())
                {
                    let Uls(var, sym, region) = subs[uls_index];
                    let new_var = work!(var);

                    deep_copy_uls_precondition(subs, var, new_var);

                    subs[new_uls_index] = Uls(new_var, sym, region);

                    subs.uls_of_var.add(new_var, lambda_set_var);
                }

                let new_ambient_function_var = work!(ambient_function_var);

                subs.set_content_unchecked(
                    lambda_set_var,
                    LambdaSet(subs::LambdaSet {
                        solved: new_solved,
                        recursion_var: new_rec_var,
                        unspecialized: new_unspecialized,
                        ambient_function: new_ambient_function_var,
                    }),
                );
            }

            RangedNumber(range) => {
                let new_content = RangedNumber(range);

                subs.set_content_unchecked(copy, new_content);
            }
        }
    }

    initial_copy
}

#[inline(always)]
fn deep_copy_uls_precondition(subs: &Subs, original_var: Variable, new_var: Variable) {
    if cfg!(debug_assertions) {
        let content = subs.get_content_without_compacting(original_var);

        debug_assert!(
            matches!(
                content,
                Content::FlexAbleVar(..) | Content::RigidAbleVar(..)
            ),
            "var in unspecialized lamba set is not bound to an ability, it is {:?}",
            roc_types::subs::SubsFmtContent(content, subs)
        );
        debug_assert!(
            original_var != new_var,
            "unspecialized lamba set var was not instantiated"
        );
    }
}
