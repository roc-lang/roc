use roc_can::expr::{
    AnnotatedMark, ClosureData, Expr, Field, Recursive, WhenBranch, WhenBranchPattern,
};
use roc_can::pattern::Pattern;
use roc_collections::SendMap;
use roc_module::called_via::CalledVia;
use roc_module::ident::Lowercase;
use roc_module::symbol::Symbol;
use roc_region::all::{Loc, Region};
use roc_types::subs::{
    Content, ExhaustiveMark, FlatType, LambdaSet, OptVariable, RecordFields, RedundantMark,
    SubsSlice, TagExt, UnionLambdas, UnionTags, Variable,
};
use roc_types::types::RecordField;

use crate::synth_var;
use crate::util::{Env, ExtensionKind};

use super::decodeWith::decode_with;

// Example:
// stepField = \state, field ->
//     when field is
//         "first" ->
//             Keep (Decode.custom \bytes, fmt ->
//                 # Uses a single-branch `when` because `let` is more expensive to monomorphize
//                 # due to checks for polymorphic expressions, and `rec` would be polymorphic.
//                 when Decode.decodeWith bytes Decode.decoder fmt is
//                     rec ->
//                         {
//                             rest: rec.rest,
//                             result: when rec.result is
//                                 Ok val -> Ok {state & first: Ok val},
//                                 Err err -> Err err
//                         })
//
//         "second" ->
//             Keep (Decode.custom \bytes, fmt ->
//                 when Decode.decodeWith bytes Decode.decoder fmt is
//                     rec ->
//                         {
//                             rest: rec.rest,
//                             result: when rec.result is
//                                 Ok val -> Ok {state & second: Ok val},
//                                 Err err -> Err err
//                         })
//
//         _ -> Skip
pub(super) fn step_field(
    env: &mut Env,
    fields: Vec<Lowercase>,
    field_vars: &[Variable],
    result_field_vars: &[Variable],
    state_record_var: Variable,
    decode_err_var: Variable,
) -> (Expr, Variable) {
    let state_arg_symbol = env.new_symbol("stateRecord");
    let field_arg_symbol = env.new_symbol("field");

    // +1 because of the default branch.
    let mut branches = Vec::with_capacity(fields.len() + 1);
    let keep_payload_var = env.subs.fresh_unnamed_flex_var();
    let keep_or_skip_var = {
        let keep_payload_subs_slice = SubsSlice::insert_into_subs(env.subs, [keep_payload_var]);
        let flat_type = FlatType::TagUnion(
            UnionTags::insert_slices_into_subs(
                env.subs,
                [
                    ("Keep".into(), keep_payload_subs_slice),
                    ("Skip".into(), Default::default()),
                ],
            ),
            TagExt::Any(Variable::EMPTY_TAG_UNION),
        );

        synth_var(env.subs, Content::Structure(flat_type))
    };

    for ((field_name, &field_var), &result_field_var) in fields
        .into_iter()
        .zip(field_vars.iter())
        .zip(result_field_vars.iter())
    {
        // Example:
        // "first" ->
        //     Keep (Decode.custom \bytes, fmt ->
        //         # Uses a single-branch `when` because `let` is more expensive to monomorphize
        //         # due to checks for polymorphic expressions, and `rec` would be polymorphic.
        //         when Decode.decodeWith bytes Decode.decoder fmt is
        //             rec ->
        //                 {
        //                     rest: rec.rest,
        //                     result: when rec.result is
        //                         Ok val -> Ok {state & first: Ok val},
        //                         Err err -> Err err
        //                 }
        //     )

        let (decode_custom_ret_var, decode_custom) = custom_decoder(
            env,
            field_var,
            decode_err_var,
            state_record_var,
            &field_name,
            result_field_var,
            state_arg_symbol,
        );

        env.unify(keep_payload_var, decode_custom_ret_var);

        let keep = {
            // Keep (Decode.custom \bytes, fmt ->
            //     when Decode.decodeWith bytes Decode.decoder fmt is
            //         rec ->
            //             {
            //                 rest: rec.rest,
            //                 result: when rec.result is
            //                     Ok val -> Ok {state & first: Ok val},
            //                     Err err -> Err err
            //             }
            // )
            Expr::Tag {
                tag_union_var: keep_or_skip_var,
                ext_var: env.new_ext_var(ExtensionKind::TagUnion),
                name: "Keep".into(),
                arguments: vec![(decode_custom_ret_var, Loc::at_zero(decode_custom))],
            }
        };

        let branch = {
            // "first" ->
            //     Keep (Decode.custom \bytes, fmt ->
            //         when Decode.decodeWith bytes Decode.decoder fmt is
            //             rec ->
            //                 {
            //                     rest: rec.rest,
            //                     result: when rec.result is
            //                         Ok val -> Ok {state & first: Ok val},
            //                         Err err -> Err err
            //                 }
            //     )
            WhenBranch {
                patterns: vec![WhenBranchPattern {
                    pattern: Loc::at_zero(Pattern::StrLiteral(field_name.into())),
                    degenerate: false,
                }],
                value: Loc::at_zero(keep),
                guard: None,
                redundant: RedundantMark::known_non_redundant(),
            }
        };

        branches.push(branch);
    }

    // Example: `_ -> Skip`
    let default_branch = WhenBranch {
        patterns: vec![WhenBranchPattern {
            pattern: Loc::at_zero(Pattern::Underscore),
            degenerate: false,
        }],
        value: Loc::at_zero(Expr::Tag {
            tag_union_var: keep_or_skip_var,
            ext_var: env.new_ext_var(ExtensionKind::TagUnion),
            name: "Skip".into(),
            arguments: Vec::new(),
        }),
        guard: None,
        redundant: RedundantMark::known_non_redundant(),
    };

    branches.push(default_branch);

    // when field is
    let body = Expr::When {
        loc_cond: Box::new(Loc::at_zero(Expr::Var(field_arg_symbol, Variable::STR))),
        cond_var: Variable::STR,
        expr_var: keep_or_skip_var,
        region: Region::zero(),
        branches,
        branches_cond_var: Variable::STR,
        exhaustive: ExhaustiveMark::known_exhaustive(),
    };

    let step_field_closure = env.new_symbol("stepField");
    let function_type = env.subs.fresh_unnamed_flex_var();
    let closure_type = {
        let lambda_set = LambdaSet {
            solved: UnionLambdas::tag_without_arguments(env.subs, step_field_closure),
            recursion_var: OptVariable::NONE,
            unspecialized: Default::default(),
            ambient_function: function_type,
        };

        synth_var(env.subs, Content::LambdaSet(lambda_set))
    };

    {
        let args_slice = SubsSlice::insert_into_subs(env.subs, [state_record_var, Variable::STR]);

        env.subs.set_content(
            function_type,
            Content::Structure(FlatType::Func(args_slice, closure_type, keep_or_skip_var)),
        )
    };

    let expr = Expr::Closure(ClosureData {
        function_type,
        closure_type,
        return_type: keep_or_skip_var,
        name: step_field_closure,
        captured_symbols: Vec::new(),
        recursive: Recursive::NotRecursive,
        arguments: vec![
            (
                state_record_var,
                AnnotatedMark::known_exhaustive(),
                Loc::at_zero(Pattern::Identifier(state_arg_symbol)),
            ),
            (
                Variable::STR,
                AnnotatedMark::known_exhaustive(),
                Loc::at_zero(Pattern::Identifier(field_arg_symbol)),
            ),
        ],
        loc_body: Box::new(Loc::at_zero(body)),
    });

    (expr, function_type)
}
/// ```roc
/// Decode.custom \bytes, fmt ->
///    # Uses a single-branch `when` because `let` is more expensive to monomorphize
///    # due to checks for polymorphic expressions, and `rec` would be polymorphic.
///    when Decode.decodeWith bytes Decode.decoder fmt is
///        rec ->
///            {
///                rest: rec.rest,
///                result: when rec.result is
///                    Ok val -> Ok {state & first: Ok val},
///                    Err err -> Err err
///            }
/// )
/// ```
///Expression for custom decoder
fn custom_decoder(
    env: &mut Env<'_>,
    field_var: Variable,
    decode_err_var: Variable,
    state_record_var: Variable,
    field_name: &Lowercase,
    result_field_var: Variable,
    state_arg_symbol: Symbol,
) -> (Variable, Expr) {
    let (this_custom_callback_var, custom_callback) = custom_decoder_lambda(
        env,
        field_name,
        field_var,
        state_arg_symbol,
        state_record_var,
        result_field_var,
        decode_err_var,
    );

    let decode_custom_ret_var = env.subs.fresh_unnamed_flex_var();
    let decode_custom = {
        let decode_custom_var = env.import_builtin_symbol_var(Symbol::DECODE_CUSTOM);
        let decode_custom_closure_var = env.subs.fresh_unnamed_flex_var();
        let this_decode_custom_var = {
            let subs_slice = SubsSlice::insert_into_subs(env.subs, [this_custom_callback_var]);
            let flat_type =
                FlatType::Func(subs_slice, decode_custom_closure_var, decode_custom_ret_var);

            synth_var(env.subs, Content::Structure(flat_type))
        };

        env.unify(decode_custom_var, this_decode_custom_var);

        // Decode.custom \bytes, fmt -> …
        Expr::Call(
            Box::new((
                this_decode_custom_var,
                Loc::at_zero(Expr::Var(Symbol::DECODE_CUSTOM, this_decode_custom_var)),
                decode_custom_closure_var,
                decode_custom_ret_var,
            )),
            vec![(this_custom_callback_var, Loc::at_zero(custom_callback))],
            CalledVia::Space,
        )
    };
    (decode_custom_ret_var, decode_custom)
}
/// ```roc
/// \bytes, fmt ->
///     when Decode.decodeWith bytes Decode.decoder fmt is
///         rec ->
///             {
///                 rest: rec.rest,
///                 result: when rec.result is
///                     Ok val -> Ok {state & first: Ok val},
///                     Err err -> Err err
///             }
///
/// ```

fn custom_decoder_lambda(
    env: &mut Env<'_>,
    field_name: &Lowercase,
    field_var: Variable,
    state_arg_symbol: Symbol,
    state_record_var: Variable,
    result_field_var: Variable,
    decode_err_var: Variable,
) -> (Variable, Expr) {
    let this_custom_callback_var;
    let custom_callback_ret_var;

    // \bytes, fmt ->
    //     when Decode.decodeWith bytes Decode.decoder fmt is
    //         rec ->
    //             {
    //                 rest: rec.rest,
    //                 result: when rec.result is
    //                     Ok val -> Ok {state & first: Ok val},
    //                     Err err -> Err err
    //             }
    let custom_callback = {
        let bytes_arg_symbol = env.new_symbol("bytes");
        let fmt_arg_symbol = env.new_symbol("fmt");
        let bytes_arg_var = env.subs.fresh_unnamed_flex_var();
        let fmt_arg_var = env.subs.fresh_unnamed_flex_var();
        // The result of decoding this field's value - either the updated state, or a decoding error.
        let when_expr_var = {
            let flat_type = FlatType::TagUnion(
                UnionTags::for_result(env.subs, state_record_var, decode_err_var),
                TagExt::Any(Variable::EMPTY_TAG_UNION),
            );

            synth_var(env.subs, Content::Structure(flat_type))
        };

        // What our decoder passed to `Decode.custom` returns - the result of decoding the
        // field's value, and the remaining bytes.
        custom_callback_ret_var = {
            let rest_field = RecordField::Required(Variable::LIST_U8);
            let result_field = RecordField::Required(when_expr_var);
            let flat_type = FlatType::Record(
                RecordFields::insert_into_subs(
                    env.subs,
                    [("rest".into(), rest_field), ("result".into(), result_field)],
                ),
                Variable::EMPTY_RECORD,
            );

            synth_var(env.subs, Content::Structure(flat_type))
        };

        // Decode.decodeWith bytes Decode.decoder fmt
        let (condition_expr, rec_var, rec_dot_result) = decode_with(
            env,
            field_var,
            Expr::Var(bytes_arg_symbol, bytes_arg_var),
            fmt_arg_var,
            fmt_arg_symbol,
            decode_err_var,
        );

        // # Uses a single-branch `when` because `let` is more expensive to monomorphize
        // # due to checks for polymorphic expressions, and `rec` would be polymorphic.
        // when Decode.decodeWith bytes Decode.decoder fmt is
        //     rec ->
        //         {
        //             rest: rec.rest,
        //             result: when rec.result is
        //                 Ok val -> Ok {state & first: Ok val},
        //                 Err err -> Err err
        //         }
        let custom_callback_body = {
            let rec_symbol = env.new_symbol("rec");

            //         {
            //             rest: rec.rest,
            //             result: when rec.result is
            //                 Ok val -> Ok {state & first: Ok val},
            //                 Err err -> Err err
            //         }
            let branch_body = state_record_update(
                env,
                field_name,
                field_var,
                state_arg_symbol,
                state_record_var,
                rec_symbol,
                rec_var,
                rec_dot_result,
                when_expr_var,
                custom_callback_ret_var,
                decode_err_var,
                result_field_var,
            );

            let branch = WhenBranch {
                patterns: vec![WhenBranchPattern {
                    pattern: Loc::at_zero(Pattern::Identifier(rec_symbol)),
                    degenerate: false,
                }],
                value: Loc::at_zero(branch_body),
                guard: None,
                redundant: RedundantMark::known_non_redundant(),
            };

            // when Decode.decodeWith bytes Decode.decoder fmt is
            // ...
            Expr::When {
                loc_cond: Box::new(Loc::at_zero(condition_expr)),
                cond_var: rec_var,
                expr_var: custom_callback_ret_var,
                region: Region::zero(),
                branches: vec![branch],
                branches_cond_var: rec_var,
                exhaustive: ExhaustiveMark::known_exhaustive(),
            }
        };
        let custom_closure_symbol = env.new_symbol("customCallback");
        this_custom_callback_var = env.subs.fresh_unnamed_flex_var();
        let custom_callback_lambda_set_var = {
            let content = Content::LambdaSet(LambdaSet {
                solved: UnionLambdas::insert_into_subs(
                    env.subs,
                    [(custom_closure_symbol, [state_record_var])],
                ),
                recursion_var: OptVariable::NONE,
                unspecialized: Default::default(),
                ambient_function: this_custom_callback_var,
            });
            let custom_callback_lambda_set_var = synth_var(env.subs, content);
            let subs_slice = SubsSlice::insert_into_subs(env.subs, [bytes_arg_var, fmt_arg_var]);

            env.subs.set_content(
                this_custom_callback_var,
                Content::Structure(FlatType::Func(
                    subs_slice,
                    custom_callback_lambda_set_var,
                    custom_callback_ret_var,
                )),
            );

            custom_callback_lambda_set_var
        };

        // \bytes, fmt -> …
        Expr::Closure(ClosureData {
            function_type: this_custom_callback_var,
            closure_type: custom_callback_lambda_set_var,
            return_type: custom_callback_ret_var,
            name: custom_closure_symbol,
            captured_symbols: vec![(state_arg_symbol, state_record_var)],
            recursive: Recursive::NotRecursive,
            arguments: vec![
                (
                    bytes_arg_var,
                    AnnotatedMark::known_exhaustive(),
                    Loc::at_zero(Pattern::Identifier(bytes_arg_symbol)),
                ),
                (
                    fmt_arg_var,
                    AnnotatedMark::known_exhaustive(),
                    Loc::at_zero(Pattern::Identifier(fmt_arg_symbol)),
                ),
            ],
            loc_body: Box::new(Loc::at_zero(custom_callback_body)),
        })
    };
    (this_custom_callback_var, custom_callback)
}

/// ```roc
///       {
///           rest: rec.rest,
///           result: when rec.result is
///               Ok val -> Ok {state & first: Ok val},
///               Err err -> Err err
///       }
/// ```
fn state_record_update(
    env: &mut Env<'_>,
    field_name: &Lowercase,
    field_var: Variable,
    state_arg_symbol: Symbol,
    state_record_var: Variable,
    rec_symbol: Symbol,
    rec_var: Variable,
    rec_dot_result: Variable,
    when_expr_var: Variable,
    custom_callback_ret_var: Variable,
    decode_err_var: Variable,
    result_field_var: Variable,
) -> Expr {
    
    {
        // result: when rec.result is
        //     Ok val -> Ok {state & first: Ok val},
        //     Err err -> Err err
        let result_val = {
            let ok_val_symbol = env.new_symbol("val");
            let err_val_symbol = env.new_symbol("err");

            // Ok {state & first: Ok val},
            let ok_branch_expr = {
                let mut updates = SendMap::default();

                updates.insert(
                    field_name.clone(),
                    Field {
                        var: result_field_var,
                        region: Region::zero(),
                        loc_expr: Box::new(Loc::at_zero(Expr::Tag {
                            tag_union_var: result_field_var,
                            ext_var: env.new_ext_var(ExtensionKind::TagUnion),
                            name: "Ok".into(),
                            arguments: vec![(
                                field_var,
                                Loc::at_zero(Expr::Var(ok_val_symbol, field_var)),
                            )],
                        })),
                    },
                );

                let updated_record = Expr::RecordUpdate {
                    record_var: state_record_var,
                    ext_var: env.new_ext_var(ExtensionKind::Record),
                    symbol: state_arg_symbol,
                    updates,
                };

                Expr::Tag {
                    tag_union_var: when_expr_var,
                    ext_var: env.new_ext_var(ExtensionKind::TagUnion),
                    name: "Ok".into(),
                    arguments: vec![(state_record_var, Loc::at_zero(updated_record))],
                }
            };

            let branches = vec![
                // Ok val -> Ok {state & first: Ok val},
                WhenBranch {
                    patterns: vec![WhenBranchPattern {
                        pattern: Loc::at_zero(Pattern::AppliedTag {
                            whole_var: rec_dot_result,
                            ext_var: Variable::EMPTY_TAG_UNION,
                            tag_name: "Ok".into(),
                            arguments: vec![(
                                field_var,
                                Loc::at_zero(Pattern::Identifier(ok_val_symbol)),
                            )],
                        }),
                        degenerate: false,
                    }],
                    value: Loc::at_zero(ok_branch_expr),
                    guard: None,
                    redundant: RedundantMark::known_non_redundant(),
                },
                // Err err -> Err err
                WhenBranch {
                    patterns: vec![WhenBranchPattern {
                        pattern: Loc::at_zero(Pattern::AppliedTag {
                            whole_var: rec_dot_result,
                            ext_var: Variable::EMPTY_TAG_UNION,
                            tag_name: "Err".into(),
                            arguments: vec![(
                                decode_err_var,
                                Loc::at_zero(Pattern::Identifier(err_val_symbol)),
                            )],
                        }),
                        degenerate: false,
                    }],
                    value: Loc::at_zero(Expr::Tag {
                        tag_union_var: when_expr_var,
                        ext_var: env.new_ext_var(ExtensionKind::TagUnion),
                        name: "Err".into(),
                        arguments: vec![(
                            decode_err_var,
                            Loc::at_zero(Expr::Var(err_val_symbol, decode_err_var)),
                        )],
                    }),
                    guard: None,
                    redundant: RedundantMark::known_non_redundant(),
                },
            ];

            // when rec.result is
            //     Ok val -> Ok {state & first: Ok val},
            //     Err err -> Err err
            Expr::When {
                loc_cond: Box::new(Loc::at_zero(Expr::RecordAccess {
                    record_var: rec_var,
                    ext_var: env.new_ext_var(ExtensionKind::Record),
                    field_var: rec_dot_result,
                    loc_expr: Box::new(Loc::at_zero(Expr::Var(rec_symbol, rec_var))),
                    field: "result".into(),
                })),
                cond_var: rec_dot_result,
                expr_var: when_expr_var,
                region: Region::zero(),
                branches,
                branches_cond_var: rec_dot_result,
                exhaustive: ExhaustiveMark::known_exhaustive(),
            }
        };

        // {
        //     rest: rec.rest,
        //     result: when rec.result is
        //         Ok val -> Ok {state & first: Ok val},
        //         Err err -> Err err
        // }
        let mut fields_map = SendMap::default();

        fields_map.insert(
            "rest".into(),
            Field {
                var: Variable::LIST_U8,
                region: Region::zero(),
                loc_expr: Box::new(Loc::at_zero(Expr::RecordAccess {
                    record_var: rec_var,
                    ext_var: env.new_ext_var(ExtensionKind::Record),
                    field_var: Variable::LIST_U8,
                    loc_expr: Box::new(Loc::at_zero(Expr::Var(rec_symbol, rec_var))),
                    field: "rest".into(),
                })),
            },
        );

        // result: when rec.result is
        //     Ok val -> Ok {state & first: Ok val},
        //     Err err -> Err err
        fields_map.insert(
            "result".into(),
            Field {
                var: when_expr_var,
                region: Region::zero(),
                loc_expr: Box::new(Loc::at_zero(result_val)),
            },
        );

        Expr::Record {
            record_var: custom_callback_ret_var,
            fields: fields_map,
        }
    }
}
