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
    Content, ExhaustiveMark, FlatType, LambdaSet, OptVariable, RecordFields, RedundantMark, TagExt,
    UnionLambdas, UnionTags, Variable,
};
use roc_types::types::RecordField;

use crate::synth_var;
use crate::util::{empty_list, ok_to_ok_branch, Env, ExtensionKind};

use super::wrap_in_decode_custom_decode_with;

/// Implements decoding of a record. For example, for
///
/// ```text
///   {first: a, second: b}
/// ```
///
/// we'd like to generate an implementation like
///
/// ```roc
/// decoder : Decoder {first: a, second: b} fmt where a implements Decoding, b implements Decoding, fmt implements DecoderFormatting
/// decoder =
///     initial_state : {f0: Result a [NoField], f1: Result b [NoField]}
///     initial_state = {f0: Err NoField, f1: Err NoField}
///
///     step_field = \state, field ->
///         when field is
///             "first" ->
///                 Keep (Decode.custom \bytes, fmt ->
///                     when Decode.decode_with bytes Decode.decoder fmt is
///                         {result, rest} ->
///                             {result: Result.map result \val -> {state & f0: Ok val}, rest})
///             "second" ->
///                 Keep (Decode.custom \bytes, fmt ->
///                     when Decode.decode_with bytes Decode.decoder fmt is
///                         {result, rest} ->
///                             {result: Result.map result \val -> {state & f1: Ok val}, rest})
///             _ -> Skip
///
///    finalizer = \rec, fmt ->
///        when
///            when rec.f0 is
///                Err NoField ->
///                    when Decode.decode_with [] Decode.decoder fmt is
///                        rec2 -> rec2.result
///                Ok a -> Ok a
///        is
///            Ok f0 ->
///                when
///                    when rec.f1 is
///                        Err NoField ->
///                            when Decode.decode_with [] Decode.decoder fmt is
///                                rec2 -> rec2.result
///                        Ok a -> Ok a
///                is
///                    Ok f1 -> Ok { f1, f0 }
///                    Err _ -> Err TooShort
///            Err _ -> Err TooShort
///
///            Decode.custom \bytes, fmt -> Decode.decode_with bytes (Decode.record initial_state step_field finalizer) fmt
///```
pub(crate) fn decoder(
    env: &mut Env,
    _def_symbol: Symbol,
    fields: Vec<Lowercase>,
) -> (Expr, Variable) {
    // The decoded type of each field in the record, e.g. {first: a, second: b}.
    let mut field_vars = Vec::with_capacity(fields.len());
    // The type of each field in the decoding state, e.g. {first: Result a [NoField], second: Result b [NoField]}
    let mut result_field_vars = Vec::with_capacity(fields.len());

    // initial_state = ...
    let (initial_state_var, initial_state) =
        initial_state(env, &fields, &mut field_vars, &mut result_field_vars);

    // finalizer = ...
    let (finalizer, finalizer_var, decode_err_var) = finalizer(
        env,
        initial_state_var,
        &fields,
        &field_vars,
        &result_field_vars,
    );

    // step_field = ...
    let (step_field, step_var) = step_field(
        env,
        fields,
        &field_vars,
        &result_field_vars,
        initial_state_var,
        decode_err_var,
    );

    // Build up the type of `Decode.record` we expect
    let record_decoder_var = env.subs.fresh_unnamed_flex_var();
    let decode_record_lambda_set = env.subs.fresh_unnamed_flex_var();
    let decode_record_var = env.import_builtin_symbol_var(Symbol::DECODE_RECORD);
    let this_decode_record_var = {
        let flat_type = FlatType::Func(
            env.subs
                .insert_into_vars([initial_state_var, step_var, finalizer_var]),
            decode_record_lambda_set,
            record_decoder_var,
            Variable::PURE,
        );

        synth_var(env.subs, Content::Structure(flat_type))
    };

    env.unify(decode_record_var, this_decode_record_var);

    // Decode.record initial_state step_field finalizer
    let call_decode_record = Expr::Call(
        Box::new((
            this_decode_record_var,
            Loc::at_zero(Expr::AbilityMember(
                Symbol::DECODE_RECORD,
                None,
                this_decode_record_var,
            )),
            decode_record_lambda_set,
            record_decoder_var,
            Variable::PURE,
        )),
        vec![
            (initial_state_var, Loc::at_zero(initial_state)),
            (step_var, Loc::at_zero(step_field)),
            (finalizer_var, Loc::at_zero(finalizer)),
        ],
        CalledVia::Space,
    );

    let (call_decode_custom, decode_custom_ret_var) = {
        let bytes_sym = env.new_symbol("bytes");
        let fmt_sym = env.new_symbol("fmt");
        let fmt_var = env.subs.fresh_unnamed_flex_var();

        let (decode_custom, decode_custom_var) = wrap_in_decode_custom_decode_with(
            env,
            bytes_sym,
            (fmt_sym, fmt_var),
            vec![],
            (call_decode_record, record_decoder_var),
        );

        (decode_custom, decode_custom_var)
    };

    (call_decode_custom, decode_custom_ret_var)
}

// Example:
// step_field = \state, field ->
//     when field is
//         "first" ->
//             Keep (Decode.custom \bytes, fmt ->
//                 # Uses a single-branch `when` because `let` is more expensive to monomorphize
//                 # due to checks for polymorphic expressions, and `rec` would be polymorphic.
//                 when Decode.decode_with bytes Decode.decoder fmt is
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
//                 when Decode.decode_with bytes Decode.decoder fmt is
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
    let state_arg_symbol = env.new_symbol("state_record");
    let field_arg_symbol = env.new_symbol("field");

    // +1 because of the default branch.
    let mut branches = Vec::with_capacity(fields.len() + 1);
    let keep_payload_var = env.subs.fresh_unnamed_flex_var();
    let keep_or_skip_var = {
        let keep_payload_subs_slice = env.subs.insert_into_vars([keep_payload_var]);
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
        //         when Decode.decode_with bytes Decode.decoder fmt is
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
            DecodingFieldArgs {
                field_name: field_name.clone(),
                field_var,
                state_arg_symbol,
                state_record_var,
                result_field_var,
                decode_err_var,
            },
        );

        env.unify(keep_payload_var, decode_custom_ret_var);

        let keep = {
            // Keep (Decode.custom \bytes, fmt ->
            //     when Decode.decode_with bytes Decode.decoder fmt is
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
            //         when Decode.decode_with bytes Decode.decoder fmt is
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

    let step_field_closure = env.new_symbol("step_field");
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
        let args_slice = env.subs.insert_into_vars([state_record_var, Variable::STR]);

        env.subs.set_content(
            function_type,
            Content::Structure(FlatType::Func(
                args_slice,
                closure_type,
                keep_or_skip_var,
                Variable::PURE,
            )),
        )
    };

    let expr = Expr::Closure(ClosureData {
        function_type,
        closure_type,
        return_type: keep_or_skip_var,
        fx_type: Variable::PURE,
        early_returns: vec![],
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

struct DecodingFieldArgs {
    field_name: Lowercase,
    field_var: Variable,
    state_arg_symbol: Symbol,
    state_record_var: Variable,
    result_field_var: Variable,
    decode_err_var: Variable,
}

/// Expression for custom decoder.
///
/// ```roc
/// Decode.custom \bytes, fmt ->
///    # Uses a single-branch `when` because `let` is more expensive to monomorphize
///    # due to checks for polymorphic expressions, and `rec` would be polymorphic.
///    when Decode.decode_with bytes Decode.decoder fmt is
///        rec ->
///            {
///                rest: rec.rest,
///                result: when rec.result is
///                    Ok val -> Ok {state & first: Ok val},
///                    Err err -> Err err
///            }
/// )
/// ```
fn custom_decoder(env: &mut Env<'_>, args: DecodingFieldArgs) -> (Variable, Expr) {
    let (this_custom_callback_var, custom_callback) = custom_decoder_lambda(env, args);

    let decode_custom_ret_var = env.subs.fresh_unnamed_flex_var();
    let decode_custom = {
        let decode_custom_var = env.import_builtin_symbol_var(Symbol::DECODE_CUSTOM);
        let decode_custom_closure_var = env.subs.fresh_unnamed_flex_var();
        let this_decode_custom_var = {
            let subs_slice = env.subs.insert_into_vars([this_custom_callback_var]);
            let flat_type = FlatType::Func(
                subs_slice,
                decode_custom_closure_var,
                decode_custom_ret_var,
                Variable::PURE,
            );

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
                Variable::PURE,
            )),
            vec![(this_custom_callback_var, Loc::at_zero(custom_callback))],
            CalledVia::Space,
        )
    };
    (decode_custom_ret_var, decode_custom)
}

/// ```roc
/// \bytes, fmt ->
///     when Decode.decode_with bytes Decode.decoder fmt is
///         rec ->
///             {
///                 rest: rec.rest,
///                 result: when rec.result is
///                     Ok val -> Ok {state & first: Ok val},
///                     Err err -> Err err
///             }
///
/// ```
fn custom_decoder_lambda(env: &mut Env<'_>, args: DecodingFieldArgs) -> (Variable, Expr) {
    let DecodingFieldArgs {
        field_var,
        state_arg_symbol,
        state_record_var,
        decode_err_var,
        ..
    } = args;

    let this_custom_callback_var;
    let custom_callback_ret_var;

    // \bytes, fmt ->
    //     when Decode.decode_with bytes Decode.decoder fmt is
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

        // What our decoder passed to `Decode.custom` returns; the result of decoding the
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

        // Decode.decode_with bytes Decode.decoder fmt
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
        // when Decode.decode_with bytes Decode.decoder fmt is
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
                args,
                rec_symbol,
                rec_var,
                rec_dot_result,
                when_expr_var,
                custom_callback_ret_var,
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

            // when Decode.decode_with bytes Decode.decoder fmt is
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
            let subs_slice = env.subs.insert_into_vars([bytes_arg_var, fmt_arg_var]);

            env.subs.set_content(
                this_custom_callback_var,
                Content::Structure(FlatType::Func(
                    subs_slice,
                    custom_callback_lambda_set_var,
                    custom_callback_ret_var,
                    Variable::PURE,
                )),
            );

            custom_callback_lambda_set_var
        };

        // \bytes, fmt -> …
        Expr::Closure(ClosureData {
            function_type: this_custom_callback_var,
            closure_type: custom_callback_lambda_set_var,
            return_type: custom_callback_ret_var,
            fx_type: Variable::PURE,
            early_returns: vec![],
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
    DecodingFieldArgs {
        field_name,
        field_var,
        state_arg_symbol,
        state_record_var,
        result_field_var,
        decode_err_var,
    }: DecodingFieldArgs,
    rec_symbol: Symbol,
    rec_var: Variable,
    rec_dot_result: Variable,
    when_expr_var: Variable,
    custom_callback_ret_var: Variable,
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
                    field_name,
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

/// Example:
/// finalizer = \rec, fmt ->
/// when
///     when rec.f0 is
///         Err NoField ->
///             when Decode.decode_with [] Decode.decoder fmt is
///                 rec2 -> rec2.result
///         Ok a -> Ok a
/// is
///     Ok f0 ->
///         when
///             when rec.f1 is
///                 Err NoField ->
///                     when Decode.decode_with [] Decode.decoder fmt is
///                         rec2 -> rec2.result
///                 Ok a -> Ok a
///         is
///             Ok f1 -> Ok { f1, f0 }
///             Err _ -> Err TooShort
///     Err _ -> Err TooShort
pub(super) fn finalizer(
    env: &mut Env,
    state_record_var: Variable,
    fields: &[Lowercase],
    field_vars: &[Variable],
    result_field_vars: &[Variable],
) -> (Expr, Variable, Variable) {
    let state_arg_symbol = env.new_symbol("state_record");

    let mut fields_map = SendMap::default();
    let mut pattern_symbols = Vec::with_capacity(fields.len());
    let decode_err_var = {
        let flat_type = FlatType::TagUnion(
            UnionTags::tag_without_arguments(env.subs, "TooShort".into()),
            TagExt::Any(Variable::EMPTY_TAG_UNION),
        );

        synth_var(env.subs, Content::Structure(flat_type))
    };

    for (field_name, &field_var) in fields.iter().zip(field_vars.iter()) {
        let symbol = env.new_symbol(field_name.as_str());

        pattern_symbols.push(symbol);

        let field_expr = Expr::Var(symbol, field_var);
        let field = Field {
            var: field_var,
            region: Region::zero(),
            loc_expr: Box::new(Loc::at_zero(field_expr)),
        };

        fields_map.insert(field_name.clone(), field);
    }

    // The bottom of the happy path - return the decoded record {f0: a, f1: b} wrapped with
    // "Ok".
    let return_type_var;
    let mut body = {
        let subs = &mut env.subs;
        let record_field_iter = fields
            .iter()
            .zip(field_vars.iter())
            .map(|(field_name, &field_var)| (field_name.clone(), RecordField::Required(field_var)));
        let flat_type = FlatType::Record(
            RecordFields::insert_into_subs(subs, record_field_iter),
            Variable::EMPTY_RECORD,
        );
        let done_record_var = synth_var(subs, Content::Structure(flat_type));
        let done_record = Expr::Record {
            record_var: done_record_var,
            fields: fields_map,
        };

        return_type_var = {
            let flat_type = FlatType::TagUnion(
                UnionTags::for_result(subs, done_record_var, decode_err_var),
                TagExt::Any(Variable::EMPTY_TAG_UNION),
            );

            synth_var(subs, Content::Structure(flat_type))
        };

        Expr::Tag {
            tag_union_var: return_type_var,
            ext_var: env.new_ext_var(ExtensionKind::TagUnion),
            name: "Ok".into(),
            arguments: vec![(done_record_var, Loc::at_zero(done_record))],
        }
    };
    let fmt_arg_symbol = env.new_symbol("fmt");
    let fmt_arg_var = env.subs.fresh_unnamed_flex_var();

    // Unwrap each result in the decoded state
    //
    // when rec.f0 is
    //     Ok f0  -> ...happy path...
    //     Err _ -> Err TooShort
    for (((symbol, field_name), &field_var), &result_field_var) in pattern_symbols
        .iter()
        .rev()
        .zip(fields.iter().rev())
        .zip(field_vars.iter().rev())
        .zip(result_field_vars.iter().rev())
    {
        // [Ok field_var, Err DecodeError]
        //   when rec.f0 is
        //       Err _ ->
        //           when Decode.decode_with [] Decode.decoder fmt is
        //               rec2 -> rec2.result
        //       Ok a -> Ok a
        let (attempt_empty_decode_expr, attempt_empty_decode_var) = attempt_empty_decode_if_missing(
            env,
            DecodingFieldArgs {
                field_name: field_name.clone(),
                field_var,
                state_arg_symbol,
                state_record_var,
                result_field_var,
                decode_err_var,
            },
            fmt_arg_var,
            fmt_arg_symbol,
            symbol,
        );

        // Example: `Ok x -> expr`
        let ok_branch = WhenBranch {
            patterns: vec![WhenBranchPattern {
                pattern: Loc::at_zero(Pattern::AppliedTag {
                    whole_var: attempt_empty_decode_var,
                    ext_var: Variable::EMPTY_TAG_UNION,
                    tag_name: "Ok".into(),
                    arguments: vec![(field_var, Loc::at_zero(Pattern::Identifier(*symbol)))],
                }),
                degenerate: false,
            }],
            value: Loc::at_zero(body),
            guard: None,
            redundant: RedundantMark::known_non_redundant(),
        };

        // Example: `_ -> Err TooShort`
        let err_branch = WhenBranch {
            patterns: vec![WhenBranchPattern {
                pattern: Loc::at_zero(Pattern::Underscore),
                degenerate: false,
            }],
            value: Loc::at_zero(Expr::Tag {
                tag_union_var: return_type_var,
                ext_var: env.new_ext_var(ExtensionKind::TagUnion),
                name: "Err".into(),
                arguments: vec![(
                    decode_err_var,
                    Loc::at_zero(Expr::Tag {
                        tag_union_var: decode_err_var,
                        ext_var: Variable::EMPTY_TAG_UNION,
                        name: "TooShort".into(),
                        arguments: Vec::new(),
                    }),
                )],
            }),
            guard: None,
            redundant: RedundantMark::known_non_redundant(),
        };

        // when
        //    when state_record.f0 is
        //        Ok f0 -> Ok f0
        //        _ ->
        //            when Decode.decode_with [] Decode.decoder fmt is
        //                dec_rec -> dec_rec.result
        // is
        //     _-> TooShort
        //     Ok x -> expr
        body = Expr::When {
            loc_cond: Box::new(Loc::at_zero(attempt_empty_decode_expr)),
            cond_var: attempt_empty_decode_var,
            expr_var: return_type_var,
            region: Region::zero(),
            branches: vec![ok_branch, err_branch],
            branches_cond_var: attempt_empty_decode_var,
            exhaustive: ExhaustiveMark::known_exhaustive(),
        };
    }

    let function_var = synth_var(env.subs, Content::Error); // We'll fix this up in subs later.
    let function_symbol = env.new_symbol("finalizer");
    let lambda_set = LambdaSet {
        solved: UnionLambdas::tag_without_arguments(env.subs, function_symbol),
        recursion_var: OptVariable::NONE,
        unspecialized: Default::default(),
        ambient_function: function_var,
    };
    let closure_type = synth_var(env.subs, Content::LambdaSet(lambda_set));
    let flat_type = FlatType::Func(
        env.subs.insert_into_vars([state_record_var, fmt_arg_var]),
        closure_type,
        return_type_var,
        Variable::PURE,
    );

    // Fix up function_var so it's not Content::Error anymore
    env.subs
        .set_content(function_var, Content::Structure(flat_type));

    let finalizer = Expr::Closure(ClosureData {
        function_type: function_var,
        closure_type,
        return_type: return_type_var,
        fx_type: Variable::PURE,
        early_returns: vec![],
        name: function_symbol,
        captured_symbols: Vec::new(),
        recursive: Recursive::NotRecursive,
        arguments: vec![
            (
                state_record_var,
                AnnotatedMark::known_exhaustive(),
                Loc::at_zero(Pattern::Identifier(state_arg_symbol)),
            ),
            (
                fmt_arg_var,
                AnnotatedMark::known_exhaustive(),
                Loc::at_zero(Pattern::Identifier(fmt_arg_symbol)),
            ),
        ],
        loc_body: Box::new(Loc::at_zero(body)),
    });

    (finalizer, function_var, decode_err_var)
}

/// ```roc
/// when rec.f0 is
///     Err _ ->
///         when Decode.decode_with [] Decode.decoder fmt is
///             dec_rec-> dec_rec.result
///     Ok a -> Ok a
/// ```
/// Tries to decode the field with a zero byte input if it missing,
/// this allows the decoder to decode types that have a state for "missing", such as
/// an "Option" type.
///
/// field_var: The variable of the field in the state record: `Result var NoField`.
/// result_field_var: The variable of the actual field.
fn attempt_empty_decode_if_missing(
    env: &mut Env<'_>,
    DecodingFieldArgs {
        field_name,
        field_var,
        state_arg_symbol,
        state_record_var,
        result_field_var,
        decode_err_var,
    }: DecodingFieldArgs,
    fmt_arg_var: Variable,
    fmt_arg_symbol: Symbol,
    symbol: &Symbol,
) -> (Expr, Variable) {
    let (decode_expr, rec_result, rec_dot_result) = decode_with(
        env,
        field_var,
        empty_list(Variable::U8),
        fmt_arg_var,
        fmt_arg_symbol,
        decode_err_var,
    );
    let decode_when = {
        let decoder_rec_symb = env.new_symbol("dec_rec");

        let branch = WhenBranch {
            patterns: vec![WhenBranchPattern {
                pattern: Loc::at_zero(Pattern::Identifier(decoder_rec_symb)),
                degenerate: false,
            }],
            value: Loc::at_zero(Expr::RecordAccess {
                record_var: rec_result,
                ext_var: env.new_ext_var(ExtensionKind::Record),
                field_var: rec_dot_result,
                loc_expr: Box::new(Loc::at_zero(Expr::Var(decoder_rec_symb, rec_result))),
                field: "result".into(),
            }),
            guard: None,
            redundant: RedundantMark::known_non_redundant(),
        };

        // when Decode.decode_with bytes Decode.decoder fmt is
        Expr::When {
            loc_cond: Box::new(Loc::at_zero(decode_expr)),
            cond_var: rec_result,
            expr_var: rec_dot_result,
            region: Region::zero(),
            branches: vec![branch],
            branches_cond_var: rec_result,
            exhaustive: ExhaustiveMark::known_exhaustive(),
        }
    };

    // when rec.f0 is
    let cond_expr = Expr::RecordAccess {
        record_var: state_record_var,
        ext_var: env.new_ext_var(ExtensionKind::Record),
        field_var: result_field_var,
        loc_expr: Box::new(Loc::at_zero(Expr::Var(state_arg_symbol, state_record_var))),
        field: field_name.clone(),
    };

    // Example: `Ok x -> Ok x`
    let ok_branch = ok_to_ok_branch(result_field_var, rec_dot_result, field_var, symbol, env);

    // Example: `Err NoField -> when decode_with [] decoder #Derived.fmt is`
    let no_field_label = "NoField";
    let union_tags = UnionTags::tag_without_arguments(env.subs, no_field_label.into());
    let no_field_var = synth_var(
        env.subs,
        Content::Structure(FlatType::TagUnion(
            union_tags,
            TagExt::Any(Variable::EMPTY_TAG_UNION),
        )),
    );

    let err_branch = WhenBranch {
        patterns: vec![WhenBranchPattern {
            pattern: Loc::at_zero(Pattern::AppliedTag {
                whole_var: result_field_var,
                ext_var: Variable::EMPTY_TAG_UNION,
                tag_name: "Err".into(),
                arguments: vec![(
                    no_field_var,
                    Loc::at_zero(Pattern::AppliedTag {
                        whole_var: no_field_var,
                        ext_var: Variable::EMPTY_TAG_UNION,
                        tag_name: "NoField".into(),
                        arguments: Vec::new(),
                    }),
                )],
            }),
            degenerate: false,
        }],
        value: Loc::at_zero(decode_when),
        guard: None,
        redundant: RedundantMark::known_non_redundant(),
    };

    let expr = Expr::When {
        loc_cond: Box::new(Loc::at_zero(cond_expr)),
        cond_var: result_field_var,
        expr_var: rec_dot_result,
        region: Region::zero(),
        branches: vec![ok_branch, err_branch],
        branches_cond_var: result_field_var,
        exhaustive: ExhaustiveMark::known_exhaustive(),
    };
    (expr, rec_dot_result)
}

// Example:
// initial_state : {first: Result a [NoField], second: Result b [NoField]}
// initial_state = {first: Err NoField, second: Err NoField}
fn initial_state(
    env: &mut Env<'_>,
    field_names: &[Lowercase],
    field_vars: &mut Vec<Variable>,
    result_field_vars: &mut Vec<Variable>,
) -> (Variable, Expr) {
    let mut initial_state_fields = SendMap::default();

    for field_name in field_names {
        let subs = &mut env.subs;
        let field_var = subs.fresh_unnamed_flex_var();

        field_vars.push(field_var);

        let no_field_label = "NoField";
        let union_tags = UnionTags::tag_without_arguments(subs, no_field_label.into());
        let no_field_var = synth_var(
            subs,
            Content::Structure(FlatType::TagUnion(
                union_tags,
                TagExt::Any(Variable::EMPTY_TAG_UNION),
            )),
        );
        let no_field = Expr::Tag {
            tag_union_var: no_field_var,
            ext_var: Variable::EMPTY_TAG_UNION,
            name: no_field_label.into(),
            arguments: Vec::new(),
        };
        let err_label = "Err";
        let union_tags = UnionTags::for_result(subs, field_var, no_field_var);
        let result_var = synth_var(
            subs,
            Content::Structure(FlatType::TagUnion(
                union_tags,
                TagExt::Any(Variable::EMPTY_TAG_UNION),
            )),
        );
        let field_expr = Expr::Tag {
            tag_union_var: result_var,
            ext_var: env.new_ext_var(ExtensionKind::TagUnion),
            name: err_label.into(),
            arguments: vec![(no_field_var, Loc::at_zero(no_field))],
        };
        result_field_vars.push(result_var);
        let field = Field {
            var: result_var,
            region: Region::zero(),
            loc_expr: Box::new(Loc::at_zero(field_expr)),
        };

        initial_state_fields.insert(field_name.clone(), field);
    }

    let subs = &mut env.subs;
    let record_field_iter = field_names
        .iter()
        .zip(result_field_vars.iter())
        .map(|(field_name, &var)| (field_name.clone(), RecordField::Required(var)));
    let flat_type = FlatType::Record(
        RecordFields::insert_into_subs(subs, record_field_iter),
        Variable::EMPTY_RECORD,
    );

    let state_record_var = synth_var(subs, Content::Structure(flat_type));

    (
        state_record_var,
        Expr::Record {
            record_var: state_record_var,
            fields: initial_state_fields,
        },
    )
}

struct DecodeWithVars {
    /// Type of the record returned by `Decode.decode_with`
    /// `rec : { rest: List U8, result: (typeof rec.result) }`
    rec_var: Variable,
    /// type of the result field of the record returned by `Decode.decode_with`
    rec_dot_result: Variable,
    /// type of `Decode.decoder`
    decoder_var: Variable,
    /// lambda set for `Decode.decode_with` call
    lambda_set_var: Variable,
    /// specialised type of this specific call to `Decode.decode_with`
    this_decode_with_var: Variable,
}

/// Makes the vars for decoding this particular field and decode format.
fn make_decode_with_vars(
    env: &mut Env<'_>,
    field_var: Variable,
    bytes_arg_var: Variable,
    fmt_arg_var: Variable,
    decode_err_var: Variable,
) -> DecodeWithVars {
    // rec.result : [Ok field_var, Err DecodeError]
    let rec_dot_result = {
        let tag_union = FlatType::TagUnion(
            UnionTags::for_result(env.subs, field_var, decode_err_var),
            TagExt::Any(Variable::EMPTY_TAG_UNION),
        );

        synth_var(env.subs, Content::Structure(tag_union))
    };

    // rec : { rest: List U8, result: (typeof rec.result) }
    let rec_var = {
        let fields = RecordFields::insert_into_subs(
            env.subs,
            [
                ("rest".into(), RecordField::Required(Variable::LIST_U8)),
                ("result".into(), RecordField::Required(rec_dot_result)),
            ],
        );
        let record = FlatType::Record(fields, Variable::EMPTY_RECORD);

        synth_var(env.subs, Content::Structure(record))
    };

    // `Decode.decoder` for the field's value
    let decoder_var = env.import_builtin_symbol_var(Symbol::DECODE_DECODER);
    let decode_with_var = env.import_builtin_symbol_var(Symbol::DECODE_DECODE_WITH);
    let lambda_set_var = env.subs.fresh_unnamed_flex_var();
    let this_decode_with_var = {
        let subs_slice = env
            .subs
            .insert_into_vars([bytes_arg_var, decoder_var, fmt_arg_var]);
        let this_decode_with_var = synth_var(
            env.subs,
            Content::Structure(FlatType::Func(
                subs_slice,
                lambda_set_var,
                rec_var,
                Variable::PURE,
            )),
        );

        env.unify(decode_with_var, this_decode_with_var);

        this_decode_with_var
    };

    DecodeWithVars {
        rec_var,
        rec_dot_result,
        decoder_var,
        lambda_set_var,
        this_decode_with_var,
    }
}

/// `Decode.decode_with bytes Decode.decoder fmt`
///
/// Generates a call to decode_with, returns that expression,
/// the variable of the return value `{ rest: List U8, result: (typeof rec.result) }`,
/// and the variable of the result field of the return value `[Ok field_var, Err DecodeError]`.
pub(super) fn decode_with(
    env: &mut Env<'_>,
    field_var: Variable,
    bytes_arg_expr: Expr,
    fmt_arg_var: Variable,
    fmt_arg_symbol: Symbol,
    decode_err_var: Variable,
) -> (Expr, Variable, Variable) {
    // Creates all the vars we need to call decode_with for the specific field and fmt we are going to call it with.
    let DecodeWithVars {
        rec_var,
        rec_dot_result,
        decoder_var,
        lambda_set_var,
        this_decode_with_var,
    } = make_decode_with_vars(
        env,
        field_var,
        Variable::LIST_U8,
        fmt_arg_var,
        decode_err_var,
    );
    let decode_expr = Expr::Call(
        Box::new((
            this_decode_with_var,
            Loc::at_zero(Expr::Var(Symbol::DECODE_DECODE_WITH, this_decode_with_var)),
            lambda_set_var,
            rec_var,
            Variable::PURE,
        )),
        vec![
            (Variable::LIST_U8, Loc::at_zero(bytes_arg_expr)),
            (
                decoder_var,
                Loc::at_zero(Expr::AbilityMember(
                    Symbol::DECODE_DECODER,
                    None,
                    decoder_var,
                )),
            ),
            (
                fmt_arg_var,
                Loc::at_zero(Expr::Var(fmt_arg_symbol, fmt_arg_var)),
            ),
        ],
        CalledVia::Space,
    );
    (decode_expr, rec_var, rec_dot_result)
}
