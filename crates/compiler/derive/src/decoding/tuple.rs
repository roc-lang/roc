use roc_can::expr::{
    AnnotatedMark, ClosureData, Expr, Field, IntValue, Recursive, WhenBranch, WhenBranchPattern,
};
use roc_can::num::{IntBound, IntLitWidth};
use roc_can::pattern::Pattern;
use roc_collections::SendMap;
use roc_module::called_via::CalledVia;
use roc_module::ident::Lowercase;
use roc_module::symbol::Symbol;
use roc_region::all::{Loc, Region};
use roc_types::subs::{
    Content, ExhaustiveMark, FlatType, LambdaSet, OptVariable, RecordFields, RedundantMark, TagExt,
    TupleElems, UnionLambdas, UnionTags, Variable,
};
use roc_types::types::RecordField;

use crate::synth_var;
use crate::util::{Env, ExtensionKind};

use super::wrap_in_decode_custom_decode_with;

/// Implements decoding of a tuple. For example, for
///
/// ```text
///   (a, b)
/// ```
///
/// we'd like to generate an impl like
///
/// ```roc
/// decoder : Decoder (a, b) fmt where a implements Decoding, b implements Decoding, fmt implements DecoderFormatting
/// decoder =
///     initialState : {e0: Result a [NoElem], e1: Result b [NoElem]}
///     initialState = {e0: Err NoElem, e1: Err NoElem}
///
///     stepElem = \state, index ->
///         when index is
///             0 ->
///                 Next (Decode.custom \bytes, fmt ->
///                     when Decode.decodeWith bytes Decode.decoder fmt is
///                         {result, rest} ->
///                             {result: Result.map result \val -> {state & e0: Ok val}, rest})
///             1 ->
///                 Next (Decode.custom \bytes, fmt ->
///                     when Decode.decodeWith bytes Decode.decoder fmt is
///                         {result, rest} ->
///                             {result: Result.map result \val -> {state & e1: Ok val}, rest})
///             _ -> TooLong
///
///     finalizer = \st ->
///         when st.e0 is
///             Ok e0 ->
///                 when st.e1 is
///                     Ok e1 -> Ok (e0, e1)
///                     Err NoElem -> Err TooShort
///             Err NoElem -> Err TooShort
///
///     Decode.custom \bytes, fmt -> Decode.decodeWith bytes (Decode.tuple initialState stepElem finalizer) fmt
/// ```
pub(crate) fn decoder(env: &mut Env, _def_symbol: Symbol, arity: u32) -> (Expr, Variable) {
    // The decoded type of each index in the tuple, e.g. (a, b).
    let mut index_vars = Vec::with_capacity(arity as _);
    // The type of each index in the decoding state, e.g. {e0: Result a [NoElem], e1: Result b [NoElem]}
    let mut state_fields = Vec::with_capacity(arity as _);
    let mut state_field_vars = Vec::with_capacity(arity as _);

    // initialState = ...
    let (state_var, initial_state) = initial_state(
        env,
        arity,
        &mut index_vars,
        &mut state_fields,
        &mut state_field_vars,
    );

    // finalizer = ...
    let (finalizer, finalizer_var, decode_err_var) = finalizer(
        env,
        &index_vars,
        state_var,
        &state_fields,
        &state_field_vars,
    );

    // stepElem = ...
    let (step_elem, step_var) = step_elem(
        env,
        &index_vars,
        state_var,
        &state_fields,
        &state_field_vars,
        decode_err_var,
    );

    // Build up the type of `Decode.tuple` we expect
    let tuple_decoder_var = env.subs.fresh_unnamed_flex_var();
    let decode_record_lambda_set = env.subs.fresh_unnamed_flex_var();
    let decode_record_var = env.import_builtin_symbol_var(Symbol::DECODE_TUPLE);
    let this_decode_record_var = {
        let flat_type = FlatType::Func(
            env.subs
                .insert_into_vars([state_var, step_var, finalizer_var]),
            decode_record_lambda_set,
            tuple_decoder_var,
            Variable::PURE,
        );

        synth_var(env.subs, Content::Structure(flat_type))
    };

    env.unify(decode_record_var, this_decode_record_var);

    // Decode.tuple initialState stepElem finalizer
    let call_decode_record = Expr::Call(
        Box::new((
            this_decode_record_var,
            Loc::at_zero(Expr::AbilityMember(
                Symbol::DECODE_TUPLE,
                None,
                this_decode_record_var,
            )),
            decode_record_lambda_set,
            tuple_decoder_var,
            Variable::PURE,
        )),
        vec![
            (state_var, Loc::at_zero(initial_state)),
            (step_var, Loc::at_zero(step_elem)),
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
            (call_decode_record, tuple_decoder_var),
        );

        (decode_custom, decode_custom_var)
    };

    (call_decode_custom, decode_custom_ret_var)
}

// Example:
// stepElem = \state, index ->
//     when index is
//         0 ->
//             Next (Decode.custom \bytes, fmt ->
//                 # Uses a single-branch `when` because `let` is more expensive to monomorphize
//                 # due to checks for polymorphic expressions, and `rec` would be polymorphic.
//                 when Decode.decodeWith bytes Decode.decoder fmt is
//                     rec ->
//                         {
//                             rest: rec.rest,
//                             result: when rec.result is
//                                 Ok val -> Ok {state & e0: Ok val},
//                                 Err err -> Err err
//                         })
//
//         "e1" ->
//             Next (Decode.custom \bytes, fmt ->
//                 when Decode.decodeWith bytes Decode.decoder fmt is
//                     rec ->
//                         {
//                             rest: rec.rest,
//                             result: when rec.result is
//                                 Ok val -> Ok {state & e1: Ok val},
//                                 Err err -> Err err
//                         })
//
//         _ -> TooLong
fn step_elem(
    env: &mut Env,
    index_vars: &[Variable],
    state_record_var: Variable,
    state_fields: &[Lowercase],
    state_field_vars: &[Variable],
    decode_err_var: Variable,
) -> (Expr, Variable) {
    let state_arg_symbol = env.new_symbol("stateRecord");
    let index_arg_symbol = env.new_symbol("index");

    // +1 because of the default branch.
    let mut branches = Vec::with_capacity(index_vars.len() + 1);
    let keep_payload_var = env.subs.fresh_unnamed_flex_var();
    let keep_or_skip_var = {
        let keep_payload_subs_slice = env.subs.insert_into_vars([keep_payload_var]);
        let flat_type = FlatType::TagUnion(
            UnionTags::insert_slices_into_subs(
                env.subs,
                [
                    ("Next".into(), keep_payload_subs_slice),
                    ("TooLong".into(), Default::default()),
                ],
            ),
            TagExt::Any(Variable::EMPTY_TAG_UNION),
        );

        synth_var(env.subs, Content::Structure(flat_type))
    };

    for (((index, state_field), &index_var), &result_index_var) in state_fields
        .iter()
        .enumerate()
        .zip(index_vars)
        .zip(state_field_vars)
    {
        // Example:
        // 0 ->
        //     Next (Decode.custom \bytes, fmt ->
        //         when Decode.decodeWith bytes Decode.decoder fmt is
        //             rec ->
        //                 {
        //                     rest: rec.rest,
        //                     result: when rec.result is
        //                         Ok val -> Ok {state & e0: Ok val},
        //                         Err err -> Err err
        //                 }
        //     )

        let this_custom_callback_var;
        let custom_callback_ret_var;
        let custom_callback = {
            // \bytes, fmt ->
            //     when Decode.decodeWith bytes Decode.decoder fmt is
            //         rec ->
            //             {
            //                 rest: rec.rest,
            //                 result: when rec.result is
            //                     Ok val -> Ok {state & e0: Ok val},
            //                     Err err -> Err err
            //             }
            let bytes_arg_symbol = env.new_symbol("bytes");
            let fmt_arg_symbol = env.new_symbol("fmt");
            let bytes_arg_var = env.subs.fresh_unnamed_flex_var();
            let fmt_arg_var = env.subs.fresh_unnamed_flex_var();

            // rec.result : [Ok index_var, Err DecodeError]
            let rec_dot_result = {
                let tag_union = FlatType::TagUnion(
                    UnionTags::for_result(env.subs, index_var, decode_err_var),
                    TagExt::Any(Variable::EMPTY_TAG_UNION),
                );

                synth_var(env.subs, Content::Structure(tag_union))
            };

            // rec : { rest: List U8, result: (typeof rec.result) }
            let rec_var = {
                let indexs = RecordFields::insert_into_subs(
                    env.subs,
                    [
                        ("rest".into(), RecordField::Required(Variable::LIST_U8)),
                        ("result".into(), RecordField::Required(rec_dot_result)),
                    ],
                );
                let record = FlatType::Record(indexs, Variable::EMPTY_RECORD);

                synth_var(env.subs, Content::Structure(record))
            };

            // `Decode.decoder` for the index's value
            let decoder_var = env.import_builtin_symbol_var(Symbol::DECODE_DECODER);
            let decode_with_var = env.import_builtin_symbol_var(Symbol::DECODE_DECODE_WITH);
            let lambda_set_var = env.subs.fresh_unnamed_flex_var();
            let this_decode_with_var = {
                let subs_slice =
                    env.subs
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

            // The result of decoding this index's value - either the updated state, or a decoding error.
            let when_expr_var = {
                let flat_type = FlatType::TagUnion(
                    UnionTags::for_result(env.subs, state_record_var, decode_err_var),
                    TagExt::Any(Variable::EMPTY_TAG_UNION),
                );

                synth_var(env.subs, Content::Structure(flat_type))
            };

            // What our decoder passed to `Decode.custom` returns - the result of decoding the
            // index's value, and the remaining bytes.
            custom_callback_ret_var = {
                let rest_index = RecordField::Required(Variable::LIST_U8);
                let result_index = RecordField::Required(when_expr_var);
                let flat_type = FlatType::Record(
                    RecordFields::insert_into_subs(
                        env.subs,
                        [("rest".into(), rest_index), ("result".into(), result_index)],
                    ),
                    Variable::EMPTY_RECORD,
                );

                synth_var(env.subs, Content::Structure(flat_type))
            };

            let custom_callback_body = {
                let rec_symbol = env.new_symbol("rec");

                // # Uses a single-branch `when` because `let` is more expensive to monomorphize
                // # due to checks for polymorphic expressions, and `rec` would be polymorphic.
                // when Decode.decodeWith bytes Decode.decoder fmt is
                //     rec ->
                //         {
                //             rest: rec.rest,
                //             result: when rec.result is
                //                 Ok val -> Ok {state & e0: Ok val},
                //                 Err err -> Err err
                //         }
                let branch_body = {
                    let result_val = {
                        // result: when rec.result is
                        //     Ok val -> Ok {state & e0: Ok val},
                        //     Err err -> Err err
                        let ok_val_symbol = env.new_symbol("val");
                        let err_val_symbol = env.new_symbol("err");
                        let ok_branch_expr = {
                            // Ok {state & e0: Ok val},
                            let mut updates = SendMap::default();

                            updates.insert(
                                state_field.clone(),
                                Field {
                                    var: result_index_var,
                                    region: Region::zero(),
                                    loc_expr: Box::new(Loc::at_zero(Expr::Tag {
                                        tag_union_var: result_index_var,
                                        ext_var: env.new_ext_var(ExtensionKind::TagUnion),
                                        name: "Ok".into(),
                                        arguments: vec![(
                                            index_var,
                                            Loc::at_zero(Expr::Var(ok_val_symbol, index_var)),
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
                            // Ok val -> Ok {state & e0: Ok val},
                            WhenBranch {
                                patterns: vec![WhenBranchPattern {
                                    pattern: Loc::at_zero(Pattern::AppliedTag {
                                        whole_var: rec_dot_result,
                                        ext_var: Variable::EMPTY_TAG_UNION,
                                        tag_name: "Ok".into(),
                                        arguments: vec![(
                                            index_var,
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
                        //     Ok val -> Ok {state & e0: Ok val},
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
                    //         Ok val -> Ok {state & e0: Ok val},
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
                    //     Ok val -> Ok {state & e0: Ok val},
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
                };

                let branch = WhenBranch {
                    patterns: vec![WhenBranchPattern {
                        pattern: Loc::at_zero(Pattern::Identifier(rec_symbol)),
                        degenerate: false,
                    }],
                    value: Loc::at_zero(branch_body),
                    guard: None,
                    redundant: RedundantMark::known_non_redundant(),
                };

                let condition_expr = Expr::Call(
                    Box::new((
                        this_decode_with_var,
                        Loc::at_zero(Expr::Var(Symbol::DECODE_DECODE_WITH, this_decode_with_var)),
                        lambda_set_var,
                        rec_var,
                        Variable::PURE,
                    )),
                    vec![
                        (
                            Variable::LIST_U8,
                            Loc::at_zero(Expr::Var(bytes_arg_symbol, Variable::LIST_U8)),
                        ),
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

                // when Decode.decodeWith bytes Decode.decoder fmt is
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

        env.unify(keep_payload_var, decode_custom_ret_var);

        let keep = {
            // Next (Decode.custom \bytes, fmt ->
            //     when Decode.decodeWith bytes Decode.decoder fmt is
            //         rec ->
            //             {
            //                 rest: rec.rest,
            //                 result: when rec.result is
            //                     Ok val -> Ok {state & e0: Ok val},
            //                     Err err -> Err err
            //             }
            // )
            Expr::Tag {
                tag_union_var: keep_or_skip_var,
                ext_var: env.new_ext_var(ExtensionKind::TagUnion),
                name: "Next".into(),
                arguments: vec![(decode_custom_ret_var, Loc::at_zero(decode_custom))],
            }
        };

        let branch = {
            // 0 ->
            //     Next (Decode.custom \bytes, fmt ->
            //         when Decode.decodeWith bytes Decode.decoder fmt is
            //             rec ->
            //                 {
            //                     rest: rec.rest,
            //                     result: when rec.result is
            //                         Ok val -> Ok {state & e0: Ok val},
            //                         Err err -> Err err
            //                 }
            //     )
            WhenBranch {
                patterns: vec![WhenBranchPattern {
                    pattern: Loc::at_zero(Pattern::IntLiteral(
                        Variable::U64,
                        Variable::UNSIGNED64,
                        index.to_string().into_boxed_str(),
                        IntValue::I128((index as i128).to_ne_bytes()),
                        IntBound::Exact(IntLitWidth::U64),
                    )),
                    degenerate: false,
                }],
                value: Loc::at_zero(keep),
                guard: None,
                redundant: RedundantMark::known_non_redundant(),
            }
        };

        branches.push(branch);
    }

    // Example: `_ -> TooLong`
    let default_branch = WhenBranch {
        patterns: vec![WhenBranchPattern {
            pattern: Loc::at_zero(Pattern::Underscore),
            degenerate: false,
        }],
        value: Loc::at_zero(Expr::Tag {
            tag_union_var: keep_or_skip_var,
            ext_var: env.new_ext_var(ExtensionKind::TagUnion),
            name: "TooLong".into(),
            arguments: Vec::new(),
        }),
        guard: None,
        redundant: RedundantMark::known_non_redundant(),
    };

    branches.push(default_branch);

    // when index is
    let body = Expr::When {
        loc_cond: Box::new(Loc::at_zero(Expr::Var(index_arg_symbol, Variable::U64))),
        cond_var: Variable::U64,
        expr_var: keep_or_skip_var,
        region: Region::zero(),
        branches,
        branches_cond_var: Variable::U64,
        exhaustive: ExhaustiveMark::known_exhaustive(),
    };

    let step_elem_closure = env.new_symbol("stepElem");
    let function_type = env.subs.fresh_unnamed_flex_var();
    let closure_type = {
        let lambda_set = LambdaSet {
            solved: UnionLambdas::tag_without_arguments(env.subs, step_elem_closure),
            recursion_var: OptVariable::NONE,
            unspecialized: Default::default(),
            ambient_function: function_type,
        };

        synth_var(env.subs, Content::LambdaSet(lambda_set))
    };

    {
        let args_slice = env.subs.insert_into_vars([state_record_var, Variable::U64]);

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
        name: step_elem_closure,
        captured_symbols: Vec::new(),
        recursive: Recursive::NotRecursive,
        arguments: vec![
            (
                state_record_var,
                AnnotatedMark::known_exhaustive(),
                Loc::at_zero(Pattern::Identifier(state_arg_symbol)),
            ),
            (
                Variable::U64,
                AnnotatedMark::known_exhaustive(),
                Loc::at_zero(Pattern::Identifier(index_arg_symbol)),
            ),
        ],
        loc_body: Box::new(Loc::at_zero(body)),
    });

    (expr, function_type)
}

// Example:
// finalizer = \rec ->
//     when rec.e0 is
//         Ok e0 ->
//             when rec.e1 is
//                 Ok e1 -> Ok (e0, e1)
//                 Err NoElem -> Err TooShort
//         Err NoElem -> Err TooShort
fn finalizer(
    env: &mut Env,
    index_vars: &[Variable],
    state_record_var: Variable,
    state_fields: &[Lowercase],
    state_field_vars: &[Variable],
) -> (Expr, Variable, Variable) {
    let state_arg_symbol = env.new_symbol("stateRecord");
    let mut tuple_elems = Vec::with_capacity(index_vars.len());
    let mut pattern_symbols = Vec::with_capacity(index_vars.len());
    let decode_err_var = {
        let flat_type = FlatType::TagUnion(
            UnionTags::tag_without_arguments(env.subs, "TooShort".into()),
            TagExt::Any(Variable::EMPTY_TAG_UNION),
        );

        synth_var(env.subs, Content::Structure(flat_type))
    };

    for (i, &index_var) in index_vars.iter().enumerate() {
        let symbol = env.new_symbol(i);

        pattern_symbols.push(symbol);

        let index_expr = Expr::Var(symbol, index_var);

        tuple_elems.push((index_var, Box::new(Loc::at_zero(index_expr))));
    }

    // The bottom of the happy path - return the decoded tuple (a, b) wrapped with
    // "Ok".
    let return_type_var;
    let mut body = {
        let subs = &mut env.subs;
        let tuple_indices_iter = index_vars.iter().copied().enumerate();
        let flat_type = FlatType::Tuple(
            TupleElems::insert_into_subs(subs, tuple_indices_iter),
            Variable::EMPTY_TUPLE,
        );
        let done_tuple_var = synth_var(subs, Content::Structure(flat_type));
        let done_record = Expr::Tuple {
            tuple_var: done_tuple_var,
            elems: tuple_elems,
        };

        return_type_var = {
            let flat_type = FlatType::TagUnion(
                UnionTags::for_result(subs, done_tuple_var, decode_err_var),
                TagExt::Any(Variable::EMPTY_TAG_UNION),
            );

            synth_var(subs, Content::Structure(flat_type))
        };

        Expr::Tag {
            tag_union_var: return_type_var,
            ext_var: env.new_ext_var(ExtensionKind::TagUnion),
            name: "Ok".into(),
            arguments: vec![(done_tuple_var, Loc::at_zero(done_record))],
        }
    };

    // Unwrap each result in the decoded state
    //
    // when rec.e0 is
    //     Ok e0 -> ...happy path...
    //     Err NoElem -> Err TooShort
    for (((symbol, field), &index_var), &result_index_var) in pattern_symbols
        .iter()
        .zip(state_fields)
        .zip(index_vars)
        .zip(state_field_vars)
        .rev()
    {
        // when rec.e0 is
        let cond_expr = Expr::RecordAccess {
            record_var: state_record_var,
            ext_var: env.new_ext_var(ExtensionKind::Record),
            field_var: result_index_var,
            loc_expr: Box::new(Loc::at_zero(Expr::Var(state_arg_symbol, state_record_var))),
            field: field.clone(),
        };

        // Example: `Ok x -> expr`
        let ok_branch = WhenBranch {
            patterns: vec![WhenBranchPattern {
                pattern: Loc::at_zero(Pattern::AppliedTag {
                    whole_var: result_index_var,
                    ext_var: Variable::EMPTY_TAG_UNION,
                    tag_name: "Ok".into(),
                    arguments: vec![(index_var, Loc::at_zero(Pattern::Identifier(*symbol)))],
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

        body = Expr::When {
            loc_cond: Box::new(Loc::at_zero(cond_expr)),
            cond_var: result_index_var,
            expr_var: return_type_var,
            region: Region::zero(),
            branches: vec![ok_branch, err_branch],
            branches_cond_var: result_index_var,
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
        env.subs.insert_into_vars([state_record_var]),
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
        arguments: vec![(
            state_record_var,
            AnnotatedMark::known_exhaustive(),
            Loc::at_zero(Pattern::Identifier(state_arg_symbol)),
        )],
        loc_body: Box::new(Loc::at_zero(body)),
    });

    (finalizer, function_var, decode_err_var)
}

// Example:
// initialState : {e0: Result a [NoElem], e1: Result b [NoElem]}
// initialState = {e0: Err NoElem, e1: Err NoElem}
fn initial_state(
    env: &mut Env<'_>,
    arity: u32,
    index_vars: &mut Vec<Variable>,
    state_fields: &mut Vec<Lowercase>,
    state_field_vars: &mut Vec<Variable>,
) -> (Variable, Expr) {
    let mut initial_state_fields = SendMap::default();

    for i in 0..arity {
        let subs = &mut env.subs;
        let index_var = subs.fresh_unnamed_flex_var();

        index_vars.push(index_var);

        let state_field = Lowercase::from(format!("e{i}"));
        state_fields.push(state_field.clone());

        let no_index_label = "NoElem";
        let union_tags = UnionTags::tag_without_arguments(subs, no_index_label.into());
        let no_index_var = synth_var(
            subs,
            Content::Structure(FlatType::TagUnion(
                union_tags,
                TagExt::Any(Variable::EMPTY_TAG_UNION),
            )),
        );
        let no_index = Expr::Tag {
            tag_union_var: no_index_var,
            ext_var: Variable::EMPTY_TAG_UNION,
            name: no_index_label.into(),
            arguments: Vec::new(),
        };
        let err_label = "Err";
        let union_tags = UnionTags::for_result(subs, index_var, no_index_var);
        let result_var = synth_var(
            subs,
            Content::Structure(FlatType::TagUnion(
                union_tags,
                TagExt::Any(Variable::EMPTY_TAG_UNION),
            )),
        );
        let index_expr = Expr::Tag {
            tag_union_var: result_var,
            ext_var: env.new_ext_var(ExtensionKind::TagUnion),
            name: err_label.into(),
            arguments: vec![(no_index_var, Loc::at_zero(no_index))],
        };
        state_field_vars.push(result_var);
        let index = Field {
            var: result_var,
            region: Region::zero(),
            loc_expr: Box::new(Loc::at_zero(index_expr)),
        };

        initial_state_fields.insert(state_field, index);
    }

    let subs = &mut env.subs;
    let record_index_iter = state_fields
        .iter()
        .zip(state_field_vars.iter())
        .map(|(index_name, &var)| (index_name.clone(), RecordField::Required(var)));
    let flat_type = FlatType::Record(
        RecordFields::insert_into_subs(subs, record_index_iter),
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
