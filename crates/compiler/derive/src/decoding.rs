//! Derivers for the `Decoding` ability.

use roc_can::expr::{
    AnnotatedMark, ClosureData, Expr, Field, Recursive, WhenBranch, WhenBranchPattern,
};
use roc_can::pattern::Pattern;
use roc_collections::SendMap;
use roc_derive_key::decoding::FlatDecodableKey;
use roc_error_macros::internal_error;
use roc_module::called_via::CalledVia;
use roc_module::ident::Lowercase;
use roc_module::symbol::Symbol;
use roc_region::all::{Loc, Region};
use roc_types::subs::{
    Content, ExhaustiveMark, FlatType, GetSubsSlice, LambdaSet, OptVariable, RecordFields,
    RedundantMark, SubsSlice, UnionLambdas, UnionTags, Variable,
};
use roc_types::types::{AliasKind, RecordField};

use crate::util::Env;
use crate::{synth_var, DerivedBody};

pub(crate) fn derive_decoder(
    env: &mut Env<'_>,
    key: FlatDecodableKey,
    def_symbol: Symbol,
) -> DerivedBody {
    let (body, body_type) = match key {
        FlatDecodableKey::List() => decoder_list(env, def_symbol),
        FlatDecodableKey::Record(fields) => decoder_record(env, def_symbol, fields),
    };

    let specialization_lambda_sets =
        env.get_specialization_lambda_sets(body_type, Symbol::DECODE_DECODER);

    DerivedBody {
        body,
        body_type,
        specialization_lambda_sets,
    }
}

// theDecoder : Decoder {first: a, second: b} fmt | a has Decoding, b has Decoding, fmt has DecoderFormatting
// theDecoder =
//     initialState : {f0: Result a [NoField], f1: Result b [NoField]}
//     initialState = {f0: Err NoField, f1: Err NoField}
//
//     stepField = \state, field ->
//         when field is
//             "first" ->
//                 Keep (Decode.custom \bytes, fmt ->
//                     when Decode.decodeWith bytes Decode.decoder fmt is
//                         {result, rest} ->
//                             {result: Result.map result \val -> {state & f0: Ok val}, rest})
//             "second" ->
//                 Keep (Decode.custom \bytes, fmt ->
//                     when Decode.decodeWith bytes Decode.decoder fmt is
//                         {result, rest} ->
//                             {result: Result.map result \val -> {state & f1: Ok val}, rest})
//             _ -> Skip
//
//     finalizer = \{f0, f1} ->
//         when f0 is
//             Ok first ->
//                 when f1 is
//                     Ok second -> Ok {first, second}
//                     Err NoField -> Err TooShort
//             Err NoField -> Err TooShort
//
//     Decode.custom \bytes, fmt -> Decode.decodeWith bytes (Decode.record initialState stepField finalizer) fmt
fn decoder_record(env: &mut Env, _def_symbol: Symbol, fields: Vec<Lowercase>) -> (Expr, Variable) {
    let mut field_vars = Vec::with_capacity(fields.len());
    let mut result_field_vars = Vec::with_capacity(fields.len());
    let (initial_state_var, initial_state) =
        decoder_initial_state(env, &fields, &mut field_vars, &mut result_field_vars);
    let (finalizer, finalizer_var, decode_err_var) = decoder_finalizer(
        env,
        initial_state_var,
        &fields,
        &field_vars,
        &result_field_vars,
    );
    let (step_field, step_var) = decoder_step_field(
        env,
        fields,
        &field_vars,
        &result_field_vars,
        initial_state_var,
        decode_err_var,
    );

    let record_decoder_var = env.subs.fresh_unnamed_flex_var();
    let decode_record_lambda_set = env.subs.fresh_unnamed_flex_var();
    let decode_record_var = env.import_builtin_symbol_var(Symbol::DECODE_RECORD);
    let this_decode_record_var = {
        let flat_type = FlatType::Func(
            SubsSlice::insert_into_subs(env.subs, [initial_state_var, step_var, finalizer_var]),
            decode_record_lambda_set,
            record_decoder_var,
        );

        synth_var(env.subs, Content::Structure(flat_type))
    };

    env.unify(decode_record_var, this_decode_record_var);

    // Decode.record initialState stepField finalizer
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
        )),
        vec![
            (initial_state_var, Loc::at_zero(initial_state)),
            (step_var, Loc::at_zero(step_field)),
            (finalizer_var, Loc::at_zero(finalizer)),
        ],
        CalledVia::Space,
    );

    let fmt_var = env.subs.fresh_unnamed_flex_var();
    let decode_custom_ret_var = env.subs.fresh_unnamed_flex_var();

    // Decode.custom \bytes, fmt -> Decode.decodeWith bytes (Decode.record initialState stepField finalizer) fmt
    let call_decode_custom = {
        let bytes_arg_symbol = env.new_symbol("bytes");
        let fmt_arg_symbol = env.new_symbol("fmt");

        let decode_with_ret_var = env.subs.fresh_unnamed_flex_var();
        let decode_with_lambda_set = env.subs.fresh_unnamed_flex_var();
        let decode_with_var = env.import_builtin_symbol_var(Symbol::DECODE_DECODE_WITH);
        let this_decode_with_var = {
            let flat_type = FlatType::Func(
                SubsSlice::insert_into_subs(
                    env.subs,
                    [Variable::LIST_U8, record_decoder_var, fmt_var],
                ),
                decode_with_lambda_set,
                decode_with_ret_var,
            );

            synth_var(env.subs, Content::Structure(flat_type))
        };

        env.unify(decode_with_var, this_decode_with_var);

        // Decode.decodeWith bytes (Decode.record initialState stepField finalizer) fmt
        let callback_body = Expr::Call(
            Box::new((
                this_decode_with_var,
                Loc::at_zero(Expr::Var(Symbol::DECODE_DECODE_WITH)),
                decode_with_lambda_set,
                decode_with_ret_var,
            )),
            vec![
                (Variable::LIST_U8, Loc::at_zero(Expr::Var(bytes_arg_symbol))),
                (record_decoder_var, Loc::at_zero(call_decode_record)),
                (fmt_var, Loc::at_zero(Expr::Var(fmt_arg_symbol))),
            ],
            CalledVia::Space,
        );

        let callback_symbol = env.new_symbol("customCallback");
        let callback_var = env.subs.fresh_unnamed_flex_var();
        let callback_lambda_set_var = {
            let lambda_set = LambdaSet {
                solved: UnionLambdas::tag_without_arguments(env.subs, callback_symbol),
                recursion_var: OptVariable::NONE,
                unspecialized: Default::default(),
                ambient_function: callback_var,
            };

            synth_var(env.subs, Content::LambdaSet(lambda_set))
        };

        {
            let flat_type = FlatType::Func(
                SubsSlice::insert_into_subs(env.subs, [Variable::LIST_U8, fmt_var]),
                callback_lambda_set_var,
                decode_with_ret_var,
            );

            env.subs
                .set_content(callback_var, Content::Structure(flat_type));
        }

        // \bytes, fmt -> Decode.decodeWith bytes (Decode.record initialState stepField finalizer) fmt
        let custom_callback = {
            Expr::Closure(ClosureData {
                function_type: callback_var,
                closure_type: callback_lambda_set_var,
                return_type: decode_with_ret_var,
                name: callback_symbol,
                captured_symbols: Vec::new(),
                recursive: Recursive::NotRecursive,
                arguments: vec![
                    (
                        Variable::LIST_U8,
                        AnnotatedMark::known_exhaustive(),
                        Loc::at_zero(Pattern::Identifier(bytes_arg_symbol)),
                    ),
                    (
                        fmt_var,
                        AnnotatedMark::known_exhaustive(),
                        Loc::at_zero(Pattern::Identifier(fmt_arg_symbol)),
                    ),
                ],
                loc_body: Box::new(Loc::at_zero(callback_body)),
            })
        };

        let decode_custom_lambda_set = env.subs.fresh_unnamed_flex_var();
        let decode_custom_var = env.import_builtin_symbol_var(Symbol::DECODE_CUSTOM);
        let this_decode_custom_var = {
            let flat_type = FlatType::Func(
                SubsSlice::insert_into_subs(env.subs, [callback_var]),
                decode_custom_lambda_set,
                decode_custom_ret_var,
            );

            synth_var(env.subs, Content::Structure(flat_type))
        };

        env.unify(decode_custom_var, this_decode_custom_var);

        // Decode.custom \bytes, fmt -> Decode.decodeWith bytes (Decode.record initialState stepField finalizer) fmt
        Expr::Call(
            Box::new((
                this_decode_custom_var,
                Loc::at_zero(Expr::Var(Symbol::DECODE_CUSTOM)),
                decode_custom_lambda_set,
                decode_custom_ret_var,
            )),
            vec![(callback_var, Loc::at_zero(custom_callback))],
            CalledVia::Space,
        )
    };

    (call_decode_custom, decode_custom_ret_var)
}

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
//                         }
//             )
//
//         "second" -> # We actually use the first branch's structure, which is a desugared version of this
//             Keep (Decode.custom \bytes, fmt ->
//                 when Decode.decodeWith bytes Decode.decoder fmt is
//                     {result, rest} ->
//                         {result: Result.map result \val -> {state & second: Ok val}, rest})
//         _ -> Skip
fn decoder_step_field(
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
            Variable::EMPTY_TAG_UNION,
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

        let this_custom_callback_var;
        let custom_callback_ret_var;
        let custom_callback = {
            // \bytes, fmt ->
            //     # Uses a single-branch `when` because `let` is more expensive to monomorphize
            //     # due to checks for polymorphic expressions, and `rec` would be polymorphic.
            //     when Decode.decodeWith bytes Decode.decoder fmt is
            //         rec ->
            //             {
            //                 rest: rec.rest,
            //                 result: when rec.result is
            //                     Ok val -> Ok {state & first: Ok val},
            //                     Err err -> Err err
            //             }
            let bytes_arg_symbol = env.new_symbol("bytes");
            let fmt_arg_symbol = env.new_symbol("fmt");
            let bytes_arg_var = env.subs.fresh_unnamed_flex_var();
            let fmt_arg_var = env.subs.fresh_unnamed_flex_var();
            let rec_var = env.subs.fresh_unnamed_flex_var();
            let decoder_var = env.import_builtin_symbol_var(Symbol::DECODE_DECODER);
            let decode_with_var = env.import_builtin_symbol_var(Symbol::DECODE_DECODE_WITH);
            let lambda_set_var = env.subs.fresh_unnamed_flex_var();
            let this_decode_with_var = {
                let subs_slice = SubsSlice::insert_into_subs(
                    env.subs,
                    [bytes_arg_var, decoder_var, fmt_arg_var],
                );
                let this_decode_with_var = synth_var(
                    env.subs,
                    Content::Structure(FlatType::Func(subs_slice, lambda_set_var, rec_var)),
                );

                env.unify(decode_with_var, this_decode_with_var);

                this_decode_with_var
            };

            let when_expr_var = {
                let flat_type = FlatType::TagUnion(
                    UnionTags::for_result(env.subs, state_record_var, decode_err_var),
                    Variable::EMPTY_TAG_UNION,
                );

                synth_var(env.subs, Content::Structure(flat_type))
            };
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

            let custom_callback_body = {
                let rec_symbol = env.new_symbol("rec");

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
                let branch_body = {
                    // when rec.result is
                    //     Ok val -> Ok {state & first: Ok val},
                    //     Err err -> Err err
                    let rec_dot_result = {
                        let flat_type = FlatType::TagUnion(
                            UnionTags::for_result(env.subs, field_var, decode_err_var),
                            Variable::EMPTY_TAG_UNION,
                        );

                        synth_var(env.subs, Content::Structure(flat_type))
                    };

                    let result_val = {
                        // result: when rec.result is
                        //     Ok val -> Ok {state & first: Ok val},
                        //     Err err -> Err err
                        let ok_val_symbol = env.new_symbol("val");
                        let err_val_symbol = env.new_symbol("err");
                        let ok_branch_expr = {
                            // Ok {state & first: Ok val},
                            let mut updates = SendMap::default();

                            updates.insert(
                                field_name.clone(),
                                Field {
                                    var: result_field_var,
                                    region: Region::zero(),
                                    loc_expr: Box::new(Loc::at_zero(Expr::Tag {
                                        tag_union_var: result_field_var,
                                        ext_var: env.subs.fresh_unnamed_flex_var(),
                                        name: "Ok".into(),
                                        arguments: vec![(
                                            field_var,
                                            Loc::at_zero(Expr::Var(ok_val_symbol)),
                                        )],
                                    })),
                                },
                            );

                            let updated_record = Expr::Update {
                                record_var: state_record_var,
                                ext_var: env.subs.fresh_unnamed_flex_var(),
                                symbol: state_arg_symbol,
                                updates,
                            };

                            Expr::Tag {
                                tag_union_var: when_expr_var,
                                ext_var: env.subs.fresh_unnamed_flex_var(),
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
                                    ext_var: env.subs.fresh_unnamed_flex_var(),
                                    name: "Err".into(),
                                    arguments: vec![(
                                        decode_err_var,
                                        Loc::at_zero(Expr::Var(err_val_symbol)),
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
                            loc_cond: Box::new(Loc::at_zero(Expr::Access {
                                record_var: rec_var,
                                ext_var: env.subs.fresh_unnamed_flex_var(),
                                field_var: rec_dot_result,
                                loc_expr: Box::new(Loc::at_zero(Expr::Var(rec_symbol))),
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
                            loc_expr: Box::new(Loc::at_zero(Expr::Access {
                                record_var: rec_var,
                                ext_var: env.subs.fresh_unnamed_flex_var(),
                                field_var: Variable::LIST_U8,
                                loc_expr: Box::new(Loc::at_zero(Expr::Var(rec_symbol))),
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
                        Loc::at_zero(Expr::Var(Symbol::DECODE_DECODE_WITH)),
                        lambda_set_var,
                        rec_var,
                    )),
                    vec![
                        (Variable::LIST_U8, Loc::at_zero(Expr::Var(bytes_arg_symbol))),
                        (
                            decoder_var,
                            Loc::at_zero(Expr::AbilityMember(
                                Symbol::DECODE_DECODER,
                                None,
                                decoder_var,
                            )),
                        ),
                        (fmt_arg_var, Loc::at_zero(Expr::Var(fmt_arg_symbol))),
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
                let subs_slice =
                    SubsSlice::insert_into_subs(env.subs, [bytes_arg_var, fmt_arg_var]);

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
                    Loc::at_zero(Expr::Var(Symbol::DECODE_CUSTOM)),
                    decode_custom_closure_var,
                    decode_custom_ret_var,
                )),
                vec![(this_custom_callback_var, Loc::at_zero(custom_callback))],
                CalledVia::Space,
            )
        };

        env.unify(keep_payload_var, decode_custom_ret_var);

        let keep = {
            // Keep (Decode.custom \bytes, fmt ->
            //     # Uses a single-branch `when` because `let` is more expensive to monomorphize
            //     # due to checks for polymorphic expressions, and `rec` would be polymorphic.
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
                ext_var: env.subs.fresh_unnamed_flex_var(),
                name: "Keep".into(),
                arguments: vec![(decode_custom_ret_var, Loc::at_zero(decode_custom))],
            }
        };

        let branch = {
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
            ext_var: env.subs.fresh_unnamed_flex_var(),
            name: "Skip".into(),
            arguments: Vec::new(),
        }),
        guard: None,
        redundant: RedundantMark::known_non_redundant(),
    };

    branches.push(default_branch);

    // when field is
    let body = Expr::When {
        loc_cond: Box::new(Loc::at_zero(Expr::Var(field_arg_symbol))),
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

// Example:
// finalizer = \rec ->
//     when rec.first is
//         Ok first ->
//             when f1 is
//                 Ok second -> Ok {first, second}
//                 Err NoField -> Err TooShort
//         Err NoField -> Err TooShort
fn decoder_finalizer(
    env: &mut Env,
    state_record_var: Variable,
    fields: &[Lowercase],
    field_vars: &[Variable],
    result_field_vars: &[Variable],
) -> (Expr, Variable, Variable) {
    let state_arg_symbol = env.new_symbol("stateRecord");
    let mut fields_map = SendMap::default();
    let mut pattern_symbols = Vec::with_capacity(fields.len());
    let decode_err_var = {
        let flat_type = FlatType::TagUnion(
            UnionTags::tag_without_arguments(env.subs, "TooShort".into()),
            Variable::EMPTY_TAG_UNION,
        );

        synth_var(env.subs, Content::Structure(flat_type))
    };

    for (field_name, &field_var) in fields.iter().zip(field_vars.iter()) {
        let symbol = env.new_symbol(field_name.as_str());

        pattern_symbols.push(symbol);

        let field_expr = Expr::Var(symbol);
        let field = Field {
            var: field_var,
            region: Region::zero(),
            loc_expr: Box::new(Loc::at_zero(field_expr)),
        };

        fields_map.insert(field_name.clone(), field);
    }

    let return_type_var;
    let mut body = {
        let subs = &mut env.subs;
        let record_field_iter = fields
            .into_iter()
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
                Variable::EMPTY_TAG_UNION,
            );

            synth_var(subs, Content::Structure(flat_type))
        };

        Expr::Tag {
            tag_union_var: return_type_var,
            ext_var: env.subs.fresh_unnamed_flex_var(),
            name: "Ok".into(),
            arguments: vec![(done_record_var, Loc::at_zero(done_record))],
        }
    };

    for (((symbol, field_name), &field_var), &result_field_var) in pattern_symbols
        .iter()
        .rev()
        .zip(fields.into_iter().rev())
        .zip(field_vars.iter().rev())
        .zip(result_field_vars.iter().rev())
    {
        // when rec.first is
        let cond_expr = Expr::Access {
            record_var: state_record_var,
            ext_var: env.subs.fresh_unnamed_flex_var(),
            field_var: result_field_var,
            loc_expr: Box::new(Loc::at_zero(Expr::Var(state_arg_symbol))),
            field: field_name.clone(),
        };

        // Example: `Ok x -> expr`
        let ok_branch = WhenBranch {
            patterns: vec![WhenBranchPattern {
                pattern: Loc::at_zero(Pattern::AppliedTag {
                    whole_var: result_field_var,
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
                ext_var: env.subs.fresh_unnamed_flex_var(),
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
            cond_var: result_field_var,
            expr_var: return_type_var,
            region: Region::zero(),
            branches: vec![ok_branch, err_branch],
            branches_cond_var: result_field_var,
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
        SubsSlice::insert_into_subs(env.subs, [state_record_var]),
        closure_type,
        return_type_var,
    );

    // Fix up function_var so it's not Content::Error anymore
    env.subs
        .set_content(function_var, Content::Structure(flat_type));

    let finalizer = Expr::Closure(ClosureData {
        function_type: function_var,
        closure_type,
        return_type: return_type_var,
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
// initialState : {first: Result a [NoField], second: Result b [NoField]}
// initialState = {first: Err NoField, second: Err NoField}
fn decoder_initial_state(
    env: &mut Env<'_>,
    field_names: &[Lowercase],
    field_vars: &mut Vec<Variable>,
    result_field_vars: &mut Vec<Variable>,
) -> (Variable, Expr) {
    let subs = &mut env.subs;
    let mut initial_state_fields = SendMap::default();

    for field_name in field_names {
        let field_var = subs.fresh_unnamed_flex_var();

        field_vars.push(field_var);

        let no_field_label = "NoField";
        let union_tags = UnionTags::tag_without_arguments(subs, no_field_label.into());
        let no_field_var = synth_var(
            subs,
            Content::Structure(FlatType::TagUnion(union_tags, Variable::EMPTY_TAG_UNION)),
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
            Content::Structure(FlatType::TagUnion(union_tags, Variable::EMPTY_TAG_UNION)),
        );
        let field_expr = Expr::Tag {
            tag_union_var: result_var,
            ext_var: subs.fresh_unnamed_flex_var(),
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

fn decoder_list(env: &mut Env<'_>, _def_symbol: Symbol) -> (Expr, Variable) {
    // Build
    //
    //   def_symbol : Decoder (List elem) fmt | elem has Decoding, fmt has DecoderFormatting
    //   def_symbol = Decode.custom \bytes, fmt -> Decode.decodeWith bytes (Decode.list Decode.decoder) fmt
    //
    // TODO try to reduce to `Decode.list Decode.decoder`

    use Expr::*;

    // Decode.list Decode.decoder : Decoder (List elem) fmt
    let (decode_list_call, this_decode_list_ret_var) = {
        // List elem
        let elem_var = env.subs.fresh_unnamed_flex_var();

        // Decode.decoder : Decoder elem fmt | elem has Decoding, fmt has EncoderFormatting
        let (elem_decoder, elem_decoder_var) = {
            // build `Decode.decoder : Decoder elem fmt` type
            // Decoder val fmt | val has Decoding, fmt has EncoderFormatting
            let elem_decoder_var = env.import_builtin_symbol_var(Symbol::DECODE_DECODER);

            // set val ~ elem
            let val_var = match env.subs.get_content_without_compacting(elem_decoder_var) {
                Content::Alias(Symbol::DECODE_DECODER_OPAQUE, vars, _, AliasKind::Opaque)
                    if vars.type_variables_len == 2 =>
                {
                    env.subs.get_subs_slice(vars.type_variables())[0]
                }
                _ => internal_error!("Decode.decode not an opaque type"),
            };

            env.unify(val_var, elem_var);

            (
                AbilityMember(Symbol::DECODE_DECODER, None, elem_decoder_var),
                elem_decoder_var,
            )
        };

        // Build `Decode.list Decode.decoder` type
        // Decoder val fmt -[uls]-> Decoder (List val) fmt | fmt has DecoderFormatting
        let decode_list_fn_var = env.import_builtin_symbol_var(Symbol::DECODE_LIST);

        // Decoder elem fmt -a-> b
        let elem_decoder_var_slice = SubsSlice::insert_into_subs(env.subs, [elem_decoder_var]);
        let this_decode_list_clos_var = env.subs.fresh_unnamed_flex_var();
        let this_decode_list_ret_var = env.subs.fresh_unnamed_flex_var();
        let this_decode_list_fn_var = synth_var(
            env.subs,
            Content::Structure(FlatType::Func(
                elem_decoder_var_slice,
                this_decode_list_clos_var,
                this_decode_list_ret_var,
            )),
        );

        //   Decoder val  fmt -[uls]-> Decoder (List val) fmt | fmt has DecoderFormatting
        // ~ Decoder elem fmt -a    -> b
        env.unify(decode_list_fn_var, this_decode_list_fn_var);

        let decode_list_member = AbilityMember(Symbol::DECODE_LIST, None, this_decode_list_fn_var);
        let decode_list_fn = Box::new((
            decode_list_fn_var,
            Loc::at_zero(decode_list_member),
            this_decode_list_clos_var,
            this_decode_list_ret_var,
        ));

        let decode_list_call = Call(
            decode_list_fn,
            vec![(elem_decoder_var, Loc::at_zero(elem_decoder))],
            CalledVia::Space,
        );

        (decode_list_call, this_decode_list_ret_var)
    };

    let bytes_sym = env.new_symbol("bytes");
    let bytes_var = env.subs.fresh_unnamed_flex_var();
    let fmt_sym = env.new_symbol("fmt");
    let fmt_var = env.subs.fresh_unnamed_flex_var();

    // Decode.decodeWith bytes (Decode.list Decode.decoder) fmt : DecodeResult (List elem)
    let (decode_with_call, decode_result_list_elem_var) = {
        // Decode.decodeWith : List U8, Decoder val fmt, fmt -> DecodeResult val | fmt has DecoderFormatting
        let decode_with_type = env.import_builtin_symbol_var(Symbol::DECODE_DECODE_WITH);

        // Decode.decodeWith : bytes, Decoder (List elem) fmt, fmt -> DecoderResult (List val)
        let this_decode_with_var_slice =
            SubsSlice::insert_into_subs(env.subs, [bytes_var, this_decode_list_ret_var, fmt_var]);
        let this_decode_with_clos_var = env.subs.fresh_unnamed_flex_var();
        let this_decode_with_ret_var = env.subs.fresh_unnamed_flex_var();
        let this_decode_with_fn_var = synth_var(
            env.subs,
            Content::Structure(FlatType::Func(
                this_decode_with_var_slice,
                this_decode_with_clos_var,
                this_decode_with_ret_var,
            )),
        );

        //   List U8, Decoder val fmt,         fmt -> DecodeResult val | fmt has DecoderFormatting
        // ~ bytes,   Decoder (List elem) fmt, fmt -> DecoderResult (List val)
        env.unify(decode_with_type, this_decode_with_fn_var);

        let decode_with_var = Var(Symbol::DECODE_DECODE_WITH);
        let decode_with_fn = Box::new((
            this_decode_with_fn_var,
            Loc::at_zero(decode_with_var),
            this_decode_with_clos_var,
            this_decode_with_ret_var,
        ));
        let decode_with_call = Call(
            decode_with_fn,
            vec![
                // bytes (Decode.list Decode.decoder) fmt
                (bytes_var, Loc::at_zero(Var(bytes_sym))),
                (this_decode_list_ret_var, Loc::at_zero(decode_list_call)),
                (fmt_var, Loc::at_zero(Var(fmt_sym))),
            ],
            CalledVia::Space,
        );

        (decode_with_call, this_decode_with_ret_var)
    };

    // \bytes, fmt -> Decode.decodeWith bytes (Decode.list Decode.decoder) fmt
    let (custom_lambda, custom_var) = {
        let fn_name = env.new_symbol("custom");

        // Create fn_var for ambient capture; we fix it up below.
        let fn_var = synth_var(env.subs, Content::Error);

        // -[[fn_name]]->
        let fn_name_labels = UnionLambdas::insert_into_subs(env.subs, [(fn_name, vec![])]);
        let fn_clos_var = synth_var(
            env.subs,
            Content::LambdaSet(LambdaSet {
                solved: fn_name_labels,
                recursion_var: OptVariable::NONE,
                unspecialized: SubsSlice::default(),
                ambient_function: fn_var,
            }),
        );

        // bytes, fmt -[[fn_name]]-> DecoderResult (List elem)
        let args_slice = SubsSlice::insert_into_subs(env.subs, [bytes_var, fmt_var]);
        env.subs.set_content(
            fn_var,
            Content::Structure(FlatType::Func(
                args_slice,
                fn_clos_var,
                decode_result_list_elem_var,
            )),
        );

        // \bytes, fmt -[[fn_name]]-> Decode.decodeWith bytes (Decode.list Decode.decoder) fmt
        let clos = Closure(ClosureData {
            function_type: fn_var,
            closure_type: fn_clos_var,
            return_type: decode_result_list_elem_var,
            name: fn_name,
            captured_symbols: vec![],
            recursive: Recursive::NotRecursive,
            arguments: vec![
                (
                    bytes_var,
                    AnnotatedMark::known_exhaustive(),
                    Loc::at_zero(Pattern::Identifier(bytes_sym)),
                ),
                (
                    fmt_var,
                    AnnotatedMark::known_exhaustive(),
                    Loc::at_zero(Pattern::Identifier(fmt_sym)),
                ),
            ],
            loc_body: Box::new(Loc::at_zero(decode_with_call)),
        });

        (clos, fn_var)
    };

    // Decode.custom \bytes, fmt -> Decode.decodeWith bytes (Decode.list Decode.decoder) fmt
    let (decode_custom_call, decoder_var) = {
        // (List U8, fmt -> DecodeResult val) -> Decoder val fmt | fmt has DecoderFormatting
        let decode_custom_type = env.import_builtin_symbol_var(Symbol::DECODE_CUSTOM);

        // (List U8, fmt -> DecodeResult (List elem)) -> Decoder (List elem) fmt
        let this_decode_custom_args = SubsSlice::insert_into_subs(env.subs, [custom_var]);
        let this_decode_custom_clos_var = env.subs.fresh_unnamed_flex_var();
        let this_decode_custom_ret_var = env.subs.fresh_unnamed_flex_var();
        let this_decode_custom_fn_var = synth_var(
            env.subs,
            Content::Structure(FlatType::Func(
                this_decode_custom_args,
                this_decode_custom_clos_var,
                this_decode_custom_ret_var,
            )),
        );

        //   (List U8, fmt -> DecodeResult val)         -> Decoder val fmt | fmt has DecoderFormatting
        // ~ (List U8, fmt -> DecodeResult (List elem)) -> Decoder (List elem) fmt
        env.unify(decode_custom_type, this_decode_custom_fn_var);

        let decode_custom_var = Var(Symbol::DECODE_CUSTOM);
        let decode_custom_fn = Box::new((
            this_decode_custom_fn_var,
            Loc::at_zero(decode_custom_var),
            this_decode_custom_clos_var,
            this_decode_custom_ret_var,
        ));
        let decode_custom_call = Call(
            decode_custom_fn,
            vec![(custom_var, Loc::at_zero(custom_lambda))],
            CalledVia::Space,
        );

        (decode_custom_call, this_decode_custom_ret_var)
    };

    (decode_custom_call, decoder_var)
}
