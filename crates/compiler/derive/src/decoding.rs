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
    Content, ExhaustiveMark, FlatType, GetSubsSlice, LambdaSet, OptVariable, RedundantMark,
    SubsSlice, UnionLambdas, Variable,
};
use roc_types::types::AliasKind;

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

fn decoder_record(env: &mut Env, _def_symbol: Symbol, fields: Vec<Lowercase>) -> (Expr, Variable) {
    let initial_state = decoder_initial_state(&fields);
    let finalizer = decoder_finalizer(env, &fields);
    let step_field = decoder_step_field(env, fields);
    let expr_var = Variable::NULL; // TODO type of entire expression, namely something like this:
                                   // Decoder {first: a, second: b} fmt | a has Decoding, b has Decoding, fmt has DecoderFormatting

    // Decode.record initialState stepField finalizer
    let call_decode_record = Expr::Call(
        Box::new((
            Variable::NULL, // TODO
            Loc::at_zero(Expr::Var(Symbol::DECODE_RECORD)),
            Variable::NULL, // TODO
            Variable::NULL, // TODO
        )),
        vec![
            (
                Variable::NULL, // TODO
                Loc::at_zero(initial_state),
            ),
            (
                Variable::NULL, // TODO
                Loc::at_zero(step_field),
            ),
            (
                Variable::NULL, // TODO
                Loc::at_zero(finalizer),
            ),
        ],
        CalledVia::Space,
    );

    // Decode.custom \bytes, fmt -> Decode.decodeWith bytes (Decode.record initialState stepField finalizer) fmt
    let call_decode_custom = {
        let bytes_arg_symbol = env.new_symbol("bytes");
        let fmt_arg_symbol = env.new_symbol("fmt");

        // Decode.decodeWith bytes (Decode.record initialState stepField finalizer) fmt
        let callback_body = Expr::Call(
            Box::new((
                Variable::NULL, // TODO
                Loc::at_zero(Expr::Var(Symbol::DECODE_DECODE_WITH)),
                Variable::NULL, // TODO
                Variable::NULL, // TODO
            )),
            vec![
                (
                    Variable::NULL, // TODO
                    Loc::at_zero(Expr::Var(bytes_arg_symbol)),
                ),
                (
                    Variable::NULL, // TODO
                    Loc::at_zero(call_decode_record),
                ),
                (
                    Variable::NULL, // TODO
                    Loc::at_zero(Expr::Var(fmt_arg_symbol)),
                ),
            ],
            CalledVia::Space,
        );

        // \bytes, fmt -> Decode.decodeWith bytes (Decode.record initialState stepField finalizer) fmt
        let custom_callback = {
            Expr::Closure(ClosureData {
                function_type: Variable::NULL, // TODO
                closure_type: Variable::NULL,  // TODO
                return_type: Variable::NULL,   // TODO
                name: env.unique_symbol(),
                captured_symbols: Vec::new(),
                recursive: Recursive::NotRecursive,
                arguments: vec![
                    (
                        Variable::NULL, // TODO
                        AnnotatedMark::known_exhaustive(),
                        Loc::at_zero(Pattern::Identifier(bytes_arg_symbol)),
                    ),
                    (
                        Variable::NULL, // TODO
                        AnnotatedMark::known_exhaustive(),
                        Loc::at_zero(Pattern::Identifier(fmt_arg_symbol)),
                    ),
                ],
                loc_body: Box::new(Loc::at_zero(callback_body)),
            })
        };

        // Decode.custom \bytes, fmt -> Decode.decodeWith bytes (Decode.record initialState stepField finalizer) fmt
        Expr::Call(
            Box::new((
                Variable::NULL, // TODO
                Loc::at_zero(Expr::Var(Symbol::DECODE_CUSTOM)),
                Variable::NULL, // TODO
                Variable::NULL, // TODO
            )),
            vec![(
                Variable::NULL, // TODO
                Loc::at_zero(custom_callback),
            )],
            CalledVia::Space,
        )
    };

    (call_decode_custom, expr_var)
}

/// Example:
/// stepField = \state, field ->
///     when field is
///         "first" ->
///             Keep (Decode.custom \bytes, fmt ->
///                 # Uses a single-branch `when` because `let` is more expensive to monomorphize
///                 # due to checks for polymorphic expressions, and `rec` would be polymorphic.
///                 when Decode.decodeWith bytes Decode.decoder fmt is
///                     rec ->
///                         {
///                             rest: rec.rest,
///                             result: when rec.result is
///                                 Ok val -> Ok {state & first: Ok val},
///                                 Err err -> Err err
///                         }
///             )
///
///         "second" -> # We actually use the first branch's structure, which is a desugared version of this
///             Keep (Decode.custom \bytes, fmt ->
///                 when Decode.decodeWith bytes Decode.decoder fmt is
///                     {result, rest} ->
///                         {result: Result.map result \val -> {state & second: Ok val}, rest})
///         _ -> Skip
fn decoder_step_field(env: &mut Env, fields: Vec<Lowercase>) -> Expr {
    let state_arg_symbol = env.new_symbol("stateRecord");
    let field_arg_symbol = env.new_symbol("field");

    // +1 because of the default branch.
    let mut branches = Vec::with_capacity(fields.len() + 1);

    for field_name in fields {
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
                            var: Variable::NULL, // TODO
                            region: Region::zero(),
                            loc_expr: Box::new(Loc::at_zero(Expr::Access {
                                record_var: Variable::NULL, // TODO
                                ext_var: Variable::EMPTY_RECORD,
                                field_var: Variable::NULL, // TODO
                                loc_expr: Box::new(Loc::at_zero(Expr::Var(rec_symbol))),
                                field: "rest".into(),
                            })),
                        },
                    );

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
                                    var: Variable::NULL, // TODO
                                    region: Region::zero(),
                                    loc_expr: Box::new(Loc::at_zero(Expr::Tag {
                                        tag_union_var: Variable::NULL, // TODO
                                        ext_var: Variable::NULL,       // TODO
                                        name: "Ok".into(),
                                        arguments: vec![(
                                            Variable::NULL, // TODO
                                            Loc::at_zero(Expr::Var(ok_val_symbol)),
                                        )],
                                    })),
                                },
                            );

                            let updated_record = Expr::Update {
                                record_var: Variable::NULL, // TODO
                                ext_var: Variable::EMPTY_RECORD,
                                symbol: state_arg_symbol,
                                updates,
                            };

                            Expr::Tag {
                                tag_union_var: Variable::NULL, // TODO
                                ext_var: Variable::NULL,       // TODO
                                name: "Ok".into(),
                                arguments: vec![(
                                    Variable::NULL, // TODO
                                    Loc::at_zero(updated_record),
                                )],
                            }
                        };

                        let branches = vec![
                            // Ok val -> Ok {state & first: Ok val},
                            WhenBranch {
                                patterns: vec![WhenBranchPattern {
                                    pattern: Loc::at_zero(Pattern::AppliedTag {
                                        whole_var: Variable::NULL, // TODO
                                        ext_var: Variable::EMPTY_TAG_UNION,
                                        tag_name: "Ok".into(),
                                        arguments: vec![(
                                            Variable::NULL, // TODO
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
                                        whole_var: Variable::NULL, // TODO
                                        ext_var: Variable::EMPTY_TAG_UNION,
                                        tag_name: "Err".into(),
                                        arguments: vec![(
                                            Variable::NULL, // TODO
                                            Loc::at_zero(Pattern::Identifier(err_val_symbol)),
                                        )],
                                    }),
                                    degenerate: false,
                                }],
                                value: Loc::at_zero(Expr::Tag {
                                    tag_union_var: Variable::NULL, // TODO
                                    ext_var: Variable::NULL,       // TODO
                                    name: "Err".into(),
                                    arguments: vec![(
                                        Variable::NULL, // TODO
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
                                record_var: Variable::NULL, // TODO
                                ext_var: Variable::EMPTY_RECORD,
                                field_var: Variable::NULL, // TODO
                                loc_expr: Box::new(Loc::at_zero(Expr::Var(rec_symbol))),
                                field: "result".into(),
                            })),
                            cond_var: Variable::NULL, // TODO
                            expr_var: Variable::NULL, // TODO
                            region: Region::zero(),
                            branches,
                            branches_cond_var: Variable::NULL, // TODO
                            exhaustive: ExhaustiveMark::known_exhaustive(),
                        }
                    };

                    fields_map.insert(
                        "result".into(),
                        Field {
                            var: Variable::NULL, // TODO
                            region: Region::zero(),
                            loc_expr: Box::new(Loc::at_zero(result_val)),
                        },
                    );

                    Expr::Record {
                        record_var: Variable::NULL, // TODO this is the type of the entire record
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
                        Variable::NULL, // TODO
                        Loc::at_zero(Expr::Var(Symbol::DECODE_DECODE_WITH)),
                        Variable::NULL, // TODO
                        Variable::NULL, // TODO
                    )),
                    vec![
                        (
                            Variable::NULL, // TODO
                            Loc::at_zero(Expr::Var(bytes_arg_symbol)),
                        ),
                        (
                            Variable::NULL, // TODO
                            Loc::at_zero(Expr::Var(Symbol::DECODE_DECODER)),
                        ),
                        (
                            Variable::NULL, // TODO
                            Loc::at_zero(Expr::Var(fmt_arg_symbol)),
                        ),
                    ],
                    CalledVia::Space,
                );

                Expr::When {
                    loc_cond: Box::new(Loc::at_zero(condition_expr)),
                    cond_var: Variable::NULL, // TODO
                    expr_var: Variable::NULL, // TODO
                    region: Region::zero(),
                    branches: vec![branch],
                    branches_cond_var: Variable::NULL, // TODO
                    exhaustive: ExhaustiveMark::known_exhaustive(),
                }
            };

            Expr::Closure(ClosureData {
                function_type: Variable::NULL, // TODO
                closure_type: Variable::NULL,  // TODO
                return_type: Variable::NULL,   // TODO
                name: env.unique_symbol(),
                captured_symbols: vec![(
                    state_arg_symbol,
                    Variable::NULL, // TODO
                )],
                recursive: Recursive::NotRecursive,
                arguments: vec![
                    (
                        Variable::NULL, // TODO
                        AnnotatedMark::known_exhaustive(),
                        Loc::at_zero(Pattern::Identifier(bytes_arg_symbol)),
                    ),
                    (
                        Variable::NULL, // TODO
                        AnnotatedMark::known_exhaustive(),
                        Loc::at_zero(Pattern::Identifier(fmt_arg_symbol)),
                    ),
                ],
                loc_body: Box::new(Loc::at_zero(custom_callback_body)),
            })
        };

        let decode_custom = {
            // Decode.custom \bytes, fmt ->
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
            Expr::Call(
                Box::new((
                    Variable::NULL, // TODO
                    Loc::at_zero(Expr::Var(Symbol::DECODE_CUSTOM)),
                    Variable::NULL, // TODO
                    Variable::NULL, // TODO
                )),
                vec![(
                    Variable::NULL, // TODO
                    Loc::at_zero(custom_callback),
                )],
                CalledVia::Space,
            )
        };

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
                tag_union_var: Variable::NULL, // TODO
                ext_var: Variable::EMPTY_TAG_UNION,
                name: "Keep".into(),
                arguments: vec![(
                    Variable::NULL, // TODO
                    Loc::at_zero(decode_custom),
                )],
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
            tag_union_var: Variable::NULL, // TODO
            ext_var: Variable::EMPTY_TAG_UNION,
            name: "Skip".into(),
            arguments: Vec::new(),
        }),
        guard: None,
        redundant: RedundantMark::known_non_redundant(),
    };

    branches.push(default_branch);

    let body = Expr::When {
        loc_cond: Box::new(Loc::at_zero(Expr::Var(field_arg_symbol))),
        cond_var: Variable::NULL, // TODO
        expr_var: Variable::NULL, // TODO
        region: Region::zero(),
        branches,
        branches_cond_var: Variable::NULL, // TODO
        exhaustive: ExhaustiveMark::known_exhaustive(),
    };

    Expr::Closure(ClosureData {
        function_type: Variable::NULL, // TODO
        closure_type: Variable::NULL,  // TODO
        return_type: Variable::NULL,   // TODO
        name: env.unique_symbol(),
        captured_symbols: Vec::new(),
        recursive: Recursive::NotRecursive,
        arguments: vec![
            (
                Variable::NULL, // TODO
                AnnotatedMark::known_exhaustive(),
                Loc::at_zero(Pattern::Identifier(state_arg_symbol)),
            ),
            (
                Variable::NULL, // TODO
                AnnotatedMark::known_exhaustive(),
                Loc::at_zero(Pattern::Identifier(field_arg_symbol)),
            ),
        ],
        loc_body: Box::new(Loc::at_zero(body)),
    })
}

/// Example:
/// finalizer = \{f0, f1} ->
///     when f0 is
///         Ok first ->
///             when f1 is
///                 Ok second -> Ok {first, second}
///                 Err NoField -> Err TooShort
///         Err NoField -> Err TooShort
fn decoder_finalizer(env: &mut Env, fields: &[Lowercase]) -> Expr {
    let state_arg_symbol = env.new_symbol("stateRecord");
    let mut fields_map = SendMap::default();
    let mut pattern_symbols = Vec::with_capacity(fields.len());

    for field_name in fields.iter() {
        let symbol = env.new_symbol(field_name.as_str());

        pattern_symbols.push(symbol);

        let field_expr = Expr::Var(symbol);
        let var = Variable::NULL; // TODO
        let field = Field {
            var,
            region: Region::zero(),
            loc_expr: Box::new(Loc::at_zero(field_expr)),
        };

        fields_map.insert(field_name.clone(), field);
    }

    let mut body = {
        let done_record = Expr::Record {
            record_var: Variable::NULL, // TODO this is the type of the entire record
            fields: fields_map,
        };
        let variant_var = Variable::NULL; // TODO
        let done_var = Variable::NULL; // TODO

        Expr::Tag {
            tag_union_var: variant_var,
            ext_var: Variable::EMPTY_TAG_UNION,
            name: "Ok".into(),
            arguments: vec![(done_var, Loc::at_zero(done_record))],
        }
    };

    for (symbol, field_name) in pattern_symbols.iter().rev().zip(fields.into_iter().rev()) {
        let cond_expr = Expr::Access {
            record_var: Variable::NULL, // TODO
            ext_var: Variable::EMPTY_RECORD,
            field_var: Variable::NULL, // TODO
            loc_expr: Box::new(Loc::at_zero(Expr::Var(state_arg_symbol))),
            field: field_name.clone(),
        };

        // Example: `Ok x -> expr`
        let ok_branch = WhenBranch {
            patterns: vec![WhenBranchPattern {
                pattern: Loc::at_zero(Pattern::AppliedTag {
                    whole_var: Variable::NULL, // TODO
                    ext_var: Variable::NULL,   // TODO
                    tag_name: "Ok".into(),
                    arguments: vec![(
                        Variable::NULL, // TODO
                        Loc::at_zero(Pattern::Identifier(*symbol)),
                    )],
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
                tag_union_var: Variable::NULL, // TODO
                ext_var: Variable::EMPTY_TAG_UNION,
                name: "Err".into(),
                arguments: vec![(
                    Variable::NULL, // TODO
                    Loc::at_zero(Expr::Tag {
                        tag_union_var: Variable::NULL, // TODO
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
            cond_var: Variable::NULL, // TODO
            expr_var: Variable::NULL, // TODO
            region: Region::zero(),
            branches: vec![ok_branch, err_branch],
            branches_cond_var: Variable::NULL, // TODO
            exhaustive: ExhaustiveMark::known_exhaustive(),
        };
    }

    Expr::Closure(ClosureData {
        function_type: Variable::NULL, // TODO
        closure_type: Variable::NULL,  // TODO
        return_type: Variable::NULL,   // TODO
        name: env.unique_symbol(),
        captured_symbols: Vec::new(),
        recursive: Recursive::NotRecursive,
        arguments: vec![(
            Variable::NULL, // TODO
            AnnotatedMark::known_exhaustive(),
            Loc::at_zero(Pattern::Identifier(state_arg_symbol)),
        )],
        loc_body: Box::new(Loc::at_zero(body)),
    })
}

/// Example:
/// initialState : {first: Result a [NoField], second: Result b [NoField]}
/// initialState = {first: Err NoField, second: Err NoField}
fn decoder_initial_state(fields: &[Lowercase]) -> Expr {
    let mut initial_state_fields = SendMap::default();
    for field_name in fields {
        let var = Variable::NULL; // TODO this is the type of the record field
        let variant_var = Variable::NULL; // TODO
        let no_field = Expr::Tag {
            variant_var,
            ext_var: Variable::EMPTY_TAG_UNION,
            name: "NoField".into(),
            arguments: Vec::new(),
        };
        let no_field_var = Variable::NULL; // TODO
        let variant_var = Variable::NULL; // TODO
        let field_expr = Expr::Tag {
            variant_var,
            ext_var: Variable::EMPTY_TAG_UNION,
            name: "Err".into(),
            arguments: vec![(no_field_var, Loc::at_zero(no_field))],
        };
        let field = Field {
            var,
            region: Region::zero(),
            loc_expr: Box::new(Loc::at_zero(field_expr)),
        };

        initial_state_fields.insert(field_name.clone(), field);
    }

    Expr::Record {
        record_var: Variable::NULL, // TODO this is the type of the entire record
        fields: initial_state_fields,
    }
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
        let args_slice = SubsSlice::insert_into_subs(env.subs, vec![bytes_var, fmt_var]);
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
