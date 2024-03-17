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
use crate::util::{empty_list, ok_to_ok_branch, Env, ExtensionKind};

use super::decodeWith::decode_with;
use super::wrap_in_decode_custom_decode_with;
// Example:
// finalizer = \rec ->
//     when rec.first is
//         Ok first ->
//             when rec.second is
//                 Ok second -> Ok {first, second}
//                 Err NoField -> Err TooShort
//         Err NoField -> Err TooShort

//THis is my new one
pub(super) fn finalizer(
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

    // The bottom of the happy path - return the decoded record {first: a, second: b} wrapped with
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
    // when rec.first is
    //     Ok first -> ...happy path...
    //     Err NoField -> Err TooShort
    for (((symbol, field_name), &field_var), &result_field_var) in pattern_symbols
        .iter()
        .rev()
        .zip(fields.iter().rev())
        .zip(field_vars.iter().rev())
        .zip(result_field_vars.iter().rev())
    {
        // [Ok field_var,Err DecodeError]
        // when rec.f0 is
        //     Err _ ->
        //         when Decode.decodeWith [] Decode.decoder fmt is
        //             rec2 -> rec2.result
        //     Ok a -> Ok a
        let (attempt_empty_decode_expr, attempt_empty_decode_var) = attempt_empty_decode_if_missing(
            state_record_var,
            env,
            field_var,
            result_field_var,
            state_arg_symbol,
            field_name,
            fmt_arg_var,
            fmt_arg_symbol,
            decode_err_var,
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
        SubsSlice::insert_into_subs(env.subs, [state_record_var, fmt_arg_var]),
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
///         when Decode.decodeWith [] Decode.decoder fmt is
///             rec2 -> rec2.result
///     Ok a -> Ok a
/// ```
/// Tries to decode the field with a zero byte input if it missing,  
/// this allows the decoder to decode types that have a state for "missing", such as
/// an "Option" type.
///
/// field_var: The variable of the field in the state record: `Result var NoField`
/// result_field_var: The variable of the actual field
fn attempt_empty_decode_if_missing(
    state_record_var: Variable,
    env: &mut Env<'_>,
    field_var: Variable,
    result_field_var: Variable,
    state_arg_symbol: Symbol,
    field_name: &Lowercase,
    fmt_arg_var: Variable,
    fmt_arg_symbol: Symbol,
    decode_err_var: Variable,
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
        let decoder_rec_symb = env.new_symbol("decRec");

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

        // when Decode.decodeWith bytes Decode.decoder fmt is
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

    // Example: `_ -> Decode.partial [] Decode.decoder fmt  `
    let err_branch = WhenBranch {
        patterns: vec![WhenBranchPattern {
            pattern: Loc::at_zero(Pattern::Underscore),
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
