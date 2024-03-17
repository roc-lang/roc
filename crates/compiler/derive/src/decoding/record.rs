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

use self::finalizer::finalizer;
use self::stepField::step_field;

use super::wrap_in_decode_custom_decode_with;

mod decodeWith;
mod finalizer;
mod stepField;

/// Implements decoding of a record. For example, for
///
/// ```text
///   {first: a, second: b}
/// ```
///
/// we'd like to generate an impl like
///
/// ```roc
/// decoder : Decoder {first: a, second: b} fmt where a implements Decoding, b implements Decoding, fmt implements DecoderFormatting
/// decoder =
///     initialState : {f0: Result a [NoField], f1: Result b [NoField]}
///     initialState = {f0: Err NoField, f1: Err NoField}
///
///     stepField = \state, field ->
///         when field is
///             "first" ->
///                 Keep (Decode.custom \bytes, fmt ->
///                     when Decode.decodeWith bytes Decode.decoder fmt is
///                         {result, rest} ->
///                             {result: Result.map result \val -> {state & f0: Ok val}, rest})
///             "second" ->
///                 Keep (Decode.custom \bytes, fmt ->
///                     when Decode.decodeWith bytes Decode.decoder fmt is
///                         {result, rest} ->
///                             {result: Result.map result \val -> {state & f1: Ok val}, rest})
///             _ -> Skip
///
///     finalizer = \{f0, f1} ->
///         when f0 is
///             Ok first ->
///                 when f1 is
///                     Ok second -> Ok {first, second}
///                     Err NoField -> Err TooShort
///             Err NoField -> Err TooShort
///
///     Decode.custom \bytes, fmt -> Decode.decodeWith bytes (Decode.record initialState stepField finalizer) fmt
/// ```
pub(crate) fn decoder(
    env: &mut Env,
    _def_symbol: Symbol,
    fields: Vec<Lowercase>,
) -> (Expr, Variable) {
    // The decoded type of each field in the record, e.g. {first: a, second: b}.
    let mut field_vars = Vec::with_capacity(fields.len());
    // The type of each field in the decoding state, e.g. {first: Result a [NoField], second: Result b [NoField]}
    let mut result_field_vars = Vec::with_capacity(fields.len());

    // initialState = ...
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

    // stepField = ...
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
// initialState : {first: Result a [NoField], second: Result b [NoField]}
// initialState = {first: Err NoField, second: Err NoField}
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
