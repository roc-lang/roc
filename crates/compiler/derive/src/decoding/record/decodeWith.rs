use roc_can::expr::Expr;

use roc_module::called_via::CalledVia;

use roc_module::symbol::Symbol;
use roc_region::all::Loc;
use roc_types::subs::{Content, FlatType, RecordFields, SubsSlice, TagExt, UnionTags, Variable};
use roc_types::types::RecordField;

use crate::synth_var;
use crate::util::Env;

/// Makes the vars for decoding this particular field and decode format
fn make_decode_with_vars(
    env: &mut Env<'_>,
    field_var: Variable,
    bytes_arg_var: Variable,
    fmt_arg_var: Variable,
    decode_err_var: Variable,
) -> (Variable, Variable, Variable, Variable, Variable) {
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
        let subs_slice =
            SubsSlice::insert_into_subs(env.subs, [bytes_arg_var, decoder_var, fmt_arg_var]);
        let this_decode_with_var = synth_var(
            env.subs,
            Content::Structure(FlatType::Func(subs_slice, lambda_set_var, rec_var)),
        );

        env.unify(decode_with_var, this_decode_with_var);

        this_decode_with_var
    };

    (
        rec_var,
        rec_dot_result,
        decoder_var,
        lambda_set_var,
        this_decode_with_var,
    )
}

/// `Decode.decodeWith bytes Decode.decoder fmt`
///
/// Generates a call to decodeWith, returns that expression,
/// the variable of the return value `{ rest: List U8, result: (typeof rec.result) }`,
/// and the variable of the result field of the return value `[Ok field_var, Err DecodeError]`
pub(super) fn decode_with(
    env: &mut Env<'_>,
    field_var: Variable,
    bytes_arg_expr: Expr,
    fmt_arg_var: Variable,
    fmt_arg_symbol: Symbol,
    decode_err_var: Variable,
) -> (Expr, Variable, Variable) {
    // Creates all the vars we need to call decode_with for the specific field and fmt we are going to call it with
    let (decode_rec_var, decode_rec_dot_result, decoder_var, lambda_set_var, this_decode_with_var) =
        make_decode_with_vars(
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
            decode_rec_var,
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
    (decode_expr, decode_rec_var, decode_rec_dot_result)
}
