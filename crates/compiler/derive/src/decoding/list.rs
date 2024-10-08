use roc_can::expr::Expr;

use roc_error_macros::internal_error;
use roc_module::called_via::CalledVia;

use roc_module::symbol::Symbol;
use roc_region::all::Loc;
use roc_types::subs::{Content, FlatType, GetSubsSlice, Variable};
use roc_types::types::AliasKind;

use crate::decoding::wrap_in_decode_custom_decode_with;
use crate::synth_var;
use crate::util::Env;

pub(crate) fn decoder(env: &mut Env<'_>, _def_symbol: Symbol) -> (Expr, Variable) {
    // Build
    //
    //   def_symbol : Decoder (List elem) fmt where elem implements Decoding, fmt implements DecoderFormatting
    //   def_symbol = Decode.custom \bytes, fmt -> Decode.decodeWith bytes (Decode.list Decode.decoder) fmt
    //
    // NB: reduction to `Decode.list Decode.decoder` is not possible to the HRR.

    use Expr::*;

    // Decode.list Decode.decoder : Decoder (List elem) fmt
    let (decode_list_call, this_decode_list_ret_var) = {
        // List elem
        let elem_var = env.subs.fresh_unnamed_flex_var();

        // Decode.decoder : Decoder elem fmt where elem implements Decoding, fmt implements EncoderFormatting
        let (elem_decoder, elem_decoder_var) = {
            // build `Decode.decoder : Decoder elem fmt` type
            // Decoder val fmt where val implements Decoding, fmt implements EncoderFormatting
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
        // Decoder val fmt -[uls]-> Decoder (List val) fmt where fmt implements DecoderFormatting
        let decode_list_fn_var = env.import_builtin_symbol_var(Symbol::DECODE_LIST);

        // Decoder elem fmt -a-> b
        let elem_decoder_var_slice = env.subs.insert_into_vars([elem_decoder_var]);
        let this_decode_list_clos_var = env.subs.fresh_unnamed_flex_var();
        let this_decode_list_ret_var = env.subs.fresh_unnamed_flex_var();
        let this_decode_list_fn_var = synth_var(
            env.subs,
            Content::Structure(FlatType::Func(
                elem_decoder_var_slice,
                this_decode_list_clos_var,
                this_decode_list_ret_var,
                Variable::PURE,
            )),
        );

        //   Decoder val  fmt -[uls]-> Decoder (List val) fmt where fmt implements DecoderFormatting
        // ~ Decoder elem fmt -a    -> b
        env.unify(decode_list_fn_var, this_decode_list_fn_var);

        let decode_list_member = AbilityMember(Symbol::DECODE_LIST, None, this_decode_list_fn_var);
        let decode_list_fn = Box::new((
            decode_list_fn_var,
            Loc::at_zero(decode_list_member),
            this_decode_list_clos_var,
            this_decode_list_ret_var,
            Variable::PURE,
        ));

        let decode_list_call = Call(
            decode_list_fn,
            vec![(elem_decoder_var, Loc::at_zero(elem_decoder))],
            CalledVia::Space,
        );

        (decode_list_call, this_decode_list_ret_var)
    };

    let bytes_sym = env.new_symbol("bytes");
    let fmt_sym = env.new_symbol("fmt");
    let fmt_var = env.subs.fresh_unnamed_flex_var();
    let captures = vec![];

    wrap_in_decode_custom_decode_with(
        env,
        bytes_sym,
        (fmt_sym, fmt_var),
        captures,
        (decode_list_call, this_decode_list_ret_var),
    )
}
