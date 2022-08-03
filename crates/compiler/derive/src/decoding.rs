//! Derivers for the `Decoding` ability.

use roc_can::expr::Expr;
use roc_derive_key::decoding::FlatDecodableKey;
use roc_error_macros::internal_error;
use roc_module::called_via::CalledVia;
use roc_module::symbol::Symbol;
use roc_region::all::Loc;
use roc_types::subs::{Content, FlatType, GetSubsSlice, SubsSlice, Variable};
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
    };

    let specialization_lambda_sets =
        env.get_specialization_lambda_sets(body_type, Symbol::DECODE_DECODER);

    DerivedBody {
        body,
        body_type,
        specialization_lambda_sets,
    }
}

fn decoder_list(env: &mut Env<'_>, _def_symbol: Symbol) -> (Expr, Variable) {
    // Build
    //
    //   def_symbol : Decoder (List elem) fmt | elem has Decoding, fmt has DecoderFormatting
    //   def_symbol = Decode.list Decode.decoder
    //
    // TODO try to reduce to `Decode.list Decode.decoder`

    use Expr::*;

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
}
