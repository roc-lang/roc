//! Derivers for the `Decoding` ability.

use roc_can::expr::{AnnotatedMark, ClosureData, Expr, Recursive};
use roc_can::pattern::Pattern;
use roc_derive_key::decoding::FlatDecodableKey;
use roc_error_macros::internal_error;
use roc_module::called_via::CalledVia;
use roc_module::symbol::Symbol;
use roc_region::all::Loc;
use roc_types::subs::{
    Content, FlatType, GetSubsSlice, LambdaSet, OptVariable, SubsSlice, UnionLambdas, Variable,
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
