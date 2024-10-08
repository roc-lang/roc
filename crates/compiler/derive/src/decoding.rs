//! Derivers for the `Decoding` ability.

use roc_can::expr::{AnnotatedMark, ClosureData, Expr, Recursive};
use roc_can::pattern::Pattern;

use roc_derive_key::decoding::FlatDecodableKey;
use roc_module::called_via::CalledVia;
use roc_module::symbol::Symbol;
use roc_region::all::Loc;
use roc_types::subs::{
    Content, FlatType, LambdaSet, OptVariable, SubsSlice, UnionLambdas, Variable,
};

use crate::util::Env;
use crate::{synth_var, DerivedBody};

mod list;
mod record;
mod tuple;

pub(crate) fn derive_decoder(
    env: &mut Env<'_>,
    key: FlatDecodableKey,
    def_symbol: Symbol,
) -> DerivedBody {
    let (body, body_type) = match key {
        FlatDecodableKey::List() => list::decoder(env, def_symbol),
        FlatDecodableKey::Record(fields) => record::decoder(env, def_symbol, fields),
        FlatDecodableKey::Tuple(arity) => tuple::decoder(env, def_symbol, arity),
    };

    let specialization_lambda_sets =
        env.get_specialization_lambda_sets(body_type, Symbol::DECODE_DECODER);

    DerivedBody {
        body,
        body_type,
        specialization_lambda_sets,
    }
}

// Wraps `myDecoder` in `Decode.custom \bytes, fmt -> Decode.decodeWith bytes myDecoder fmt`.
//
// Needed to work around the Higher-Region Restriction. See https://github.com/roc-lang/roc/issues/3724.
fn wrap_in_decode_custom_decode_with(
    env: &mut Env,
    bytes: Symbol,
    fmt: (Symbol, Variable),
    sorted_inner_decoder_captures: Vec<(Symbol, Variable)>,
    inner_decoder: (Expr, Variable),
) -> (Expr, Variable) {
    use Expr::*;

    debug_assert!({
        let mut sorted = sorted_inner_decoder_captures.clone();
        sorted.sort_by_key(|(sym, _)| *sym);
        sorted == sorted_inner_decoder_captures
    });

    let (bytes_sym, bytes_var) = (bytes, Variable::LIST_U8);
    let (fmt_sym, fmt_var) = fmt;
    let (inner_decoder, inner_decoder_var) = inner_decoder;

    // Decode.decodeWith bytes inner_decoder fmt : DecodeResult val
    let (decode_with_call, decode_with_result_var) = {
        // Decode.decodeWith : List U8, Decoder val fmt, fmt -> DecodeResult val where fmt implements DecoderFormatting
        let decode_with_type = env.import_builtin_symbol_var(Symbol::DECODE_DECODE_WITH);

        // Decode.decodeWith : bytes, inner_decoder, fmt -> DecoderResult (List val)
        let this_decode_with_var_slice =
            env.subs
                .insert_into_vars([bytes_var, inner_decoder_var, fmt_var]);
        let this_decode_with_clos_var = env.subs.fresh_unnamed_flex_var();
        let this_decode_with_ret_var = env.subs.fresh_unnamed_flex_var();
        let this_decode_with_fn_var = synth_var(
            env.subs,
            Content::Structure(FlatType::Func(
                this_decode_with_var_slice,
                this_decode_with_clos_var,
                this_decode_with_ret_var,
                Variable::PURE,
            )),
        );

        //   List U8, Decoder val fmt,         fmt -> DecodeResult val where fmt implements DecoderFormatting
        // ~ bytes,   Decoder (List elem) fmt, fmt -> DecoderResult (List val)
        env.unify(decode_with_type, this_decode_with_fn_var);

        let decode_with_var = Var(Symbol::DECODE_DECODE_WITH, this_decode_with_fn_var);
        let decode_with_fn = Box::new((
            this_decode_with_fn_var,
            Loc::at_zero(decode_with_var),
            this_decode_with_clos_var,
            this_decode_with_ret_var,
            Variable::PURE,
        ));
        let decode_with_call = Call(
            decode_with_fn,
            vec![
                // bytes inner_decoder fmt
                (bytes_var, Loc::at_zero(Var(bytes_sym, bytes_var))),
                (inner_decoder_var, Loc::at_zero(inner_decoder)),
                (fmt_var, Loc::at_zero(Var(fmt_sym, fmt_var))),
            ],
            CalledVia::Space,
        );

        (decode_with_call, this_decode_with_ret_var)
    };

    // \bytes, fmt -> Decode.decodeWith bytes myDecoder fmt
    let (custom_lambda, custom_var) = {
        let fn_name = env.new_symbol("custom");

        // Create fn_var for ambient capture; we fix it up below.
        let fn_var = synth_var(env.subs, Content::Error);

        // -[[fn_name]]->
        let fn_name_labels = UnionLambdas::insert_into_subs(
            env.subs,
            [(
                fn_name,
                sorted_inner_decoder_captures.iter().map(|(_, var)| *var),
            )],
        );
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
        let args_slice = env.subs.insert_into_vars([bytes_var, fmt_var]);
        env.subs.set_content(
            fn_var,
            Content::Structure(FlatType::Func(
                args_slice,
                fn_clos_var,
                decode_with_result_var,
                Variable::PURE,
            )),
        );

        // \bytes, fmt -[[fn_name]]-> Decode.decodeWith bytes inner_decoder fmt
        let clos = Closure(ClosureData {
            function_type: fn_var,
            closure_type: fn_clos_var,
            return_type: decode_with_result_var,
            fx_type: Variable::PURE,
            early_returns: vec![],
            name: fn_name,
            captured_symbols: sorted_inner_decoder_captures,
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

    // Decode.custom \bytes, fmt -> Decode.decodeWith bytes inner_decoder fmt
    let (decode_custom_call, decoder_var) = {
        // (List U8, fmt -> DecodeResult val) -> Decoder val fmt where fmt implements DecoderFormatting
        let decode_custom_type = env.import_builtin_symbol_var(Symbol::DECODE_CUSTOM);

        // (List U8, fmt -> DecodeResult (List elem)) -> Decoder (List elem) fmt
        let this_decode_custom_args = env.subs.insert_into_vars([custom_var]);
        let this_decode_custom_clos_var = env.subs.fresh_unnamed_flex_var();
        let this_decode_custom_ret_var = env.subs.fresh_unnamed_flex_var();
        let this_decode_custom_fn_var = synth_var(
            env.subs,
            Content::Structure(FlatType::Func(
                this_decode_custom_args,
                this_decode_custom_clos_var,
                this_decode_custom_ret_var,
                Variable::PURE,
            )),
        );

        //   (List U8, fmt -> DecodeResult val)         -> Decoder val fmt where fmt implements DecoderFormatting
        // ~ (List U8, fmt -> DecodeResult (List elem)) -> Decoder (List elem) fmt
        env.unify(decode_custom_type, this_decode_custom_fn_var);

        let decode_custom_var = Var(Symbol::DECODE_CUSTOM, this_decode_custom_fn_var);
        let decode_custom_fn = Box::new((
            this_decode_custom_fn_var,
            Loc::at_zero(decode_custom_var),
            this_decode_custom_clos_var,
            this_decode_custom_ret_var,
            Variable::PURE,
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
