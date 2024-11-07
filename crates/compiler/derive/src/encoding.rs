//! Derivers for the `Encoding` ability.

use std::iter::once;

use roc_can::expr::{
    AnnotatedMark, ClosureData, Expr, Field, Recursive, WhenBranch, WhenBranchPattern,
};
use roc_can::pattern::Pattern;
use roc_collections::SendMap;
use roc_derive_key::encoding::FlatEncodableKey;
use roc_module::called_via::CalledVia;
use roc_module::ident::Lowercase;
use roc_module::symbol::Symbol;
use roc_region::all::{Loc, Region};
use roc_types::subs::{
    Content, ExhaustiveMark, FlatType, GetSubsSlice, LambdaSet, OptVariable, RecordFields,
    RedundantMark, SubsSlice, TagExt, TupleElems, UnionLambdas, UnionTags, Variable,
    VariableSubsSlice,
};
use roc_types::types::RecordField;

use crate::util::Env;
use crate::{synth_var, DerivedBody};

pub(crate) fn derive_to_encoder(
    env: &mut Env<'_>,
    key: FlatEncodableKey,
    def_symbol: Symbol,
) -> DerivedBody {
    let (body, body_type) = match key {
        FlatEncodableKey::List() => to_encoder_list(env, def_symbol),
        FlatEncodableKey::Set() => todo!(),
        FlatEncodableKey::Dict() => todo!(),
        FlatEncodableKey::Record(fields) => {
            // Generalized record var so we can reuse this impl between many records:
            // if fields = { a, b }, this is { a: t1, b: t2 } for fresh t1, t2.
            let flex_fields = fields
                .into_iter()
                .map(|name| {
                    (
                        name,
                        RecordField::Required(env.subs.fresh_unnamed_flex_var()),
                    )
                })
                .collect::<Vec<(Lowercase, _)>>();
            let fields = RecordFields::insert_into_subs(env.subs, flex_fields);
            let record_var = synth_var(
                env.subs,
                Content::Structure(FlatType::Record(fields, Variable::EMPTY_RECORD)),
            );

            to_encoder_record(env, record_var, fields, def_symbol)
        }
        FlatEncodableKey::Tuple(arity) => {
            // Generalized tuple var so we can reuse this impl between many tuples:
            // if arity = n, this is (t1, ..., tn) for fresh t1, ..., tn.
            let flex_elems = (0..arity)
                .map(|idx| (idx as usize, env.subs.fresh_unnamed_flex_var()))
                .collect::<Vec<_>>();
            let elems = TupleElems::insert_into_subs(env.subs, flex_elems);
            let tuple_var = synth_var(
                env.subs,
                Content::Structure(FlatType::Tuple(elems, Variable::EMPTY_TUPLE)),
            );

            to_encoder_tuple(env, tuple_var, elems, def_symbol)
        }
        FlatEncodableKey::TagUnion(tags) => {
            // Generalized tag union var so we can reuse this impl between many unions:
            // if tags = [ A arity=2, B arity=1 ], this is [ A t1 t2, B t3 ] for fresh t1, t2, t3
            let flex_tag_labels = tags
                .into_iter()
                .map(|(label, arity)| {
                    let variables_slice = env.subs.reserve_into_vars(arity.into());
                    for var_index in variables_slice {
                        env.subs[var_index] = env.subs.fresh_unnamed_flex_var();
                    }
                    (label, variables_slice)
                })
                .collect::<Vec<_>>();
            let union_tags = UnionTags::insert_slices_into_subs(env.subs, flex_tag_labels);
            let tag_union_var = synth_var(
                env.subs,
                Content::Structure(FlatType::TagUnion(
                    union_tags,
                    TagExt::Any(Variable::EMPTY_TAG_UNION),
                )),
            );

            to_encoder_tag_union(env, tag_union_var, union_tags, def_symbol)
        }
    };

    let specialization_lambda_sets =
        env.get_specialization_lambda_sets(body_type, Symbol::ENCODE_TO_ENCODER);

    DerivedBody {
        body,
        body_type,
        specialization_lambda_sets,
    }
}

fn to_encoder_list(env: &mut Env<'_>, fn_name: Symbol) -> (Expr, Variable) {
    // Build \lst -> Encode.list lst (\elem -> Encode.toEncoder elem)
    //
    // TODO eta reduce this baby     ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

    use Expr::*;

    let lst_sym = env.new_symbol("lst");
    let elem_sym = env.new_symbol("elem");

    // List elem
    let elem_var = env.subs.fresh_unnamed_flex_var();
    let elem_var_slice = env.subs.insert_into_vars([elem_var]);
    let list_var = synth_var(
        env.subs,
        Content::Structure(FlatType::Apply(Symbol::LIST_LIST, elem_var_slice)),
    );

    // build `toEncoder elem` type
    // val -[uls]-> Encoder fmt where fmt implements EncoderFormatting
    let to_encoder_fn_var = env.import_builtin_symbol_var(Symbol::ENCODE_TO_ENCODER);

    // elem -[clos]-> t1
    let to_encoder_clos_var = env.subs.fresh_unnamed_flex_var(); // clos
    let elem_encoder_var = env.subs.fresh_unnamed_flex_var(); // t1
    let elem_to_encoder_fn_var = synth_var(
        env.subs,
        Content::Structure(FlatType::Func(
            elem_var_slice,
            to_encoder_clos_var,
            elem_encoder_var,
            Variable::PURE,
        )),
    );

    //   val  -[uls]->  Encoder fmt where fmt implements EncoderFormatting
    // ~ elem -[clos]-> t1
    env.unify(to_encoder_fn_var, elem_to_encoder_fn_var);

    // toEncoder : (typeof rcd.a) -[clos]-> Encoder fmt where fmt implements EncoderFormatting
    let to_encoder_var = AbilityMember(Symbol::ENCODE_TO_ENCODER, None, elem_to_encoder_fn_var);
    let to_encoder_fn = Box::new((
        to_encoder_fn_var,
        Loc::at_zero(to_encoder_var),
        to_encoder_clos_var,
        elem_encoder_var,
        Variable::PURE,
    ));

    // toEncoder elem
    let to_encoder_call = Call(
        to_encoder_fn,
        vec![(elem_var, Loc::at_zero(Var(elem_sym, elem_var)))],
        CalledVia::Space,
    );

    // elem -[to_elem_encoder]-> toEncoder elem
    let to_elem_encoder_sym = env.new_symbol("to_elem_encoder");

    // Create fn_var for ambient capture; we fix it up below.
    let to_elem_encoder_fn_var = synth_var(env.subs, Content::Error);

    // -[to_elem_encoder]->
    let to_elem_encoder_labels =
        UnionLambdas::insert_into_subs(env.subs, once((to_elem_encoder_sym, vec![])));
    let to_elem_encoder_lset = synth_var(
        env.subs,
        Content::LambdaSet(LambdaSet {
            solved: to_elem_encoder_labels,
            recursion_var: OptVariable::NONE,
            unspecialized: SubsSlice::default(),
            ambient_function: to_elem_encoder_fn_var,
        }),
    );
    // elem -[to_elem_encoder]-> toEncoder elem
    env.subs.set_content(
        to_elem_encoder_fn_var,
        Content::Structure(FlatType::Func(
            elem_var_slice,
            to_elem_encoder_lset,
            elem_encoder_var,
            Variable::PURE,
        )),
    );

    // \elem -> toEncoder elem
    let to_elem_encoder = Closure(ClosureData {
        function_type: to_elem_encoder_fn_var,
        closure_type: to_elem_encoder_lset,
        return_type: elem_encoder_var,
        fx_type: Variable::PURE,
        early_returns: vec![],
        name: to_elem_encoder_sym,
        captured_symbols: vec![],
        recursive: Recursive::NotRecursive,
        arguments: vec![(
            elem_var,
            AnnotatedMark::known_exhaustive(),
            Loc::at_zero(Pattern::Identifier(elem_sym)),
        )],
        loc_body: Box::new(Loc::at_zero(to_encoder_call)),
    });

    // build `Encode.list lst (\elem -> Encode.toEncoder elem)` type
    // List e, (e -> Encoder fmt) -[uls]-> Encoder fmt where fmt implements EncoderFormatting
    let encode_list_fn_var = env.import_builtin_symbol_var(Symbol::ENCODE_LIST);

    // List elem, to_elem_encoder_fn_var -[clos]-> t1
    let this_encode_list_args_slice = env
        .subs
        .insert_into_vars([list_var, to_elem_encoder_fn_var]);
    let this_encode_list_clos_var = env.subs.fresh_unnamed_flex_var(); // clos
    let this_list_encoder_var = env.subs.fresh_unnamed_flex_var(); // t1
    let this_encode_list_fn_var = synth_var(
        env.subs,
        Content::Structure(FlatType::Func(
            this_encode_list_args_slice,
            this_encode_list_clos_var,
            this_list_encoder_var,
            Variable::PURE,
        )),
    );

    //   List e,    (e -> Encoder fmt)     -[uls]->  Encoder fmt where fmt implements EncoderFormatting
    // ~ List elem, to_elem_encoder_fn_var -[clos]-> t1
    env.unify(encode_list_fn_var, this_encode_list_fn_var);

    // Encode.list : List elem, to_elem_encoder_fn_var -[clos]-> Encoder fmt where fmt implements EncoderFormatting
    let encode_list = AbilityMember(Symbol::ENCODE_LIST, None, this_encode_list_fn_var);
    let encode_list_fn = Box::new((
        this_encode_list_fn_var,
        Loc::at_zero(encode_list),
        this_encode_list_clos_var,
        this_list_encoder_var,
        Variable::PURE,
    ));

    // Encode.list lst to_elem_encoder
    let encode_list_call = Call(
        encode_list_fn,
        vec![
            (list_var, Loc::at_zero(Var(lst_sym, list_var))),
            (to_elem_encoder_fn_var, Loc::at_zero(to_elem_encoder)),
        ],
        CalledVia::Space,
    );

    // Encode.custom \bytes, fmt -> Encode.appendWith bytes (Encode.list ..) fmt
    let (body, this_encoder_var) = wrap_in_encode_custom(
        env,
        encode_list_call,
        this_list_encoder_var,
        lst_sym,
        list_var,
    );

    // \lst -> Encode.list lst (\elem -> Encode.toEncoder elem)
    // Create fn_var for ambient capture; we fix it up below.
    let fn_var = synth_var(env.subs, Content::Error);

    // -[fn_name]->
    let fn_name_labels = UnionLambdas::insert_into_subs(env.subs, once((fn_name, vec![])));
    let fn_clos_var = synth_var(
        env.subs,
        Content::LambdaSet(LambdaSet {
            solved: fn_name_labels,
            recursion_var: OptVariable::NONE,
            unspecialized: SubsSlice::default(),
            ambient_function: fn_var,
        }),
    );
    // List elem -[fn_name]-> Encoder fmt
    let list_var_slice = env.subs.insert_into_vars(once(list_var));
    env.subs.set_content(
        fn_var,
        Content::Structure(FlatType::Func(
            list_var_slice,
            fn_clos_var,
            this_encoder_var,
            Variable::PURE,
        )),
    );

    // \lst -[fn_name]-> Encode.list lst (\elem -> Encode.toEncoder elem)
    let clos = Closure(ClosureData {
        function_type: fn_var,
        closure_type: fn_clos_var,
        return_type: this_encoder_var,
        fx_type: Variable::PURE,
        early_returns: vec![],
        name: fn_name,
        captured_symbols: vec![],
        recursive: Recursive::NotRecursive,
        arguments: vec![(
            list_var,
            AnnotatedMark::known_exhaustive(),
            Loc::at_zero(Pattern::Identifier(lst_sym)),
        )],
        loc_body: Box::new(Loc::at_zero(body)),
    });

    (clos, fn_var)
}

fn to_encoder_record(
    env: &mut Env<'_>,
    record_var: Variable,
    fields: RecordFields,
    fn_name: Symbol,
) -> (Expr, Variable) {
    // Suppose rcd = { a: t1, b: t2 }. Build
    //
    // \rcd -> Encode.record [
    //      { key: "a", value: Encode.toEncoder rcd.a },
    //      { key: "b", value: Encode.toEncoder rcd.b },
    //   ]

    let rcd_sym = env.new_symbol("rcd");
    let whole_rcd_var = env.subs.fresh_unnamed_flex_var(); // type of the { key, value } records in the list

    use Expr::*;

    let fields_list = fields
        .iter_all()
        .map(|(field_name_index, field_var_index, _)| {
            let field_name = env.subs[field_name_index].clone();
            let field_var = env.subs[field_var_index];
            let field_var_slice = VariableSubsSlice::new(field_var_index.index() as u32, 1);

            // key: "a"
            let key_field = Field {
                var: Variable::STR,
                region: Region::zero(),
                loc_expr: Box::new(Loc::at_zero(Str(field_name.as_str().into()))),
            };

            // rcd.a
            let field_access = RecordAccess {
                record_var,
                ext_var: env.subs.fresh_unnamed_flex_var(),
                field_var,
                loc_expr: Box::new(Loc::at_zero(Var(
                    rcd_sym,
                    env.subs.fresh_unnamed_flex_var(),
                ))),
                field: field_name,
            };

            // build `toEncoder rcd.a` type
            // val -[uls]-> Encoder fmt where fmt implements EncoderFormatting
            let to_encoder_fn_var = env.import_builtin_symbol_var(Symbol::ENCODE_TO_ENCODER);

            // (typeof rcd.a) -[clos]-> t1
            let to_encoder_clos_var = env.subs.fresh_unnamed_flex_var(); // clos
            let encoder_var = env.subs.fresh_unnamed_flex_var(); // t1
            let this_to_encoder_fn_var = synth_var(
                env.subs,
                Content::Structure(FlatType::Func(
                    field_var_slice,
                    to_encoder_clos_var,
                    encoder_var,
                    Variable::PURE,
                )),
            );

            //   val            -[uls]->  Encoder fmt where fmt implements EncoderFormatting
            // ~ (typeof rcd.a) -[clos]-> t1
            env.unify(to_encoder_fn_var, this_to_encoder_fn_var);

            // toEncoder : (typeof rcd.a) -[clos]-> Encoder fmt where fmt implements EncoderFormatting
            let to_encoder_var = AbilityMember(Symbol::ENCODE_TO_ENCODER, None, to_encoder_fn_var);
            let to_encoder_fn = Box::new((
                to_encoder_fn_var,
                Loc::at_zero(to_encoder_var),
                to_encoder_clos_var,
                encoder_var,
                Variable::PURE,
            ));

            // toEncoder rcd.a
            let to_encoder_call = Call(
                to_encoder_fn,
                vec![(field_var, Loc::at_zero(field_access))],
                CalledVia::Space,
            );

            // value: toEncoder rcd.a
            let value_field = Field {
                var: encoder_var,
                region: Region::zero(),
                loc_expr: Box::new(Loc::at_zero(to_encoder_call)),
            };

            // { key: "a", value: toEncoder rcd.a }
            let mut kv = SendMap::default();
            kv.insert("key".into(), key_field);
            kv.insert("value".into(), value_field);

            let this_record_fields = RecordFields::insert_into_subs(
                env.subs,
                (once(("key".into(), RecordField::Required(Variable::STR))))
                    .chain(once(("value".into(), RecordField::Required(encoder_var)))),
            );
            let this_record_var = synth_var(
                env.subs,
                Content::Structure(FlatType::Record(this_record_fields, Variable::EMPTY_RECORD)),
            );
            // NOTE: must be done to unify the lambda sets under `encoder_var`
            env.unify(this_record_var, whole_rcd_var);

            Loc::at_zero(Record {
                record_var: whole_rcd_var,
                fields: kv,
            })
        })
        .collect::<Vec<_>>();

    // typeof [ { key: .., value: .. }, { key: .., value: .. } ]
    let fields_rcd_var_slice = env.subs.insert_into_vars(once(whole_rcd_var));
    let fields_list_var = synth_var(
        env.subs,
        Content::Structure(FlatType::Apply(Symbol::LIST_LIST, fields_rcd_var_slice)),
    );

    // [ { key: .., value: ..}, .. ]
    let fields_list = List {
        elem_var: whole_rcd_var,
        loc_elems: fields_list,
    };

    // build `Encode.record [ { key: .., value: ..}, .. ]` type
    // List { key : Str, value : Encoder fmt } -[uls]-> Encoder fmt where fmt implements EncoderFormatting
    let encode_record_fn_var = env.import_builtin_symbol_var(Symbol::ENCODE_RECORD);

    // fields_list_var -[clos]-> t1
    let fields_list_var_slice = env.subs.insert_into_vars(once(fields_list_var));
    let encode_record_clos_var = env.subs.fresh_unnamed_flex_var(); // clos
    let encoder_var = env.subs.fresh_unnamed_flex_var(); // t1
    let this_encode_record_fn_var = synth_var(
        env.subs,
        Content::Structure(FlatType::Func(
            fields_list_var_slice,
            encode_record_clos_var,
            encoder_var,
            Variable::PURE,
        )),
    );

    //   List { key : Str, value : Encoder fmt } -[uls]->  Encoder fmt where fmt implements EncoderFormatting
    // ~ fields_list_var                         -[clos]-> t1
    env.unify(encode_record_fn_var, this_encode_record_fn_var);

    // Encode.record : fields_list_var -[clos]-> Encoder fmt where fmt implements EncoderFormatting
    let encode_record_var = AbilityMember(Symbol::ENCODE_RECORD, None, encode_record_fn_var);
    let encode_record_fn = Box::new((
        encode_record_fn_var,
        Loc::at_zero(encode_record_var),
        encode_record_clos_var,
        encoder_var,
        Variable::PURE,
    ));

    // Encode.record [ { key: .., value: .. }, .. ]
    let encode_record_call = Call(
        encode_record_fn,
        vec![(fields_list_var, Loc::at_zero(fields_list))],
        CalledVia::Space,
    );

    // Encode.custom \bytes, fmt -> Encode.appendWith bytes (Encode.record ..) fmt
    let (body, this_encoder_var) =
        wrap_in_encode_custom(env, encode_record_call, encoder_var, rcd_sym, record_var);

    // Create fn_var for ambient capture; we fix it up below.
    let fn_var = synth_var(env.subs, Content::Error);

    // -[fn_name]->
    let fn_name_labels = UnionLambdas::insert_into_subs(env.subs, once((fn_name, vec![])));
    let fn_clos_var = synth_var(
        env.subs,
        Content::LambdaSet(LambdaSet {
            solved: fn_name_labels,
            recursion_var: OptVariable::NONE,
            unspecialized: SubsSlice::default(),
            ambient_function: fn_var,
        }),
    );
    // typeof rcd -[fn_name]-> (typeof Encode.record [ .. ] = Encoder fmt)
    let record_var_slice = env.subs.insert_into_vars(once(record_var));
    env.subs.set_content(
        fn_var,
        Content::Structure(FlatType::Func(
            record_var_slice,
            fn_clos_var,
            this_encoder_var,
            Variable::PURE,
        )),
    );

    // \rcd -[fn_name]-> Encode.record [ { key: .., value: .. }, .. ]
    let clos = Closure(ClosureData {
        function_type: fn_var,
        closure_type: fn_clos_var,
        return_type: this_encoder_var,
        fx_type: Variable::PURE,
        early_returns: vec![],
        name: fn_name,
        captured_symbols: vec![],
        recursive: Recursive::NotRecursive,
        arguments: vec![(
            record_var,
            AnnotatedMark::known_exhaustive(),
            Loc::at_zero(Pattern::Identifier(rcd_sym)),
        )],
        loc_body: Box::new(Loc::at_zero(body)),
    });

    (clos, fn_var)
}

fn to_encoder_tuple(
    env: &mut Env<'_>,
    tuple_var: Variable,
    elems: TupleElems,
    fn_name: Symbol,
) -> (Expr, Variable) {
    // Suppose tup = (t1, t2). Build
    //
    // \tup -> Encode.tuple [
    //      Encode.toEncoder tup.0,
    //      Encode.toEncoder tup.1,
    //   ]

    let tup_sym = env.new_symbol("tup");
    let whole_encoder_in_list_var = env.subs.fresh_unnamed_flex_var(); // type of the encoder in the list

    use Expr::*;

    let elem_encoders_list = elems
        .iter_all()
        .map(|(elem_index, elem_var_index)| {
            let index = env.subs[elem_index];
            let elem_var = env.subs[elem_var_index];
            let elem_var_slice = VariableSubsSlice::new(elem_var_index.index() as u32, 1);

            // tup.0
            let tuple_access = TupleAccess {
                tuple_var,
                ext_var: env.subs.fresh_unnamed_flex_var(),
                elem_var,
                loc_expr: Box::new(Loc::at_zero(Var(
                    tup_sym,
                    env.subs.fresh_unnamed_flex_var(),
                ))),
                index,
            };

            // build `toEncoder tup.0` type
            // val -[uls]-> Encoder fmt where fmt implements EncoderFormatting
            let to_encoder_fn_var = env.import_builtin_symbol_var(Symbol::ENCODE_TO_ENCODER);

            // (typeof tup.0) -[clos]-> t1
            let to_encoder_clos_var = env.subs.fresh_unnamed_flex_var(); // clos
            let encoder_var = env.subs.fresh_unnamed_flex_var(); // t1
            let this_to_encoder_fn_var = synth_var(
                env.subs,
                Content::Structure(FlatType::Func(
                    elem_var_slice,
                    to_encoder_clos_var,
                    encoder_var,
                    Variable::PURE,
                )),
            );

            //   val            -[uls]->  Encoder fmt where fmt implements EncoderFormatting
            // ~ (typeof tup.0) -[clos]-> t1
            env.unify(to_encoder_fn_var, this_to_encoder_fn_var);

            // toEncoder : (typeof tup.0) -[clos]-> Encoder fmt where fmt implements EncoderFormatting
            let to_encoder_var = AbilityMember(Symbol::ENCODE_TO_ENCODER, None, to_encoder_fn_var);
            let to_encoder_fn = Box::new((
                to_encoder_fn_var,
                Loc::at_zero(to_encoder_var),
                to_encoder_clos_var,
                encoder_var,
                Variable::PURE,
            ));

            // toEncoder tup.0
            let to_encoder_call = Call(
                to_encoder_fn,
                vec![(elem_var, Loc::at_zero(tuple_access))],
                CalledVia::Space,
            );

            // NOTE: must be done to unify the lambda sets under `encoder_var`
            env.unify(encoder_var, whole_encoder_in_list_var);

            Loc::at_zero(to_encoder_call)
        })
        .collect::<Vec<_>>();

    // typeof [ toEncoder tup.0, toEncoder tup.1 ]
    let whole_encoder_in_list_var_slice =
        env.subs.insert_into_vars(once(whole_encoder_in_list_var));
    let elem_encoders_list_var = synth_var(
        env.subs,
        Content::Structure(FlatType::Apply(
            Symbol::LIST_LIST,
            whole_encoder_in_list_var_slice,
        )),
    );

    // [ toEncoder tup.0, toEncoder tup.1 ]
    let elem_encoders_list = List {
        elem_var: whole_encoder_in_list_var,
        loc_elems: elem_encoders_list,
    };

    // build `Encode.tuple [ toEncoder tup.0, toEncoder tup.1 ]` type
    // List (Encoder fmt) -[uls]-> Encoder fmt where fmt implements EncoderFormatting
    let encode_tuple_fn_var = env.import_builtin_symbol_var(Symbol::ENCODE_TUPLE);

    // elem_encoders_list_var -[clos]-> t1
    let elem_encoders_list_var_slice = env.subs.insert_into_vars(once(elem_encoders_list_var));
    let encode_tuple_clos_var = env.subs.fresh_unnamed_flex_var(); // clos
    let encoder_var = env.subs.fresh_unnamed_flex_var(); // t1
    let this_encode_tuple_fn_var = synth_var(
        env.subs,
        Content::Structure(FlatType::Func(
            elem_encoders_list_var_slice,
            encode_tuple_clos_var,
            encoder_var,
            Variable::PURE,
        )),
    );

    //   List (Encoder fmt)     -[uls]->  Encoder fmt where fmt implements EncoderFormatting
    // ~ elem_encoders_list_var -[clos]-> t1
    env.unify(encode_tuple_fn_var, this_encode_tuple_fn_var);

    // Encode.tuple : elem_encoders_list_var -[clos]-> Encoder fmt where fmt implements EncoderFormatting
    let encode_tuple_var = AbilityMember(Symbol::ENCODE_TUPLE, None, encode_tuple_fn_var);
    let encode_tuple_fn = Box::new((
        encode_tuple_fn_var,
        Loc::at_zero(encode_tuple_var),
        encode_tuple_clos_var,
        encoder_var,
        Variable::PURE,
    ));

    // Encode.tuple [ { key: .., value: .. }, .. ]
    let encode_tuple_call = Call(
        encode_tuple_fn,
        vec![(elem_encoders_list_var, Loc::at_zero(elem_encoders_list))],
        CalledVia::Space,
    );

    // Encode.custom \bytes, fmt -> Encode.appendWith bytes (Encode.tuple_var ..) fmt
    let (body, this_encoder_var) =
        wrap_in_encode_custom(env, encode_tuple_call, encoder_var, tup_sym, tuple_var);

    // Create fn_var for ambient capture; we fix it up below.
    let fn_var = synth_var(env.subs, Content::Error);

    // -[fn_name]->
    let fn_name_labels = UnionLambdas::insert_into_subs(env.subs, once((fn_name, vec![])));
    let fn_clos_var = synth_var(
        env.subs,
        Content::LambdaSet(LambdaSet {
            solved: fn_name_labels,
            recursion_var: OptVariable::NONE,
            unspecialized: SubsSlice::default(),
            ambient_function: fn_var,
        }),
    );
    // typeof tup -[fn_name]-> (typeof Encode.tuple [ .. ] = Encoder fmt)
    let tuple_var_slice = env.subs.insert_into_vars(once(tuple_var));
    env.subs.set_content(
        fn_var,
        Content::Structure(FlatType::Func(
            tuple_var_slice,
            fn_clos_var,
            this_encoder_var,
            Variable::PURE,
        )),
    );

    // \tup -[fn_name]-> Encode.tuple [ { key: .., value: .. }, .. ]
    let clos = Closure(ClosureData {
        function_type: fn_var,
        closure_type: fn_clos_var,
        return_type: this_encoder_var,
        fx_type: Variable::PURE,
        early_returns: vec![],
        name: fn_name,
        captured_symbols: vec![],
        recursive: Recursive::NotRecursive,
        arguments: vec![(
            tuple_var,
            AnnotatedMark::known_exhaustive(),
            Loc::at_zero(Pattern::Identifier(tup_sym)),
        )],
        loc_body: Box::new(Loc::at_zero(body)),
    });

    (clos, fn_var)
}

fn to_encoder_tag_union(
    env: &mut Env<'_>,
    tag_union_var: Variable,
    tags: UnionTags,
    fn_name: Symbol,
) -> (Expr, Variable) {
    // Suppose tag = [ A t1 t2, B t3 ]. Build
    //
    // \tag -> when tag is
    //     A v1 v2 -> Encode.tag "A" [ Encode.toEncoder v1, Encode.toEncoder v2 ]
    //     B v3 -> Encode.tag "B" [ Encode.toEncoder v3 ]

    let tag_sym = env.new_symbol("tag");
    let whole_tag_encoders_var = env.subs.fresh_unnamed_flex_var(); // type of the Encode.tag ... calls in the branch bodies

    use Expr::*;

    let branches = tags
        .iter_all()
        .map(|(tag_name_index, tag_vars_slice_index)| {
            // A
            let tag_name = &env.subs[tag_name_index].clone();
            let vars_slice = env.subs[tag_vars_slice_index];
            // t1 t2
            let payload_vars = env.subs.get_subs_slice(vars_slice).to_vec();
            // v1 v2
            let payload_syms: Vec<_> = std::iter::repeat_with(|| env.unique_symbol())
                .take(payload_vars.len())
                .collect();

            // `A v1 v2` pattern
            let pattern = Pattern::AppliedTag {
                whole_var: tag_union_var,
                tag_name: tag_name.clone(),
                ext_var: Variable::EMPTY_TAG_UNION,
                // (t1, v1) (t2, v2)
                arguments: (payload_vars.iter())
                    .zip(payload_syms.iter())
                    .map(|(var, sym)| (*var, Loc::at_zero(Pattern::Identifier(*sym))))
                    .collect(),
            };
            let branch_pattern = WhenBranchPattern {
                pattern: Loc::at_zero(pattern),
                degenerate: false,
            };

            // whole type of the elements in [ Encode.toEncoder v1, Encode.toEncoder v2 ]
            let whole_payload_encoders_var = env.subs.fresh_unnamed_flex_var();
            // [ Encode.toEncoder v1, Encode.toEncoder v2 ]
            let payload_to_encoders = (payload_syms.iter())
                .zip(payload_vars.iter())
                .map(|(&sym, &sym_var)| {
                    // build `toEncoder v1` type
                    // expected: val -[uls]-> Encoder fmt where fmt implements EncoderFormatting
                    let to_encoder_fn_var =
                        env.import_builtin_symbol_var(Symbol::ENCODE_TO_ENCODER);

                    // wanted: t1 -[clos]-> t'
                    let var_slice_of_sym_var = env.subs.insert_into_vars([sym_var]); // [ t1 ]
                    let to_encoder_clos_var = env.subs.fresh_unnamed_flex_var(); // clos
                    let encoder_var = env.subs.fresh_unnamed_flex_var(); // t'
                    let this_to_encoder_fn_var = synth_var(
                        env.subs,
                        Content::Structure(FlatType::Func(
                            var_slice_of_sym_var,
                            to_encoder_clos_var,
                            encoder_var,
                            Variable::PURE,
                        )),
                    );

                    //   val -[uls]->  Encoder fmt where fmt implements EncoderFormatting
                    // ~ t1  -[clos]-> t'
                    env.unify(to_encoder_fn_var, this_to_encoder_fn_var);

                    // toEncoder : t1 -[clos]-> Encoder fmt where fmt implements EncoderFormatting
                    let to_encoder_var =
                        AbilityMember(Symbol::ENCODE_TO_ENCODER, None, this_to_encoder_fn_var);
                    let to_encoder_fn = Box::new((
                        this_to_encoder_fn_var,
                        Loc::at_zero(to_encoder_var),
                        to_encoder_clos_var,
                        encoder_var,
                        Variable::PURE,
                    ));

                    // toEncoder rcd.a
                    let to_encoder_call = Call(
                        to_encoder_fn,
                        vec![(sym_var, Loc::at_zero(Var(sym, sym_var)))],
                        CalledVia::Space,
                    );

                    // NOTE: must be done to unify the lambda sets under `encoder_var`
                    env.unify(encoder_var, whole_payload_encoders_var);

                    Loc::at_zero(to_encoder_call)
                })
                .collect();

            // typeof [ Encode.toEncoder v1, Encode.toEncoder v2 ]
            let whole_encoders_var_slice = env.subs.insert_into_vars([whole_payload_encoders_var]);
            let payload_encoders_list_var = synth_var(
                env.subs,
                Content::Structure(FlatType::Apply(Symbol::LIST_LIST, whole_encoders_var_slice)),
            );

            // [ Encode.toEncoder v1, Encode.toEncoder v2 ]
            let payload_encoders_list = List {
                elem_var: whole_payload_encoders_var,
                loc_elems: payload_to_encoders,
            };

            // build `Encode.tag "A" [ ... ]` type
            // expected: Str, List (Encoder fmt) -[uls]-> Encoder fmt where fmt implements EncoderFormatting
            let encode_tag_fn_var = env.import_builtin_symbol_var(Symbol::ENCODE_TAG);

            // wanted: Str, List whole_encoders_var -[clos]-> t'
            let this_encode_tag_args_var_slice = env
                .subs
                .insert_into_vars([Variable::STR, payload_encoders_list_var]);
            let this_encode_tag_clos_var = env.subs.fresh_unnamed_flex_var(); // -[clos]->
            let this_encoder_var = env.subs.fresh_unnamed_flex_var(); // t'
            let this_encode_tag_fn_var = synth_var(
                env.subs,
                Content::Structure(FlatType::Func(
                    this_encode_tag_args_var_slice,
                    this_encode_tag_clos_var,
                    this_encoder_var,
                    Variable::PURE,
                )),
            );

            //   Str, List (Encoder fmt)      -[uls]->  Encoder fmt where fmt implements EncoderFormatting
            // ~ Str, List whole_encoders_var -[clos]-> t'
            env.unify(encode_tag_fn_var, this_encode_tag_fn_var);

            // Encode.tag : Str, List whole_encoders_var -[clos]-> Encoder fmt where fmt implements EncoderFormatting
            let encode_tag_var = AbilityMember(Symbol::ENCODE_TAG, None, this_encode_tag_fn_var);
            let encode_tag_fn = Box::new((
                this_encode_tag_fn_var,
                Loc::at_zero(encode_tag_var),
                this_encode_tag_clos_var,
                this_encoder_var,
                Variable::PURE,
            ));

            // Encode.tag "A" [ Encode.toEncoder v1, Encode.toEncoder v2 ]
            let encode_tag_call = Call(
                encode_tag_fn,
                vec![
                    // (Str, "A")
                    (Variable::STR, Loc::at_zero(Str(tag_name.0.as_str().into()))),
                    // (List (Encoder fmt), [ Encode.toEncoder v1, Encode.toEncoder v2 ])
                    (
                        payload_encoders_list_var,
                        Loc::at_zero(payload_encoders_list),
                    ),
                ],
                CalledVia::Space,
            );

            // NOTE: must be done to unify the lambda sets under `encoder_var`
            // Encode.tag "A" [ Encode.toEncoder v1, Encode.toEncoder v2 ] ~ whole_encoders
            env.unify(this_encoder_var, whole_tag_encoders_var);

            WhenBranch {
                patterns: vec![branch_pattern],
                value: Loc::at_zero(encode_tag_call),
                guard: None,
                redundant: RedundantMark::known_non_redundant(),
            }
        })
        .collect::<Vec<_>>();

    // when tag is
    //     A v1 v2 -> Encode.tag "A" [ Encode.toEncoder v1, Encode.toEncoder v2 ]
    //     B v3 -> Encode.tag "B" [ Encode.toEncoder v3 ]
    let when_branches = When {
        loc_cond: Box::new(Loc::at_zero(Var(tag_sym, tag_union_var))),
        cond_var: tag_union_var,
        expr_var: whole_tag_encoders_var,
        region: Region::zero(),
        branches,
        branches_cond_var: tag_union_var,
        exhaustive: ExhaustiveMark::known_exhaustive(),
    };

    // Encode.custom \bytes, fmt -> Encode.appendWith bytes (when ..) fmt
    let (body, this_encoder_var) = wrap_in_encode_custom(
        env,
        when_branches,
        whole_tag_encoders_var,
        tag_sym,
        tag_union_var,
    );

    // Create fn_var for ambient capture; we fix it up below.
    let fn_var = synth_var(env.subs, Content::Error);

    // -[fn_name]->
    let fn_name_labels = UnionLambdas::insert_into_subs(env.subs, once((fn_name, vec![])));
    let fn_clos_var = synth_var(
        env.subs,
        Content::LambdaSet(LambdaSet {
            solved: fn_name_labels,
            recursion_var: OptVariable::NONE,
            unspecialized: SubsSlice::default(),
            ambient_function: fn_var,
        }),
    );
    // tag_union_var -[fn_name]-> whole_tag_encoders_var
    let tag_union_var_slice = env.subs.insert_into_vars(once(tag_union_var));
    env.subs.set_content(
        fn_var,
        Content::Structure(FlatType::Func(
            tag_union_var_slice,
            fn_clos_var,
            this_encoder_var,
            Variable::PURE,
        )),
    );

    // \tag ->
    //   Encode.custom \bytes, fmt -> Encode.appendWith bytes (
    //     when tag is
    //        A v1 v2 -> Encode.tag "A" [ Encode.toEncoder v1, Encode.toEncoder v2 ]
    //        B v3 -> Encode.tag "B" [ Encode.toEncoder v3 ])
    //     fmt
    let clos = Closure(ClosureData {
        function_type: fn_var,
        closure_type: fn_clos_var,
        return_type: this_encoder_var,
        fx_type: Variable::PURE,
        early_returns: vec![],
        name: fn_name,
        captured_symbols: vec![],
        recursive: Recursive::NotRecursive,
        arguments: vec![(
            tag_union_var,
            AnnotatedMark::known_exhaustive(),
            Loc::at_zero(Pattern::Identifier(tag_sym)),
        )],
        loc_body: Box::new(Loc::at_zero(body)),
    });

    (clos, fn_var)
}

/// Lift `encoder` to `Encode.custom \bytes, fmt -> Encode.appendWith bytes encoder fmt`
///
/// TODO: currently it appears that just `encoder` is not isomorphic to the lift, on the
/// monomorphization level, even though we would think it is. In particular, unspecialized lambda
/// sets fail to resolve when we use the non-lifted version.
/// More investigation is needed to figure out why.
fn wrap_in_encode_custom(
    env: &mut Env,
    encoder: Expr,
    encoder_var: Variable,
    captured_symbol: Symbol,
    captured_var: Variable,
) -> (Expr, Variable) {
    use Expr::*;

    let fn_name = env.new_symbol("custom");

    // bytes: List U8
    let bytes_sym = env.new_symbol("bytes");
    let bytes_var = Variable::LIST_U8;

    // fmt: fmt where fmt implements EncoderFormatting
    let fmt_sym = env.new_symbol("fmt");
    let fmt_var = env.subs.fresh_unnamed_flex_var();

    // build `Encode.appendWith bytes encoder fmt` type
    // expected: Encode.appendWith : List U8, Encoder fmt, fmt -[appendWith]-> List U8 where fmt implements EncoderFormatting
    let append_with_fn_var = env.import_builtin_symbol_var(Symbol::ENCODE_APPEND_WITH);

    // wanted: Encode.appendWith : List U8, encoder_var, fmt -[clos]-> List U8 where fmt implements EncoderFormatting
    let this_append_with_args_var_slice =
        env.subs
            .insert_into_vars([Variable::LIST_U8, encoder_var, fmt_var]);
    let this_append_with_clos_var = env.subs.fresh_unnamed_flex_var(); // -[clos]->
    let this_append_with_fn_var = synth_var(
        env.subs,
        Content::Structure(FlatType::Func(
            this_append_with_args_var_slice,
            this_append_with_clos_var,
            Variable::LIST_U8,
            Variable::PURE,
        )),
    );

    //   List U8, Encoder fmt, fmt -[appendWith]-> List U8 where fmt implements EncoderFormatting
    // ~ List U8, encoder_var, fmt -[clos]->       List U8 where fmt implements EncoderFormatting
    env.unify(append_with_fn_var, this_append_with_fn_var);

    // Encode.appendWith : List U8, encoder_var, fmt -[appendWith]-> List U8 where fmt implements EncoderFormatting
    let append_with_fn = Box::new((
        this_append_with_fn_var,
        Loc::at_zero(Var(Symbol::ENCODE_APPEND_WITH, this_append_with_fn_var)),
        this_append_with_clos_var,
        Variable::LIST_U8,
        Variable::PURE,
    ));

    // Encode.appendWith bytes encoder fmt
    let append_with_call = Call(
        append_with_fn,
        vec![
            // (bytes_var, bytes)
            (bytes_var, Loc::at_zero(Var(bytes_sym, bytes_var))),
            // (encoder_var, encoder)
            (encoder_var, Loc::at_zero(encoder)),
            // (fmt, fmt_var)
            (fmt_var, Loc::at_zero(Var(fmt_sym, fmt_var))),
        ],
        CalledVia::Space,
    );

    // Create fn_var for ambient capture; we fix it up below.
    let fn_var = synth_var(env.subs, Content::Error);

    // -[[FN_name captured_var]]->
    let fn_name_labels =
        UnionLambdas::insert_into_subs(env.subs, once((fn_name, vec![captured_var])));
    let fn_clos_var = synth_var(
        env.subs,
        Content::LambdaSet(LambdaSet {
            solved: fn_name_labels,
            recursion_var: OptVariable::NONE,
            unspecialized: SubsSlice::default(),
            ambient_function: fn_var,
        }),
    );

    // bytes, fmt -[[FN_name captured_var]]-> Encode.appendWith bytes encoder fmt
    let args_slice = env.subs.insert_into_vars(vec![bytes_var, fmt_var]);
    env.subs.set_content(
        fn_var,
        Content::Structure(FlatType::Func(
            args_slice,
            fn_clos_var,
            Variable::LIST_U8,
            Variable::PURE,
        )),
    );

    // \bytes, fmt -[[fn_name captured_var]]-> Encode.appendWith bytes encoder fmt
    let clos = Closure(ClosureData {
        function_type: fn_var,
        closure_type: fn_clos_var,
        return_type: Variable::LIST_U8,
        fx_type: Variable::PURE,
        early_returns: vec![],
        name: fn_name,
        captured_symbols: vec![(captured_symbol, captured_var)],
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
        loc_body: Box::new(Loc::at_zero(append_with_call)),
    });

    // Build
    // Encode.custom \bytes, fmt -> Encode.appendWith bytes encoder fmt
    //
    // expected: Encode.custom : (List U8, fmt -> List U8) -> Encoder fmt where fmt implements EncoderFormatting
    let custom_fn_var = env.import_builtin_symbol_var(Symbol::ENCODE_CUSTOM);

    // wanted: Encode.custom : fn_var -[clos]-> t'
    let this_custom_args_var_slice = env.subs.insert_into_vars([fn_var]);
    let this_custom_clos_var = env.subs.fresh_unnamed_flex_var(); // -[clos]->
    let this_custom_encoder_var = env.subs.fresh_unnamed_flex_var(); // t'
    let this_custom_fn_var = synth_var(
        env.subs,
        Content::Structure(FlatType::Func(
            this_custom_args_var_slice,
            this_custom_clos_var,
            this_custom_encoder_var,
            Variable::PURE,
        )),
    );

    //   (List U8, fmt -> List U8) -[..]->   Encoder fmt where fmt implements EncoderFormatting
    // ~ fn_var                    -[clos]-> t'
    env.unify(custom_fn_var, this_custom_fn_var);

    // Encode.custom : (List U8, fmt -> List U8) -> Encoder fmt where fmt implements EncoderFormatting
    let custom_fn = Box::new((
        this_custom_fn_var,
        Loc::at_zero(Var(Symbol::ENCODE_CUSTOM, this_custom_fn_var)),
        this_custom_clos_var,    // -[clos]->
        this_custom_encoder_var, // t' ~ Encoder fmt
        Variable::PURE,
    ));

    // Encode.custom \bytes, fmt -> Encode.appendWith bytes encoder fmt
    let custom_call = Call(
        custom_fn,
        vec![(fn_var, Loc::at_zero(clos))],
        CalledVia::Space,
    );

    (custom_call, this_custom_encoder_var)
}
