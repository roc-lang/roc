//! Derivers for the `Encoding` ability.

use std::iter::once;

use bumpalo::Bump;

use roc_can::abilities::AbilitiesStore;
use roc_can::expr::{AnnotatedMark, ClosureData, Expr, Field, Recursive, WhenBranch};
use roc_can::pattern::Pattern;
use roc_collections::SendMap;
use roc_derive_key::encoding::FlatEncodable;
use roc_derive_key::DeriveKey;
use roc_error_macros::internal_error;
use roc_late_solve::{instantiate_rigids, AbilitiesView};
use roc_module::called_via::CalledVia;
use roc_module::ident::Lowercase;
use roc_module::symbol::{IdentIds, ModuleId, Symbol};
use roc_region::all::{Loc, Region};
use roc_types::subs::{
    Content, ExhaustiveMark, ExposedTypesStorageSubs, FlatType, GetSubsSlice, LambdaSet,
    OptVariable, RecordFields, RedundantMark, Subs, SubsFmtContent, SubsSlice, UnionLambdas,
    UnionTags, Variable, VariableSubsSlice,
};
use roc_types::types::{AliasKind, RecordField};

use crate::derive::synth_var;

macro_rules! bad_input {
    ($subs:expr, $var:expr) => {
        bad_input!($subs, $var, "Invalid content")
    };
    ($subs:expr, $var:expr, $msg:expr) => {
        internal_error!(
            "{:?} for toEncoder deriver: {:?}",
            $msg,
            SubsFmtContent($subs.get_content_without_compacting($var), $subs)
        )
    };
}

pub struct Env<'a> {
    pub home: ModuleId,
    pub arena: &'a Bump,
    pub subs: &'a mut Subs,
    pub ident_ids: &'a mut IdentIds,
    pub exposed_encode_types: &'a mut ExposedTypesStorageSubs,
}

impl Env<'_> {
    fn unique_symbol(&mut self) -> Symbol {
        let ident_id = self.ident_ids.gen_unique();

        Symbol::new(self.home, ident_id)
    }

    fn import_encode_symbol(&mut self, symbol: Symbol) -> Variable {
        debug_assert_eq!(symbol.module_id(), ModuleId::ENCODE);

        let storage_var = self
            .exposed_encode_types
            .stored_vars_by_symbol
            .get(&symbol)
            .unwrap();

        let imported = self
            .exposed_encode_types
            .storage_subs
            .export_variable_to(self.subs, *storage_var);

        instantiate_rigids(self.subs, imported.variable);

        imported.variable
    }

    fn unify(&mut self, left: Variable, right: Variable) {
        // NOTE: I don't believe the abilities store is necessary for unification at this point!
        roc_late_solve::unify(
            self.home,
            self.arena,
            self.subs,
            &AbilitiesView::Module(&AbilitiesStore::default()),
            left,
            right,
        )
        .expect("unification failed!")
    }
}

// TODO: decide whether it will be better to pass the whole signature, or just the argument type.
// For now we are only using the argument type for convinience of testing.
#[allow(dead_code)]
fn verify_signature(env: &mut Env<'_>, signature: Variable) {
    // Verify the signature is what we expect: input -> Encoder fmt | fmt has EncoderFormatting
    // and get the input type
    match env.subs.get_content_without_compacting(signature) {
        Content::Structure(FlatType::Func(input, _, output)) => {
            // Check the output is Encoder fmt | fmt has EncoderFormatting
            match env.subs.get_content_without_compacting(*output) {
                Content::Alias(Symbol::ENCODE_ENCODER, args, _, AliasKind::Opaque) => {
                    match env.subs.get_subs_slice(args.all_variables()) {
                        [one] => match env.subs.get_content_without_compacting(*one) {
                            Content::FlexAbleVar(_, Symbol::ENCODE_ENCODERFORMATTING) => {}
                            _ => bad_input!(env.subs, signature),
                        },
                        _ => bad_input!(env.subs, signature),
                    }
                }
                _ => bad_input!(env.subs, signature),
            }

            // Get the only parameter into toEncoder
            match env.subs.get_subs_slice(*input) {
                [one] => *one,
                _ => bad_input!(env.subs, signature),
            }
        }
        _ => bad_input!(env.subs, signature),
    };
}

pub fn derive_to_encoder(env: &mut Env<'_>, for_var: Variable) -> Expr {
    match DeriveKey::encoding(env.subs, for_var).repr {
        FlatEncodable::U8 => todo!(),
        FlatEncodable::U16 => todo!(),
        FlatEncodable::U32 => todo!(),
        FlatEncodable::U64 => todo!(),
        FlatEncodable::U128 => todo!(),
        FlatEncodable::I8 => todo!(),
        FlatEncodable::I16 => todo!(),
        FlatEncodable::I32 => todo!(),
        FlatEncodable::I64 => todo!(),
        FlatEncodable::I128 => todo!(),
        FlatEncodable::Dec => todo!(),
        FlatEncodable::F32 => todo!(),
        FlatEncodable::F64 => todo!(),
        FlatEncodable::List() => todo!(),
        FlatEncodable::Set() => todo!(),
        FlatEncodable::Dict() => todo!(),
        FlatEncodable::Str => todo!(),
        FlatEncodable::Record(fields) => {
            // Generalized record var so we can reuse this impl between many records:
            // if fields = { a, b }, this is { a: t1, b: t2 } for fresh t1, t2.
            let flex_fields = fields
                .iter()
                .copied()
                .cloned()
                .collect::<Vec<_>>()
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

            to_encoder_record(env, record_var, fields)
        }
        FlatEncodable::TagUnion(tags) => {
            // Generalized tag union var so we can reuse this impl between many unions:
            // if tags = [ A arity=2, B arity=1 ], this is [ A t1 t2, B t3 ] for fresh t1, t2, t3
            let flex_tag_labels = tags
                .iter()
                .copied()
                .map(|(label, arity)| (label.clone(), arity))
                .collect::<Vec<_>>()
                .into_iter()
                .map(|(label, arity)| {
                    let variables_slice =
                        VariableSubsSlice::reserve_into_subs(env.subs, arity.into());
                    for var_index in variables_slice {
                        env.subs[var_index] = env.subs.fresh_unnamed_flex_var();
                    }
                    (label, variables_slice)
                })
                .collect::<Vec<_>>();
            let union_tags = UnionTags::insert_slices_into_subs(env.subs, flex_tag_labels);
            let tag_union_var = synth_var(
                env.subs,
                Content::Structure(FlatType::TagUnion(union_tags, Variable::EMPTY_TAG_UNION)),
            );

            to_encoder_tag_union(env, tag_union_var, union_tags)
        }
    }
}

fn to_encoder_record(env: &mut Env<'_>, record_var: Variable, fields: RecordFields) -> Expr {
    // Suppose rcd = { a: t1, b: t2 }. Build
    //
    // \rcd -> Encode.record [
    //      { key: "a", value: Encode.toEncoder rcd.a },
    //      { key: "b", value: Encode.toEncoder rcd.b },
    //   ]

    let rcd_sym = env.unique_symbol();
    let whole_rcd_var = env.subs.fresh_unnamed_flex_var(); // type of the { key, value } records in the list

    use Expr::*;

    let fields_list = fields
        .iter_all()
        .map(|(field_name_index, field_var_index, _)| {
            let field_name = env.subs[field_name_index].clone();
            let field_var = env.subs[field_var_index];
            let field_var_slice = VariableSubsSlice::new(field_var_index.index, 1);

            // key: "a"
            let key_field = Field {
                var: Variable::STR,
                region: Region::zero(),
                loc_expr: Box::new(Loc::at_zero(Str(field_name.as_str().into()))),
            };

            // rcd.a
            let field_access = Access {
                record_var,
                ext_var: env.subs.fresh_unnamed_flex_var(),
                field_var,
                loc_expr: Box::new(Loc::at_zero(Var(rcd_sym))),
                field: field_name,
            };

            // build `toEncoder rcd.a` type
            // val -[uls]-> Encoder fmt | fmt has EncoderFormatting
            let to_encoder_fn_var = env.import_encode_symbol(Symbol::ENCODE_TO_ENCODER);

            // (typeof rcd.a) -[clos]-> t1
            let to_encoder_clos_var = env.subs.fresh_unnamed_flex_var(); // clos
            let encoder_var = env.subs.fresh_unnamed_flex_var(); // t1
            let this_to_encoder_fn_var = synth_var(
                env.subs,
                Content::Structure(FlatType::Func(
                    field_var_slice,
                    to_encoder_clos_var,
                    encoder_var,
                )),
            );

            //   val            -[uls]->  Encoder fmt | fmt has EncoderFormatting
            // ~ (typeof rcd.a) -[clos]-> t1
            env.unify(to_encoder_fn_var, this_to_encoder_fn_var);

            // toEncoder : (typeof rcd.a) -[clos]-> Encoder fmt | fmt has EncoderFormatting
            let to_encoder_fn = Box::new((
                to_encoder_fn_var,
                Loc::at_zero(Var(Symbol::ENCODE_TO_ENCODER)),
                to_encoder_clos_var,
                encoder_var,
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
    let fields_rcd_var_slice = VariableSubsSlice::insert_into_subs(env.subs, once(whole_rcd_var));
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
    // List { key : Str, value : Encoder fmt } -[uls]-> Encoder fmt | fmt has EncoderFormatting
    let encode_record_fn_var = env.import_encode_symbol(Symbol::ENCODE_RECORD);

    // fields_list_var -[clos]-> t1
    let fields_list_var_slice =
        VariableSubsSlice::insert_into_subs(env.subs, once(fields_list_var));
    let encode_record_clos_var = env.subs.fresh_unnamed_flex_var(); // clos
    let encoder_var = env.subs.fresh_unnamed_flex_var(); // t1
    let this_encode_record_fn_var = synth_var(
        env.subs,
        Content::Structure(FlatType::Func(
            fields_list_var_slice,
            encode_record_clos_var,
            encoder_var,
        )),
    );

    //   List { key : Str, value : Encoder fmt } -[uls]->  Encoder fmt | fmt has EncoderFormatting
    // ~ fields_list_var                         -[clos]-> t1
    env.unify(encode_record_fn_var, this_encode_record_fn_var);

    // Encode.record : fields_list_var -[clos]-> Encoder fmt | fmt has EncoderFormatting
    let encode_record_fn = Box::new((
        encode_record_fn_var,
        Loc::at_zero(Var(Symbol::ENCODE_RECORD)),
        encode_record_clos_var,
        encoder_var,
    ));

    // Encode.record [ { key: .., value: .. }, .. ]
    let encode_record_call = Call(
        encode_record_fn,
        vec![(fields_list_var, Loc::at_zero(fields_list))],
        CalledVia::Space,
    );

    let fn_name = env.unique_symbol();
    let fn_name_labels = UnionLambdas::insert_into_subs(env.subs, once((fn_name, vec![])));
    // -[fn_name]->
    let fn_clos_var = synth_var(
        env.subs,
        Content::LambdaSet(LambdaSet {
            solved: fn_name_labels,
            recursion_var: OptVariable::NONE,
            unspecialized: SubsSlice::default(),
        }),
    );
    // typeof rcd -[fn_name]-> (typeof Encode.record [ .. ] = Encoder fmt)
    let record_var_slice = SubsSlice::insert_into_subs(env.subs, once(record_var));
    let fn_var = synth_var(
        env.subs,
        Content::Structure(FlatType::Func(record_var_slice, fn_clos_var, encoder_var)),
    );

    // \rcd -[fn_name]-> Encode.record [ { key: .., value: .. }, .. ]
    Closure(ClosureData {
        function_type: fn_var,
        closure_type: fn_clos_var,
        return_type: encoder_var,
        name: fn_name,
        captured_symbols: vec![],
        recursive: Recursive::NotRecursive,
        arguments: vec![(
            record_var,
            AnnotatedMark::known_exhaustive(),
            Loc::at_zero(Pattern::Identifier(rcd_sym)),
        )],
        loc_body: Box::new(Loc::at_zero(encode_record_call)),
    })
}

fn to_encoder_tag_union(env: &mut Env<'_>, tag_union_var: Variable, tags: UnionTags) -> Expr {
    // Suppose tag = [ A t1 t2, B t3 ]. Build
    //
    // \tag -> when tag is
    //     A v1 v2 -> Encode.tag "A" [ Encode.toEncoder v1, Encode.toEncoder v2 ]
    //     B v3 -> Encode.tag "B" [ Encode.toEncoder v3 ]

    let tag_sym = env.unique_symbol();
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

            // whole type of the elements in [ Encode.toEncoder v1, Encode.toEncoder v2 ]
            let whole_payload_encoders_var = env.subs.fresh_unnamed_flex_var();
            // [ Encode.toEncoder v1, Encode.toEncoder v2 ]
            let payload_to_encoders = (payload_syms.iter())
                .zip(payload_vars.iter())
                .map(|(&sym, &sym_var)| {
                    // build `toEncoder v1` type
                    // expected: val -[uls]-> Encoder fmt | fmt has EncoderFormatting
                    let to_encoder_fn_var = env.import_encode_symbol(Symbol::ENCODE_TO_ENCODER);

                    // wanted: t1 -[clos]-> t'
                    let var_slice_of_sym_var =
                        VariableSubsSlice::insert_into_subs(env.subs, [sym_var]); // [ t1 ]
                    let to_encoder_clos_var = env.subs.fresh_unnamed_flex_var(); // clos
                    let encoder_var = env.subs.fresh_unnamed_flex_var(); // t'
                    let this_to_encoder_fn_var = synth_var(
                        env.subs,
                        Content::Structure(FlatType::Func(
                            var_slice_of_sym_var,
                            to_encoder_clos_var,
                            encoder_var,
                        )),
                    );

                    //   val -[uls]->  Encoder fmt | fmt has EncoderFormatting
                    // ~ t1  -[clos]-> t'
                    env.unify(to_encoder_fn_var, this_to_encoder_fn_var);

                    // toEncoder : t1 -[clos]-> Encoder fmt | fmt has EncoderFormatting
                    let to_encoder_fn = Box::new((
                        this_to_encoder_fn_var,
                        Loc::at_zero(Var(Symbol::ENCODE_TO_ENCODER)),
                        to_encoder_clos_var,
                        encoder_var,
                    ));

                    // toEncoder rcd.a
                    let to_encoder_call = Call(
                        to_encoder_fn,
                        vec![(sym_var, Loc::at_zero(Var(sym)))],
                        CalledVia::Space,
                    );

                    // NOTE: must be done to unify the lambda sets under `encoder_var`
                    env.unify(encoder_var, whole_payload_encoders_var);

                    Loc::at_zero(to_encoder_call)
                })
                .collect();

            // typeof [ Encode.toEncoder v1, Encode.toEncoder v2 ]
            let whole_encoders_var_slice =
                VariableSubsSlice::insert_into_subs(env.subs, [whole_payload_encoders_var]);
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
            // expected: Str, List (Encoder fmt) -[uls]-> Encoder fmt | fmt has EncoderFormatting
            let encode_tag_fn_var = env.import_encode_symbol(Symbol::ENCODE_TAG);

            // wanted: Str, List whole_encoders_var -[clos]-> t'
            // wanted: Str, List whole_encoders_var
            let this_encode_tag_args_var_slice = VariableSubsSlice::insert_into_subs(
                env.subs,
                [Variable::STR, payload_encoders_list_var],
            );
            let this_encode_tag_clos_var = env.subs.fresh_unnamed_flex_var(); // -[clos]->
            let this_encoder_var = env.subs.fresh_unnamed_flex_var(); // t'
            let this_encode_tag_fn_var = synth_var(
                env.subs,
                Content::Structure(FlatType::Func(
                    this_encode_tag_args_var_slice,
                    this_encode_tag_clos_var,
                    this_encoder_var,
                )),
            );

            //   Str, List (Encoder fmt)      -[uls]->  Encoder fmt | fmt has EncoderFormatting
            // ~ Str, List whole_encoders_var -[clos]-> t'
            env.unify(encode_tag_fn_var, this_encode_tag_fn_var);

            // Encode.tag : Str, List whole_encoders_var -[clos]-> Encoder fmt | fmt has EncoderFormatting
            let encode_tag_fn = Box::new((
                this_encode_tag_fn_var,
                Loc::at_zero(Var(Symbol::ENCODE_TO_ENCODER)),
                this_encode_tag_clos_var,
                this_encoder_var,
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
                patterns: vec![Loc::at_zero(pattern)],
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
        loc_cond: Box::new(Loc::at_zero(Var(tag_sym))),
        cond_var: tag_union_var,
        expr_var: whole_tag_encoders_var,
        region: Region::zero(),
        branches,
        branches_cond_var: tag_union_var,
        exhaustive: ExhaustiveMark::known_exhaustive(),
    };

    let fn_name = env.unique_symbol();
    let fn_name_labels = UnionLambdas::insert_into_subs(env.subs, once((fn_name, vec![])));
    // -[fn_name]->
    let fn_clos_var = synth_var(
        env.subs,
        Content::LambdaSet(LambdaSet {
            solved: fn_name_labels,
            recursion_var: OptVariable::NONE,
            unspecialized: SubsSlice::default(),
        }),
    );
    // tag_union_var -[fn_name]-> whole_tag_encoders_var
    let tag_union_var_slice = SubsSlice::insert_into_subs(env.subs, once(tag_union_var));
    let fn_var = synth_var(
        env.subs,
        Content::Structure(FlatType::Func(
            tag_union_var_slice,
            fn_clos_var,
            whole_tag_encoders_var,
        )),
    );

    // \tag -> when tag is
    //     A v1 v2 -> Encode.tag "A" [ Encode.toEncoder v1, Encode.toEncoder v2 ]
    //     B v3 -> Encode.tag "B" [ Encode.toEncoder v3 ]
    Closure(ClosureData {
        function_type: fn_var,
        closure_type: fn_clos_var,
        return_type: whole_tag_encoders_var,
        name: fn_name,
        captured_symbols: vec![],
        recursive: Recursive::NotRecursive,
        arguments: vec![(
            tag_union_var,
            AnnotatedMark::known_exhaustive(),
            Loc::at_zero(Pattern::Identifier(tag_sym)),
        )],
        loc_body: Box::new(Loc::at_zero(when_branches)),
    })
}
