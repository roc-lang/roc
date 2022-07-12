//! Derivers for the `Encoding` ability.

use std::iter::once;

use roc_can::abilities::SpecializationLambdaSets;
use roc_can::expr::{AnnotatedMark, ClosureData, Expr, Field, Recursive, WhenBranch};
use roc_can::module::ExposedByModule;
use roc_can::pattern::Pattern;
use roc_collections::SendMap;
use roc_derive_key::encoding::FlatEncodableKey;
use roc_error_macros::internal_error;
use roc_module::called_via::CalledVia;
use roc_module::ident::Lowercase;
use roc_module::symbol::{IdentIds, ModuleId, Symbol};
use roc_region::all::{Loc, Region};
use roc_types::subs::{
    instantiate_rigids, Content, ExhaustiveMark, FlatType, GetSubsSlice, LambdaSet, OptVariable,
    RecordFields, RedundantMark, Subs, SubsFmtContent, SubsSlice, UnionLambdas, UnionTags,
    Variable, VariableSubsSlice,
};
use roc_types::types::{AliasKind, RecordField};

use crate::{synth_var, DerivedBody, DERIVED_MODULE};

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

pub(crate) struct Env<'a> {
    /// NB: This **must** be subs for the derive module!
    pub subs: &'a mut Subs,
    pub exposed_types: &'a ExposedByModule,
    pub derived_ident_ids: &'a mut IdentIds,
}

impl Env<'_> {
    fn new_symbol(&mut self, name_hint: &str) -> Symbol {
        if cfg!(any(
            debug_assertions,
            test,
            feature = "debug-derived-symbols"
        )) {
            let mut i = 0;
            let debug_name = loop {
                i += 1;
                let name = if i == 1 {
                    name_hint.to_owned()
                } else {
                    format!("{}{}", name_hint, i)
                };
                if self.derived_ident_ids.get_id(&name).is_none() {
                    break name;
                }
            };

            let ident_id = self.derived_ident_ids.get_or_insert(&debug_name);

            Symbol::new(DERIVED_MODULE, ident_id)
        } else {
            self.unique_symbol()
        }
    }

    fn unique_symbol(&mut self) -> Symbol {
        let ident_id = self.derived_ident_ids.gen_unique();
        Symbol::new(DERIVED_MODULE, ident_id)
    }

    fn import_encode_symbol(&mut self, symbol: Symbol) -> Variable {
        debug_assert_eq!(symbol.module_id(), ModuleId::ENCODE);

        let encode_types = &self
            .exposed_types
            .get(&ModuleId::ENCODE)
            .unwrap()
            .exposed_types_storage_subs;
        let storage_var = encode_types.stored_vars_by_symbol.get(&symbol).unwrap();
        let imported = encode_types
            .storage_subs
            .export_variable_to_directly_to_use_site(self.subs, *storage_var);

        instantiate_rigids(self.subs, imported.variable);

        imported.variable
    }

    fn unify(&mut self, left: Variable, right: Variable) {
        use roc_unify::unify::{unify, Mode, Unified};

        let unified = unify(self.subs, left, right, Mode::EQ);

        match unified {
            Unified::Success {
                vars: _,
                must_implement_ability: _,
                lambda_sets_to_specialize,
                extra_metadata: _,
            } => {
                if !lambda_sets_to_specialize.is_empty() {
                    internal_error!("Did not expect derivers to need to specialize unspecialized lambda sets, but we got some: {:?}", lambda_sets_to_specialize)
                }
            }
            Unified::Failure(..) | Unified::BadType(..) => {
                internal_error!("Unification failed in deriver - that's a deriver bug!")
            }
        }
    }

    fn get_specialization_lambda_sets(
        &mut self,
        specialization_type: Variable,
        ability_member: Symbol,
    ) -> SpecializationLambdaSets {
        use roc_unify::unify::{unify_introduced_ability_specialization, Mode, Unified};

        let member_signature = self.import_encode_symbol(ability_member);

        let unified = unify_introduced_ability_specialization(
            self.subs,
            member_signature,
            specialization_type,
            Mode::EQ,
        );

        match unified {
            Unified::Success {
                vars: _,
                must_implement_ability: _,
                lambda_sets_to_specialize: _lambda_sets_to_specialize,
                extra_metadata: specialization_lsets,
            } => {
                let specialization_lsets: SpecializationLambdaSets = specialization_lsets
                    .0
                    .into_iter()
                    .map(|((spec_member, region), var)| {
                        debug_assert_eq!(spec_member, ability_member);
                        (region, var)
                    })
                    .collect();

                // Since we're doing `{foo} ~ a | a has Encoding`, we may see "lambda sets to
                // specialize" for e.g. `{foo}:toEncoder:1`, but these are actually just the
                // specialization lambda sets, so we don't need to do any extra work!
                //
                // If there are other lambda sets to specialize in here, that's unexpected, because
                // that means we would have been deriving something like `toEncoder {foo: bar}`,
                // and now seen that we needed `toEncoder bar` where `bar` is a concrete type. But
                // we only expect `bar` to polymorphic at this stage!
                //
                // TODO: it would be better if `unify` could prune these for us. See also
                // https://github.com/rtfeldman/roc/issues/3207; that is a blocker for this TODO.
                #[cfg(debug_assertions)]
                {
                    for (spec_var, lambda_sets) in _lambda_sets_to_specialize.drain() {
                        for lambda_set in lambda_sets {
                            let belongs_to_specialized_lambda_sets =
                                specialization_lsets.iter().any(|(_, var)| {
                                    self.subs.get_root_key_without_compacting(*var)
                                        == self.subs.get_root_key_without_compacting(lambda_set)
                                });
                            debug_assert!(belongs_to_specialized_lambda_sets,
                                "Did not expect derivers to need to specialize unspecialized lambda sets, but we got one: {:?} for {:?}", lambda_set, spec_var)
                        }
                    }
                }
                specialization_lsets
            }
            Unified::Failure(..) | Unified::BadType(..) => {
                internal_error!("Unification failed in deriver - that's a deriver bug!")
            }
        }
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

pub(crate) fn derive_to_encoder(
    env: &mut Env<'_>,
    key: FlatEncodableKey,
    def_symbol: Symbol,
) -> DerivedBody {
    let (body, body_type) = match key {
        FlatEncodableKey::String => to_encoder_string(env, def_symbol),
        FlatEncodableKey::List() => todo!(),
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
        FlatEncodableKey::TagUnion(tags) => {
            // Generalized tag union var so we can reuse this impl between many unions:
            // if tags = [ A arity=2, B arity=1 ], this is [ A t1 t2, B t3 ] for fresh t1, t2, t3
            let flex_tag_labels = tags
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

fn to_encoder_string(env: &mut Env<'_>, fn_name: Symbol) -> (Expr, Variable) {
    // Build \s -> Encode.string s

    use Expr::*;

    let s_sym = env.new_symbol("s");

    // build `Encode.string s` type
    // Str -[uls]-> Encoder fmt | fmt has EncoderFormatting
    let encode_string_fn_var = env.import_encode_symbol(Symbol::ENCODE_STRING);

    // Str -[clos]-> t1
    let string_var_slice = VariableSubsSlice::insert_into_subs(env.subs, once(Variable::STR)); // TODO: consider caching this singleton slice
    let encode_string_clos_var = env.subs.fresh_unnamed_flex_var(); // clos
    let encoder_var = env.subs.fresh_unnamed_flex_var(); // t1
    let this_encode_string_fn_var = synth_var(
        env.subs,
        Content::Structure(FlatType::Func(
            string_var_slice,
            encode_string_clos_var,
            encoder_var,
        )),
    );

    //   Str -[uls]->  Encoder fmt | fmt has EncoderFormatting
    // ~ Str -[clos]-> t1
    env.unify(encode_string_fn_var, this_encode_string_fn_var);

    // Encode.string : Str -[clos]-> Encoder fmt | fmt has EncoderFormatting
    let encode_string_var = AbilityMember(Symbol::ENCODE_STRING, None, encode_string_fn_var);
    let encode_record_fn = Box::new((
        encode_string_fn_var,
        Loc::at_zero(encode_string_var),
        encode_string_clos_var,
        encoder_var,
    ));

    // Encode.string s
    let encode_string_call = Call(
        encode_record_fn,
        vec![(Variable::STR, Loc::at_zero(Var(s_sym)))],
        CalledVia::Space,
    );

    // Encode.custom \bytes, fmt -> Encode.appendWith bytes (Encode.string s) fmt
    let (body, this_encoder_var) =
        wrap_in_encode_custom(env, encode_string_call, encoder_var, s_sym, Variable::STR);

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
    // Str -[fn_name]-> (typeof Encode.record [ .. ] = Encoder fmt)
    env.subs.set_content(
        fn_var,
        Content::Structure(FlatType::Func(
            string_var_slice,
            fn_clos_var,
            this_encoder_var,
        )),
    );

    // \rcd -[fn_name]-> Encode.record [ { key: .., value: .. }, .. ]
    let clos = Closure(ClosureData {
        function_type: fn_var,
        closure_type: fn_clos_var,
        return_type: this_encoder_var,
        name: fn_name,
        captured_symbols: vec![],
        recursive: Recursive::NotRecursive,
        arguments: vec![(
            Variable::STR,
            AnnotatedMark::known_exhaustive(),
            Loc::at_zero(Pattern::Identifier(s_sym)),
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
            let to_encoder_var = AbilityMember(Symbol::ENCODE_TO_ENCODER, None, to_encoder_fn_var);
            let to_encoder_fn = Box::new((
                to_encoder_fn_var,
                Loc::at_zero(to_encoder_var),
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
    let encode_record_var = AbilityMember(Symbol::ENCODE_RECORD, None, encode_record_fn_var);
    let encode_record_fn = Box::new((
        encode_record_fn_var,
        Loc::at_zero(encode_record_var),
        encode_record_clos_var,
        encoder_var,
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
    let record_var_slice = SubsSlice::insert_into_subs(env.subs, once(record_var));
    env.subs.set_content(
        fn_var,
        Content::Structure(FlatType::Func(
            record_var_slice,
            fn_clos_var,
            this_encoder_var,
        )),
    );

    // \rcd -[fn_name]-> Encode.record [ { key: .., value: .. }, .. ]
    let clos = Closure(ClosureData {
        function_type: fn_var,
        closure_type: fn_clos_var,
        return_type: this_encoder_var,
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
                    let to_encoder_var =
                        AbilityMember(Symbol::ENCODE_TO_ENCODER, None, this_to_encoder_fn_var);
                    let to_encoder_fn = Box::new((
                        this_to_encoder_fn_var,
                        Loc::at_zero(to_encoder_var),
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
            let encode_tag_var = AbilityMember(Symbol::ENCODE_TAG, None, this_encode_tag_fn_var);
            let encode_tag_fn = Box::new((
                this_encode_tag_fn_var,
                Loc::at_zero(encode_tag_var),
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
    let tag_union_var_slice = SubsSlice::insert_into_subs(env.subs, once(tag_union_var));
    env.subs.set_content(
        fn_var,
        Content::Structure(FlatType::Func(
            tag_union_var_slice,
            fn_clos_var,
            this_encoder_var,
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

    // fmt: fmt | fmt has EncoderFormatting
    let fmt_sym = env.new_symbol("fmt");
    let fmt_var = env.subs.fresh_unnamed_flex_var();

    // build `Encode.appendWith bytes encoder fmt` type
    // expected: Encode.appendWith : List U8, Encoder fmt, fmt -[appendWith]-> List U8 | fmt has EncoderFormatting
    let append_with_fn_var = env.import_encode_symbol(Symbol::ENCODE_APPEND_WITH);

    // wanted: Encode.appendWith : List U8, encoder_var, fmt -[clos]-> List U8 | fmt has EncoderFormatting
    let this_append_with_args_var_slice =
        VariableSubsSlice::insert_into_subs(env.subs, [Variable::LIST_U8, encoder_var, fmt_var]);
    let this_append_with_clos_var = env.subs.fresh_unnamed_flex_var(); // -[clos]->
    let this_append_with_fn_var = synth_var(
        env.subs,
        Content::Structure(FlatType::Func(
            this_append_with_args_var_slice,
            this_append_with_clos_var,
            Variable::LIST_U8,
        )),
    );

    //   List U8, Encoder fmt, fmt -[appendWith]-> List U8 | fmt has EncoderFormatting
    // ~ List U8, encoder_var, fmt -[clos]->       List U8 | fmt has EncoderFormatting
    env.unify(append_with_fn_var, this_append_with_fn_var);

    // Encode.appendWith : List U8, encoder_var, fmt -[appendWith]-> List U8 | fmt has EncoderFormatting
    let append_with_fn = Box::new((
        this_append_with_fn_var,
        Loc::at_zero(Var(Symbol::ENCODE_APPEND_WITH)),
        this_append_with_clos_var,
        Variable::LIST_U8,
    ));

    // Encode.appendWith bytes encoder fmt
    let append_with_call = Call(
        append_with_fn,
        vec![
            // (bytes_var, bytes)
            (bytes_var, Loc::at_zero(Var(bytes_sym))),
            // (encoder_var, encoder)
            (encoder_var, Loc::at_zero(encoder)),
            // (fmt, fmt_var)
            (fmt_var, Loc::at_zero(Var(fmt_sym))),
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
    let args_slice = SubsSlice::insert_into_subs(env.subs, vec![bytes_var, fmt_var]);
    env.subs.set_content(
        fn_var,
        Content::Structure(FlatType::Func(args_slice, fn_clos_var, Variable::LIST_U8)),
    );

    // \bytes, fmt -[[fn_name captured_var]]-> Encode.appendWith bytes encoder fmt
    let clos = Closure(ClosureData {
        function_type: fn_var,
        closure_type: fn_clos_var,
        return_type: Variable::LIST_U8,
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
    // expected: Encode.custom : (List U8, fmt -> List U8) -> Encoder fmt | fmt has EncoderFormatting
    let custom_fn_var = env.import_encode_symbol(Symbol::ENCODE_CUSTOM);

    // wanted: Encode.custom : fn_var -[clos]-> t'
    let this_custom_args_var_slice = VariableSubsSlice::insert_into_subs(env.subs, [fn_var]);
    let this_custom_clos_var = env.subs.fresh_unnamed_flex_var(); // -[clos]->
    let this_custom_encoder_var = env.subs.fresh_unnamed_flex_var(); // t'
    let this_custom_fn_var = synth_var(
        env.subs,
        Content::Structure(FlatType::Func(
            this_custom_args_var_slice,
            this_custom_clos_var,
            this_custom_encoder_var,
        )),
    );

    //   (List U8, fmt -> List U8) -[..]->   Encoder fmt | fmt has EncoderFormatting
    // ~ fn_var                    -[clos]-> t'
    env.unify(custom_fn_var, this_custom_fn_var);

    // Encode.custom : (List U8, fmt -> List U8) -> Encoder fmt | fmt has EncoderFormatting
    let custom_fn = Box::new((
        this_custom_fn_var,
        Loc::at_zero(Var(Symbol::ENCODE_CUSTOM)),
        this_custom_clos_var,    // -[clos]->
        this_custom_encoder_var, // t' ~ Encoder fmt
    ));

    // Encode.custom \bytes, fmt -> Encode.appendWith bytes encoder fmt
    let custom_call = Call(
        custom_fn,
        vec![(fn_var, Loc::at_zero(clos))],
        CalledVia::Space,
    );

    (custom_call, this_custom_encoder_var)
}
