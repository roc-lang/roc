//! Derivers for the `Encoding` ability.

use std::iter::once;

use bumpalo::Bump;

use roc_can::abilities::AbilitiesStore;
use roc_can::expr::{AnnotatedMark, ClosureData, Expr, Field, Recursive};
use roc_can::pattern::Pattern;
use roc_collections::SendMap;
use roc_error_macros::internal_error;
use roc_late_solve::{instantiate_rigids, AbilitiesView};
use roc_module::called_via::CalledVia;
use roc_module::ident::{Lowercase, TagName};
use roc_module::symbol::{IdentIds, ModuleId, Symbol};
use roc_region::all::{Loc, Region};
use roc_types::subs::{
    Content, ExposedTypesStorageSubs, FlatType, GetSubsSlice, LambdaSet, OptVariable, RecordFields,
    Subs, SubsFmtContent, SubsSlice, UnionLambdas, Variable, VariableSubsSlice,
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

#[derive(Hash)]
pub(super) enum FlatEncodable<'a> {
    U8,
    U16,
    U32,
    U64,
    U128,
    I8,
    I16,
    I32,
    I64,
    I128,
    Dec,
    F32,
    F64,
    List(/* takes one variable */),
    Set(/* takes one variable */),
    Dict(/* takes two variables */),
    Str,
    // Unfortunate that we must allocate here, c'est la vie
    Record(Vec<&'a Lowercase>),
    TagUnion(Vec<(&'a TagName, u16)>),
}

impl FlatEncodable<'_> {
    pub fn from_var(subs: &Subs, var: Variable) -> FlatEncodable {
        match *subs.get_content_without_compacting(var) {
            Content::Structure(flat_type) => match flat_type {
                FlatType::Apply(sym, _) => match sym {
                    Symbol::LIST_LIST => FlatEncodable::List(),
                    Symbol::SET_SET => FlatEncodable::Set(),
                    Symbol::DICT_DICT => FlatEncodable::Dict(),
                    Symbol::STR_STR => FlatEncodable::Str,
                    _ => bad_input!(subs, var),
                },
                FlatType::Record(fields, ext) => {
                    debug_assert!(matches!(
                        subs.get_content_without_compacting(ext),
                        Content::Structure(FlatType::EmptyRecord)
                    ));

                    let mut field_names: Vec<_> =
                        subs.get_subs_slice(fields.field_names()).iter().collect();
                    field_names.sort();
                    FlatEncodable::Record(field_names)
                }
                FlatType::TagUnion(tags, ext) | FlatType::RecursiveTagUnion(_, tags, ext) => {
                    // The recursion var doesn't matter, because the derived implementation will only
                    // look on the surface of the tag union type, and more over the payloads of the
                    // arguments will be left generic for the monomorphizer to fill in with the
                    // appropriate type. That is,
                    //   [ A t1, B t1 t2 ]
                    // and
                    //   [ A t1, B t1 t2 ] as R
                    // look the same on the surface, because `R` is only somewhere inside of the
                    // `t`-prefixed payload types.
                    debug_assert!(matches!(
                        subs.get_content_without_compacting(ext),
                        Content::Structure(FlatType::EmptyTagUnion)
                    ));
                    let mut tag_names_and_payload_sizes: Vec<_> = tags
                        .iter_all()
                        .map(|(name_index, payload_slice_index)| {
                            let payload_slice = subs[payload_slice_index];
                            let payload_size = payload_slice.length;
                            let name = &subs[name_index];
                            (name, payload_size)
                        })
                        .collect();
                    tag_names_and_payload_sizes.sort_by_key(|t| t.0);
                    FlatEncodable::TagUnion(tag_names_and_payload_sizes)
                }
                FlatType::FunctionOrTagUnion(name_index, _, _) => {
                    FlatEncodable::TagUnion(vec![(&subs[name_index], 0)])
                }
                FlatType::EmptyRecord => FlatEncodable::Record(vec![]),
                FlatType::EmptyTagUnion => FlatEncodable::TagUnion(vec![]),
                //
                FlatType::Erroneous(_) => bad_input!(subs, var),
                FlatType::Func(..) => bad_input!(subs, var, "functions cannot be encoded"),
            },
            Content::Alias(sym, _, real_var, _) => match sym {
                Symbol::NUM_U8 => FlatEncodable::U8,
                Symbol::NUM_U16 => FlatEncodable::U16,
                Symbol::NUM_U32 => FlatEncodable::U32,
                Symbol::NUM_U64 => FlatEncodable::U64,
                Symbol::NUM_U128 => FlatEncodable::U128,
                Symbol::NUM_I8 => FlatEncodable::I8,
                Symbol::NUM_I16 => FlatEncodable::I16,
                Symbol::NUM_I32 => FlatEncodable::I32,
                Symbol::NUM_I64 => FlatEncodable::I64,
                Symbol::NUM_I128 => FlatEncodable::I128,
                Symbol::NUM_DEC => FlatEncodable::Dec,
                Symbol::NUM_F32 => FlatEncodable::F32,
                Symbol::NUM_F64 => FlatEncodable::F64,
                _ => Self::from_var(subs, real_var),
            },
            Content::RangedNumber(real_var, _) => Self::from_var(subs, real_var),
            //
            Content::RecursionVar { .. } => bad_input!(subs, var),
            Content::Error => bad_input!(subs, var),
            Content::FlexVar(_)
            | Content::RigidVar(_)
            | Content::FlexAbleVar(_, _)
            | Content::RigidAbleVar(_, _) => bad_input!(subs, var, "flex vars cannot be encoded"),
            Content::LambdaSet(_) => bad_input!(subs, var, "errors cannot be encoded"),
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

pub fn derive_to_encoder(env: &mut Env<'_>, for_var: Variable) -> Expr {
    to_encoder_from_var(env, for_var)
}

fn to_encoder_from_var(env: &mut Env<'_>, mut var: Variable) -> Expr {
    loop {
        match *env.subs.get_content_without_compacting(var) {
            Content::Alias(_, _, real_var, _) => var = real_var,
            Content::RangedNumber(real_var, _) => var = real_var,

            Content::RecursionVar { .. } => todo!(),
            Content::LambdaSet(_) => todo!(),
            Content::Structure(flat_type) => match flat_type {
                FlatType::Record(fields, ext_var) => {
                    return to_encoder_record(env, var, fields, ext_var)
                }
                FlatType::EmptyRecord => {
                    return to_encoder_record(env, var, RecordFields::empty(), var)
                }

                FlatType::Apply(_, _) => todo!(),
                FlatType::TagUnion(_, _) => todo!(),
                FlatType::FunctionOrTagUnion(_, _, _) => todo!(),
                FlatType::RecursiveTagUnion(_, _, _) => todo!(),
                FlatType::EmptyTagUnion => todo!(),

                FlatType::Func(..) => bad_input!(env.subs, var, "functions cannot be encoded"),
                FlatType::Erroneous(_) => bad_input!(env.subs, var),
            },

            Content::FlexVar(_)
            | Content::RigidVar(_)
            | Content::FlexAbleVar(_, _)
            | Content::RigidAbleVar(_, _) => bad_input!(env.subs, var, "unresolved variable"),
            Content::Error => bad_input!(env.subs, var),
        }
    }
}

fn to_encoder_record(
    env: &mut Env<'_>,
    record_var: Variable,
    fields: RecordFields,
    ext_var: Variable,
) -> Expr {
    // Suppose rcd = { a: t1, b: t2 }. Build
    //
    // \rcd -> Encode.record [
    //      { key: "a", value: Encode.toEncoder rcd.a },
    //      { key: "b", value: Encode.toEncoder rcd.b },
    //   ]

    debug_assert!(matches!(
        env.subs.get_content_without_compacting(ext_var),
        Content::Structure(FlatType::EmptyRecord)
    ));
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
