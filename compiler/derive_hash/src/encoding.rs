use roc_error_macros::internal_error;
use roc_module::{
    ident::{Lowercase, TagName},
    symbol::Symbol,
};
use roc_types::subs::{Content, FlatType, GetSubsSlice, Subs, SubsFmtContent, Variable};

#[derive(Hash)]
pub enum FlatEncodable<'a> {
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

macro_rules! unexpected {
    ($subs:expr, $var:expr) => {
        internal_error!(
            "Invalid content for toEncoder: {:?}",
            SubsFmtContent($subs.get_content_without_compacting($var), $subs)
        )
    };
}

impl FlatEncodable<'_> {
    pub(crate) fn from_var(subs: &Subs, var: Variable) -> FlatEncodable {
        match *subs.get_content_without_compacting(var) {
            Content::Structure(flat_type) => match flat_type {
                FlatType::Apply(sym, _) => match sym {
                    Symbol::LIST_LIST => FlatEncodable::List(),
                    Symbol::SET_SET => FlatEncodable::Set(),
                    Symbol::DICT_DICT => FlatEncodable::Dict(),
                    Symbol::STR_STR => FlatEncodable::Str,
                    _ => unexpected!(subs, var),
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
                FlatType::Erroneous(_) => unexpected!(subs, var),
                FlatType::Func(..) => unexpected!(subs, var),
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
                // TODO: I believe it is okay to unwrap opaques here because derivers are only used
                // by the backend, and the backend treats opaques like structural aliases.
                _ => Self::from_var(subs, real_var),
            },
            Content::RangedNumber(real_var, _) => Self::from_var(subs, real_var),
            //
            Content::RecursionVar { .. } => unexpected!(subs, var),
            Content::Error => unexpected!(subs, var),
            Content::FlexVar(_)
            | Content::RigidVar(_)
            | Content::FlexAbleVar(_, _)
            | Content::RigidAbleVar(_, _) => unexpected!(subs, var),
            Content::LambdaSet(_) => unexpected!(subs, var),
        }
    }
}
