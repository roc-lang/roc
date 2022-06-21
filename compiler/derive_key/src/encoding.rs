use roc_error_macros::internal_error;
use roc_module::{
    ident::{Lowercase, TagName},
    symbol::Symbol,
};
use roc_types::subs::{Content, FlatType, GetSubsSlice, Subs, SubsFmtContent, Variable};

#[derive(Hash)]
pub enum FlatEncodable {
    Immediate(Symbol),
    Key(FlatEncodableKey),
}

#[derive(Hash, PartialEq, Eq, Debug)]
pub enum FlatEncodableKey {
    List(/* takes one variable */),
    Set(/* takes one variable */),
    Dict(/* takes two variables */),
    // Unfortunate that we must allocate here, c'est la vie
    Record(Vec<Lowercase>),
    TagUnion(Vec<(TagName, u16)>),
}

impl FlatEncodableKey {
    pub(crate) fn debug_name(&self) -> String {
        match self {
            FlatEncodableKey::List() => "list".to_string(),
            FlatEncodableKey::Set() => "set".to_string(),
            FlatEncodableKey::Dict() => "dict".to_string(),
            FlatEncodableKey::Record(fields) => {
                let mut str = String::from('{');
                fields.iter().enumerate().for_each(|(i, f)| {
                    if i > 0 {
                        str.push(',');
                    }
                    str.push_str(f.as_str());
                });
                str.push('}');
                str
            }
            FlatEncodableKey::TagUnion(tags) => {
                let mut str = String::from('[');
                tags.iter().enumerate().for_each(|(i, (tag, arity))| {
                    if i > 0 {
                        str.push(',');
                    }
                    str.push_str(tag.0.as_str());
                    str.push(' ');
                    str.push_str(&arity.to_string());
                });
                str.push(']');
                str
            }
        }
    }
}

macro_rules! unexpected {
    ($subs:expr, $var:expr) => {
        internal_error!(
            "Invalid content for toEncoder: {:?}",
            SubsFmtContent($subs.get_content_without_compacting($var), $subs)
        )
    };
}

impl FlatEncodable {
    pub(crate) fn from_var(subs: &Subs, var: Variable) -> FlatEncodable {
        use FlatEncodable::*;
        match *subs.get_content_without_compacting(var) {
            Content::Structure(flat_type) => match flat_type {
                FlatType::Apply(sym, _) => match sym {
                    Symbol::LIST_LIST => Key(FlatEncodableKey::List()),
                    Symbol::SET_SET => Key(FlatEncodableKey::Set()),
                    Symbol::DICT_DICT => Key(FlatEncodableKey::Dict()),
                    Symbol::STR_STR => Immediate(Symbol::ENCODE_STRING),
                    _ => unexpected!(subs, var),
                },
                FlatType::Record(fields, ext) => {
                    debug_assert!(matches!(
                        subs.get_content_without_compacting(ext),
                        Content::Structure(FlatType::EmptyRecord)
                    ));

                    let mut field_names: Vec<_> = subs
                        .get_subs_slice(fields.field_names())
                        .iter()
                        .cloned()
                        .collect();
                    field_names.sort();
                    Key(FlatEncodableKey::Record(field_names))
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
                            (name.clone(), payload_size)
                        })
                        .collect();
                    tag_names_and_payload_sizes.sort_by(|(t1, _), (t2, _)| t1.cmp(t2));
                    Key(FlatEncodableKey::TagUnion(tag_names_and_payload_sizes))
                }
                FlatType::FunctionOrTagUnion(name_index, _, _) => Key(FlatEncodableKey::TagUnion(
                    vec![(subs[name_index].clone(), 0)],
                )),
                FlatType::EmptyRecord => Key(FlatEncodableKey::Record(vec![])),
                FlatType::EmptyTagUnion => Key(FlatEncodableKey::TagUnion(vec![])),
                //
                FlatType::Erroneous(_) => unexpected!(subs, var),
                FlatType::Func(..) => unexpected!(subs, var),
            },
            Content::Alias(sym, _, real_var, _) => match sym {
                Symbol::NUM_U8 => Immediate(Symbol::ENCODE_U8),
                Symbol::NUM_U16 => Immediate(Symbol::ENCODE_U16),
                Symbol::NUM_U32 => Immediate(Symbol::ENCODE_U32),
                Symbol::NUM_U64 => Immediate(Symbol::ENCODE_U64),
                Symbol::NUM_U128 => Immediate(Symbol::ENCODE_U128),
                Symbol::NUM_I8 => Immediate(Symbol::ENCODE_I8),
                Symbol::NUM_I16 => Immediate(Symbol::ENCODE_I16),
                Symbol::NUM_I32 => Immediate(Symbol::ENCODE_I32),
                Symbol::NUM_I64 => Immediate(Symbol::ENCODE_I64),
                Symbol::NUM_I128 => Immediate(Symbol::ENCODE_I128),
                Symbol::NUM_DEC => Immediate(Symbol::ENCODE_DEC),
                Symbol::NUM_F32 => Immediate(Symbol::ENCODE_F32),
                Symbol::NUM_F64 => Immediate(Symbol::ENCODE_F64),
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
