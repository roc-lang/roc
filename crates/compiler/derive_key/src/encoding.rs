use roc_module::{
    ident::{Lowercase, TagName},
    symbol::Symbol,
};
use roc_types::subs::{Content, FlatType, GetSubsSlice, Subs, Variable};

use crate::DeriveError;

#[derive(Hash)]
pub enum FlatEncodable {
    Immediate(Symbol),
    Key(FlatEncodableKey),
}

#[derive(Hash, PartialEq, Eq, Debug, Clone)]
pub enum FlatEncodableKey {
    String,
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
            FlatEncodableKey::String => "string".to_string(),
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

fn check_ext_var(
    subs: &Subs,
    ext_var: Variable,
    is_empty_ext: impl Fn(&Content) -> bool,
) -> Result<(), DeriveError> {
    let ext_content = subs.get_content_without_compacting(ext_var);
    if is_empty_ext(ext_content) {
        Ok(())
    } else {
        match ext_content {
            Content::FlexVar(_) => Err(DeriveError::UnboundVar),
            _ => Err(DeriveError::Underivable),
        }
    }
}

impl FlatEncodable {
    pub(crate) fn from_var(subs: &Subs, var: Variable) -> Result<FlatEncodable, DeriveError> {
        use DeriveError::*;
        use FlatEncodable::*;
        match *subs.get_content_without_compacting(var) {
            Content::Structure(flat_type) => match flat_type {
                FlatType::Apply(sym, _) => match sym {
                    Symbol::LIST_LIST => Ok(Key(FlatEncodableKey::List())),
                    Symbol::SET_SET => Ok(Key(FlatEncodableKey::Set())),
                    Symbol::DICT_DICT => Ok(Key(FlatEncodableKey::Dict())),
                    Symbol::STR_STR => Ok(Key(FlatEncodableKey::String)),
                    _ => Err(Underivable),
                },
                FlatType::Record(fields, ext) => {
                    check_ext_var(subs, ext, |ext| {
                        matches!(ext, Content::Structure(FlatType::EmptyRecord))
                    })?;

                    let mut field_names: Vec<_> =
                        subs.get_subs_slice(fields.field_names()).to_vec();
                    field_names.sort();

                    Ok(Key(FlatEncodableKey::Record(field_names)))
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
                    check_ext_var(subs, ext, |ext| {
                        matches!(ext, Content::Structure(FlatType::EmptyTagUnion))
                    })?;

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
                    Ok(Key(FlatEncodableKey::TagUnion(tag_names_and_payload_sizes)))
                }
                FlatType::FunctionOrTagUnion(name_index, _, _) => Ok(Key(
                    FlatEncodableKey::TagUnion(vec![(subs[name_index].clone(), 0)]),
                )),
                FlatType::EmptyRecord => Ok(Key(FlatEncodableKey::Record(vec![]))),
                FlatType::EmptyTagUnion => Ok(Key(FlatEncodableKey::TagUnion(vec![]))),
                //
                FlatType::Erroneous(_) => Err(Underivable),
                FlatType::Func(..) => Err(Underivable),
            },
            Content::Alias(sym, _, real_var, _) => match sym {
                Symbol::NUM_U8 => Ok(Immediate(Symbol::ENCODE_U8)),
                Symbol::NUM_U16 => Ok(Immediate(Symbol::ENCODE_U16)),
                Symbol::NUM_U32 => Ok(Immediate(Symbol::ENCODE_U32)),
                Symbol::NUM_U64 => Ok(Immediate(Symbol::ENCODE_U64)),
                Symbol::NUM_U128 => Ok(Immediate(Symbol::ENCODE_U128)),
                Symbol::NUM_I8 => Ok(Immediate(Symbol::ENCODE_I8)),
                Symbol::NUM_I16 => Ok(Immediate(Symbol::ENCODE_I16)),
                Symbol::NUM_I32 => Ok(Immediate(Symbol::ENCODE_I32)),
                Symbol::NUM_I64 => Ok(Immediate(Symbol::ENCODE_I64)),
                Symbol::NUM_I128 => Ok(Immediate(Symbol::ENCODE_I128)),
                Symbol::NUM_DEC => Ok(Immediate(Symbol::ENCODE_DEC)),
                Symbol::NUM_F32 => Ok(Immediate(Symbol::ENCODE_F32)),
                Symbol::NUM_F64 => Ok(Immediate(Symbol::ENCODE_F64)),
                // TODO: I believe it is okay to unwrap opaques here because derivers are only used
                // by the backend, and the backend treats opaques like structural aliases.
                _ => Self::from_var(subs, real_var),
            },
            Content::RangedNumber(real_var, _) => Self::from_var(subs, real_var),
            //
            Content::RecursionVar { .. } => Err(Underivable),
            Content::Error => Err(Underivable),
            Content::FlexVar(_)
            | Content::RigidVar(_)
            | Content::FlexAbleVar(_, _)
            | Content::RigidAbleVar(_, _) => Err(UnboundVar),
            Content::LambdaSet(_) => Err(Underivable),
        }
    }
}
