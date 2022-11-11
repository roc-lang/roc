use roc_module::{
    ident::{Lowercase, TagName},
    symbol::Symbol,
};
use roc_types::subs::{Content, FlatType, GetSubsSlice, Subs, Variable};

use crate::{
    util::{check_derivable_ext_var, debug_name_record, debug_name_tag},
    DeriveError,
};

#[derive(Hash)]
pub enum FlatEncodable {
    Immediate(Symbol),
    Key(FlatEncodableKey),
}

#[derive(Hash, PartialEq, Eq, Debug, Clone)]
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
            FlatEncodableKey::Record(fields) => debug_name_record(fields),
            FlatEncodableKey::TagUnion(tags) => debug_name_tag(tags),
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
                    Symbol::STR_STR => Ok(Immediate(Symbol::ENCODE_STRING)),
                    _ => Err(Underivable),
                },
                FlatType::Record(fields, ext) => {
                    let (fields_iter, ext) = fields.unsorted_iterator_and_ext(subs, ext);

                    check_derivable_ext_var(subs, ext, |ext| {
                        matches!(ext, Content::Structure(FlatType::EmptyRecord))
                    })?;

                    let mut field_names = Vec::with_capacity(fields.len());
                    for (field_name, _) in fields_iter {
                        field_names.push(field_name.clone());
                    }

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
                    let (tags_iter, ext) = tags.unsorted_tags_and_ext(subs, ext);

                    check_derivable_ext_var(subs, ext, |ext| {
                        matches!(ext, Content::Structure(FlatType::EmptyTagUnion))
                    })?;

                    let mut tag_names_and_payload_sizes: Vec<_> = tags_iter
                        .tags
                        .into_iter()
                        .map(|(name, payload_slice)| {
                            let payload_size = payload_slice.len();
                            (name.clone(), payload_size as _)
                        })
                        .collect();

                    tag_names_and_payload_sizes.sort_by(|(t1, _), (t2, _)| t1.cmp(t2));

                    Ok(Key(FlatEncodableKey::TagUnion(tag_names_and_payload_sizes)))
                }
                FlatType::FunctionOrTagUnion(names_index, _, _) => {
                    Ok(Key(FlatEncodableKey::TagUnion(
                        subs.get_subs_slice(names_index)
                            .iter()
                            .map(|t| (t.clone(), 0))
                            .collect(),
                    )))
                }
                FlatType::EmptyRecord => Ok(Key(FlatEncodableKey::Record(vec![]))),
                FlatType::EmptyTagUnion => Ok(Key(FlatEncodableKey::TagUnion(vec![]))),
                //
                FlatType::Func(..) => Err(Underivable),
            },
            Content::Alias(sym, _, real_var, _) => match sym {
                Symbol::NUM_U8 | Symbol::NUM_UNSIGNED8 => Ok(Immediate(Symbol::ENCODE_U8)),
                Symbol::NUM_U16 | Symbol::NUM_UNSIGNED16 => Ok(Immediate(Symbol::ENCODE_U16)),
                Symbol::NUM_U32 | Symbol::NUM_UNSIGNED32 => Ok(Immediate(Symbol::ENCODE_U32)),
                Symbol::NUM_U64 | Symbol::NUM_UNSIGNED64 => Ok(Immediate(Symbol::ENCODE_U64)),
                Symbol::NUM_U128 | Symbol::NUM_UNSIGNED128 => Ok(Immediate(Symbol::ENCODE_U128)),
                Symbol::NUM_I8 | Symbol::NUM_SIGNED8 => Ok(Immediate(Symbol::ENCODE_I8)),
                Symbol::NUM_I16 | Symbol::NUM_SIGNED16 => Ok(Immediate(Symbol::ENCODE_I16)),
                Symbol::NUM_I32 | Symbol::NUM_SIGNED32 => Ok(Immediate(Symbol::ENCODE_I32)),
                Symbol::NUM_I64 | Symbol::NUM_SIGNED64 => Ok(Immediate(Symbol::ENCODE_I64)),
                Symbol::NUM_I128 | Symbol::NUM_SIGNED128 => Ok(Immediate(Symbol::ENCODE_I128)),
                Symbol::NUM_DEC | Symbol::NUM_DECIMAL => Ok(Immediate(Symbol::ENCODE_DEC)),
                Symbol::NUM_F32 | Symbol::NUM_BINARY32 => Ok(Immediate(Symbol::ENCODE_F32)),
                Symbol::NUM_F64 | Symbol::NUM_BINARY64 => Ok(Immediate(Symbol::ENCODE_F64)),
                // TODO: I believe it is okay to unwrap opaques here because derivers are only used
                // by the backend, and the backend treats opaques like structural aliases.
                _ => Self::from_var(subs, real_var),
            },
            Content::RangedNumber(_) => Err(Underivable),
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
