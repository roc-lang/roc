use roc_module::{
    ident::{Lowercase, TagName},
    symbol::Symbol,
};
use roc_types::subs::{Content, FlatType, GetSubsSlice, Subs, Variable};

use crate::{
    util::{
        check_derivable_ext_var, debug_name_fn, debug_name_record, debug_name_tag, debug_name_tuple,
    },
    DeriveError,
};

#[derive(Hash)]
pub enum FlatInspectable {
    Immediate(Symbol),
    Key(FlatInspectableKey),
}

#[derive(Hash, PartialEq, Eq, Debug, Clone)]
pub enum FlatInspectableKey {
    List(/* takes one variable */),
    Set(/* takes one variable */),
    Dict(/* takes two variables */),
    // Unfortunate that we must allocate here, c'est la vie
    Record(Vec<Lowercase>),
    Tuple(u32),
    TagUnion(Vec<(TagName, u16)>),
    Function(u32 /* arity; +1 for return type */),
}

impl FlatInspectableKey {
    pub(crate) fn debug_name(&self) -> String {
        match self {
            FlatInspectableKey::List() => "list".to_string(),
            FlatInspectableKey::Set() => "set".to_string(),
            FlatInspectableKey::Dict() => "dict".to_string(),
            FlatInspectableKey::Record(fields) => debug_name_record(fields),
            FlatInspectableKey::Tuple(arity) => debug_name_tuple(*arity),
            FlatInspectableKey::TagUnion(tags) => debug_name_tag(tags),
            FlatInspectableKey::Function(arity) => debug_name_fn(*arity),
        }
    }
}

impl FlatInspectable {
    pub(crate) fn from_var(subs: &Subs, var: Variable) -> FlatInspectable {
        use DeriveError::*;
        use FlatInspectable::*;
        match *subs.get_content_without_compacting(var) {
            Content::Structure(flat_type) => match flat_type {
                FlatType::Apply(sym, _) => match sym {
                    Symbol::LIST_LIST => Key(FlatInspectableKey::List()),
                    Symbol::SET_SET => Key(FlatInspectableKey::Set()),
                    Symbol::DICT_DICT => Key(FlatInspectableKey::Dict()),
                    Symbol::STR_STR => Immediate(Symbol::INSPECT_STR),
                    Symbol::NUM_NUM => {
                        // TODO need to match again to see what type of Num it was, then
                        // use Symbol::INSPECT_WHATEVERNUMBERTYPE
                    }
                    _ => Immediate(Symbol::INSPECT_OPAQUE),
                },
                FlatType::Record(fields, ext) => {
                    let (fields_iter, ext) = fields.unsorted_iterator_and_ext(subs, ext);

                    check_derivable_ext_var(subs, ext, |ext| {
                        // TODO open records can still derive Inspect, but we need to decide how/if to render the ext var.
                        // matches!(ext, Content::Structure(FlatType::EmptyRecord))
                    })?;

                    let mut field_names = Vec::with_capacity(fields.len());
                    for (field_name, _) in fields_iter {
                        field_names.push(field_name.clone());
                    }

                    field_names.sort();

                    Key(FlatInspectableKey::Record(field_names))
                }
                FlatType::Tuple(elems, ext) => {
                    let (elems_iter, ext) = elems.sorted_iterator_and_ext(subs, ext);

                    check_derivable_ext_var(subs, ext, |ext| {
                        // TODO open tuples can still derive Inspect, but we need to decide how/if to render the ext var.
                        // matches!(ext, Content::Structure(FlatType::EmptyTuple))
                    })?;

                    Key(FlatInspectableKey::Tuple(elems_iter.count() as _))
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

                    check_derivable_ext_var(subs, ext.var(), |ext| {
                        // TODO open tag unions can still derive Inspect, but we need to decide how/if to render the ext var.
                        // matches!(ext, Content::Structure(FlatType::EmptyTagUnion))
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

                    Key(FlatInspectableKey::TagUnion(tag_names_and_payload_sizes))
                }
                FlatType::FunctionOrTagUnion(names_index, _, _) => {
                    Key(FlatInspectableKey::TagUnion(
                        subs.get_subs_slice(names_index)
                            .iter()
                            .map(|t| (t.clone(), 0))
                            .collect(),
                    ))
                }
                FlatType::EmptyRecord => Key(FlatInspectableKey::Record(Vec::new())),
                FlatType::EmptyTuple => todo!(),
                FlatType::EmptyTagUnion => Ok(Key(FlatInspectableKey::TagUnion(Vec::new()))),
                //
                FlatType::Func(..) => Err(Underivable),
            },
            Content::Alias(sym, _, real_var, _) => match from_builtin_symbol(sym) {
                Some(lambda) => lambda,
                // TODO: I believe it is okay to unwrap opaques here because derivers are only used
                // by the backend, and the backend treats opaques like structural aliases.
                _ => Self::from_var(subs, real_var),
            },
            Content::RangedNumber(range) => {
                Self::from_var(subs, range.default_compilation_variable())
            }
            //
            Content::RecursionVar { structure, .. } => Self::from_var(subs, structure),
            //
            Content::Error => Err(Underivable),
            Content::FlexVar(_)
            | Content::RigidVar(_)
            | Content::FlexAbleVar(_, _)
            | Content::RigidAbleVar(_, _) => Err(UnboundVar),
            Content::LambdaSet(_) | Content::ErasedLambda => Err(Underivable),
        }
    }

    pub(crate) fn from_builtin_symbol(symbol: Symbol) -> Result<FlatEncodable, DeriveError> {
        from_builtin_symbol(symbol).unwrap_or(Err(DeriveError::Underivable))
    }
}

const fn from_builtin_symbol(symbol: Symbol) -> Option<Result<FlatEncodable, DeriveError>> {
    use FlatEncodable::*;
    match symbol {
        Symbol::BOOL_BOOL => Some(Ok(Immediate(Symbol::ENCODE_BOOL))),
        Symbol::NUM_U8 | Symbol::NUM_UNSIGNED8 => Some(Ok(Immediate(Symbol::ENCODE_U8))),
        Symbol::NUM_U16 | Symbol::NUM_UNSIGNED16 => Some(Ok(Immediate(Symbol::ENCODE_U16))),
        Symbol::NUM_U32 | Symbol::NUM_UNSIGNED32 => Some(Ok(Immediate(Symbol::ENCODE_U32))),
        Symbol::NUM_U64 | Symbol::NUM_UNSIGNED64 => Some(Ok(Immediate(Symbol::ENCODE_U64))),
        Symbol::NUM_U128 | Symbol::NUM_UNSIGNED128 => Some(Ok(Immediate(Symbol::ENCODE_U128))),
        Symbol::NUM_I8 | Symbol::NUM_SIGNED8 => Some(Ok(Immediate(Symbol::ENCODE_I8))),
        Symbol::NUM_I16 | Symbol::NUM_SIGNED16 => Some(Ok(Immediate(Symbol::ENCODE_I16))),
        Symbol::NUM_I32 | Symbol::NUM_SIGNED32 => Some(Ok(Immediate(Symbol::ENCODE_I32))),
        Symbol::NUM_I64 | Symbol::NUM_SIGNED64 => Some(Ok(Immediate(Symbol::ENCODE_I64))),
        Symbol::NUM_I128 | Symbol::NUM_SIGNED128 => Some(Ok(Immediate(Symbol::ENCODE_I128))),
        Symbol::NUM_DEC | Symbol::NUM_DECIMAL => Some(Ok(Immediate(Symbol::ENCODE_DEC))),
        Symbol::NUM_F32 | Symbol::NUM_BINARY32 => Some(Ok(Immediate(Symbol::ENCODE_F32))),
        Symbol::NUM_F64 | Symbol::NUM_BINARY64 => Some(Ok(Immediate(Symbol::ENCODE_F64))),
        Symbol::NUM_NAT | Symbol::NUM_NATURAL => Some(Err(DeriveError::Underivable)),
        _ => None,
    }
}
