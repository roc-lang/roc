use roc_module::{ident::Lowercase, symbol::Symbol};
use roc_types::subs::{Content, FlatType, Subs, Variable};

use crate::{
    util::{check_derivable_ext_var, debug_name_record, debug_name_tuple},
    DeriveError,
};

#[derive(Hash)]
pub enum FlatDecodable {
    Immediate(Symbol),
    Key(FlatDecodableKey),
}

#[derive(Hash, PartialEq, Eq, Debug, Clone)]
pub enum FlatDecodableKey {
    List(/* takes one variable */),

    // Unfortunate that we must allocate here, c'est la vie
    Record(Vec<Lowercase>),
    Tuple(u32),
}

impl FlatDecodableKey {
    pub(crate) fn debug_name(&self) -> String {
        match self {
            FlatDecodableKey::List() => "list".to_string(),
            FlatDecodableKey::Record(fields) => debug_name_record(fields),
            FlatDecodableKey::Tuple(arity) => debug_name_tuple(*arity),
        }
    }
}

impl FlatDecodable {
    pub(crate) fn from_var(subs: &Subs, var: Variable) -> Result<FlatDecodable, DeriveError> {
        use DeriveError::*;
        use FlatDecodable::*;
        match *subs.get_content_without_compacting(var) {
            Content::Structure(flat_type) => match flat_type {
                FlatType::Apply(sym, _) => match sym {
                    Symbol::LIST_LIST => Ok(Key(FlatDecodableKey::List())),
                    Symbol::STR_STR => Ok(Immediate(Symbol::DECODE_STRING)),
                    _ => Err(Underivable),
                },
                FlatType::Record(fields, ext) => {
                    let (fields_iter, ext) = fields.unsorted_iterator_and_ext(subs, ext);

                    check_derivable_ext_var(subs, ext, |ext| {
                        matches!(ext, Content::Structure(FlatType::EmptyRecord))
                    })?;

                    let mut field_names = Vec::with_capacity(fields.len());
                    for (field_name, record_field) in fields_iter {
                        if record_field.is_optional() {
                            // Can't derive a concrete decoder for optional fields, since those are
                            // compile-time-polymorphic
                            return Err(Underivable);
                        }
                        field_names.push(field_name.clone());
                    }

                    field_names.sort();

                    Ok(Key(FlatDecodableKey::Record(field_names)))
                }
                FlatType::Tuple(elems, ext) => {
                    let (elems_iter, ext) = elems.sorted_iterator_and_ext(subs, ext);

                    check_derivable_ext_var(subs, ext, |ext| {
                        matches!(ext, Content::Structure(FlatType::EmptyTuple))
                    })?;

                    Ok(Key(FlatDecodableKey::Tuple(elems_iter.count() as _)))
                }
                FlatType::TagUnion(_tags, _ext) | FlatType::RecursiveTagUnion(_, _tags, _ext) => {
                    Err(Underivable) // yet
                }
                FlatType::FunctionOrTagUnion(_name_index, _, _) => {
                    Err(Underivable) // yet
                }
                FlatType::EmptyRecord => Ok(Key(FlatDecodableKey::Record(vec![]))),
                FlatType::EmptyTuple => Ok(Key(FlatDecodableKey::Tuple(0))),
                FlatType::EmptyTagUnion => {
                    Err(Underivable) // yet
                }
                //
                FlatType::Func(..) | FlatType::EffectfulFunc => Err(Underivable),
            },
            Content::Alias(sym, _, real_var, _) => match from_builtin_symbol(sym) {
                Some(lambda) => lambda,
                // NB: I believe it is okay to unwrap opaques here because derivers are only used
                // by the backend, and the backend treats opaques like structural aliases.
                None => Self::from_var(subs, real_var),
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
            Content::Pure | Content::Effectful => Err(Underivable),
        }
    }

    pub(crate) fn from_builtin_symbol(symbol: Symbol) -> Result<FlatDecodable, DeriveError> {
        from_builtin_symbol(symbol).unwrap_or(Err(DeriveError::Underivable))
    }
}

const fn from_builtin_symbol(symbol: Symbol) -> Option<Result<FlatDecodable, DeriveError>> {
    use FlatDecodable::*;
    match symbol {
        Symbol::BOOL_BOOL => Some(Ok(Immediate(Symbol::DECODE_BOOL))),
        Symbol::NUM_U8 | Symbol::NUM_UNSIGNED8 => Some(Ok(Immediate(Symbol::DECODE_U8))),
        Symbol::NUM_U16 | Symbol::NUM_UNSIGNED16 => Some(Ok(Immediate(Symbol::DECODE_U16))),
        Symbol::NUM_U32 | Symbol::NUM_UNSIGNED32 => Some(Ok(Immediate(Symbol::DECODE_U32))),
        Symbol::NUM_U64 | Symbol::NUM_UNSIGNED64 => Some(Ok(Immediate(Symbol::DECODE_U64))),
        Symbol::NUM_U128 | Symbol::NUM_UNSIGNED128 => Some(Ok(Immediate(Symbol::DECODE_U128))),
        Symbol::NUM_I8 | Symbol::NUM_SIGNED8 => Some(Ok(Immediate(Symbol::DECODE_I8))),
        Symbol::NUM_I16 | Symbol::NUM_SIGNED16 => Some(Ok(Immediate(Symbol::DECODE_I16))),
        Symbol::NUM_I32 | Symbol::NUM_SIGNED32 => Some(Ok(Immediate(Symbol::DECODE_I32))),
        Symbol::NUM_I64 | Symbol::NUM_SIGNED64 => Some(Ok(Immediate(Symbol::DECODE_I64))),
        Symbol::NUM_I128 | Symbol::NUM_SIGNED128 => Some(Ok(Immediate(Symbol::DECODE_I128))),
        Symbol::NUM_DEC | Symbol::NUM_DECIMAL => Some(Ok(Immediate(Symbol::DECODE_DEC))),
        Symbol::NUM_F32 | Symbol::NUM_BINARY32 => Some(Ok(Immediate(Symbol::DECODE_F32))),
        Symbol::NUM_F64 | Symbol::NUM_BINARY64 => Some(Ok(Immediate(Symbol::DECODE_F64))),
        _ => None,
    }
}
