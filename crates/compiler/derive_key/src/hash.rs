use roc_module::{
    ident::{Lowercase, TagName},
    symbol::Symbol,
};
use roc_types::subs::{Content, FlatType, GetSubsSlice, Subs, Variable};

use crate::{
    util::{check_derivable_ext_var, debug_name_record, debug_name_tag, debug_name_tuple},
    DeriveError,
};

#[derive(Hash)]
pub enum FlatHash {
    // `hash` is always of form `hasher, a -> hasher` where `hasher` and `a` are opaque, so all
    // immediates must have exactly one lambda set!
    SingleLambdaSetImmediate(Symbol),
    Key(FlatHashKey),
}

#[derive(Hash, PartialEq, Eq, Debug, Clone)]
pub enum FlatHashKey {
    // Unfortunate that we must allocate here, c'est la vie
    Record(Vec<Lowercase>),
    Tuple(u32),
    TagUnion(Vec<(TagName, u16)>),
}

impl FlatHashKey {
    pub(crate) fn debug_name(&self) -> String {
        match self {
            FlatHashKey::Record(fields) => debug_name_record(fields),
            FlatHashKey::Tuple(arity) => debug_name_tuple(*arity),
            FlatHashKey::TagUnion(tags) => debug_name_tag(tags),
        }
    }
}

impl FlatHash {
    pub(crate) fn from_var(subs: &Subs, var: Variable) -> Result<FlatHash, DeriveError> {
        use DeriveError::*;
        use FlatHash::*;
        match *subs.get_content_without_compacting(var) {
            Content::Structure(flat_type) => match flat_type {
                FlatType::Apply(sym, _) => match sym {
                    Symbol::LIST_LIST => Ok(SingleLambdaSetImmediate(Symbol::HASH_HASH_LIST)),
                    Symbol::STR_STR => Ok(SingleLambdaSetImmediate(Symbol::HASH_HASH_STR_BYTES)),
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

                    Ok(Key(FlatHashKey::Record(field_names)))
                }
                FlatType::Tuple(elems, ext) => {
                    let (elems_iter, ext) = elems.sorted_iterator_and_ext(subs, ext);

                    check_derivable_ext_var(subs, ext, |ext| {
                        matches!(ext, Content::Structure(FlatType::EmptyTuple))
                    })?;

                    Ok(Key(FlatHashKey::Tuple(elems_iter.count() as _)))
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

                    Ok(Key(FlatHashKey::TagUnion(tag_names_and_payload_sizes)))
                }
                FlatType::FunctionOrTagUnion(names_index, _, _) => Ok(Key(FlatHashKey::TagUnion(
                    subs.get_subs_slice(names_index)
                        .iter()
                        .map(|t| (t.clone(), 0))
                        .collect(),
                ))),
                FlatType::EmptyRecord => Ok(Key(FlatHashKey::Record(vec![]))),
                FlatType::EmptyTuple => Ok(Key(FlatHashKey::Tuple(0))),
                FlatType::EmptyTagUnion => Ok(Key(FlatHashKey::TagUnion(vec![]))),

                FlatType::Func(..) | FlatType::EffectfulFunc => Err(Underivable),
            },
            Content::Alias(sym, _, real_var, _) => match builtin_symbol_to_hash_lambda(sym) {
                Some(lambda) => Ok(lambda),
                // NB: I believe it is okay to unwrap opaques here because derivers are only used
                // by the backend, and the backend treats opaques like structural aliases.
                None => Self::from_var(subs, real_var),
            },
            Content::RangedNumber(range) => {
                // Find the integer we're going to compile to, that'll tell us what lambda we
                // should resolve to.
                //
                // Note that at this point, we don't need to update the underlying type variable.
                // That's because
                //
                //   - If the type variable always had a ground constructor after solving, we would
                //     have already refined the ranged number during obligation checking.
                //
                //   - If the type variable was generalized, then this branch is only reached
                //     during monomorphization, at which point we always choose a default layout
                //     for ranged numbers, without concern for reification to a ground type.
                let chosen_width = range.default_compilation_width();
                let lambda = builtin_symbol_to_hash_lambda(chosen_width.symbol()).unwrap();
                Ok(lambda)
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

    pub fn from_builtin_symbol(symbol: Symbol) -> Result<FlatHash, DeriveError> {
        builtin_symbol_to_hash_lambda(symbol).ok_or(DeriveError::Underivable)
    }
}

const fn builtin_symbol_to_hash_lambda(symbol: Symbol) -> Option<FlatHash> {
    use FlatHash::*;
    match symbol {
        Symbol::BOOL_BOOL => Some(SingleLambdaSetImmediate(Symbol::HASH_HASH_BOOL)),
        Symbol::NUM_U8 | Symbol::NUM_UNSIGNED8 => {
            Some(SingleLambdaSetImmediate(Symbol::HASH_ADD_U8))
        }
        Symbol::NUM_U16 | Symbol::NUM_UNSIGNED16 => {
            Some(SingleLambdaSetImmediate(Symbol::HASH_ADD_U16))
        }
        Symbol::NUM_U32 | Symbol::NUM_UNSIGNED32 => {
            Some(SingleLambdaSetImmediate(Symbol::HASH_ADD_U32))
        }
        Symbol::NUM_U64 | Symbol::NUM_UNSIGNED64 => {
            Some(SingleLambdaSetImmediate(Symbol::HASH_ADD_U64))
        }
        Symbol::NUM_U128 | Symbol::NUM_UNSIGNED128 => {
            Some(SingleLambdaSetImmediate(Symbol::HASH_ADD_U128))
        }
        Symbol::NUM_I8 | Symbol::NUM_SIGNED8 => {
            Some(SingleLambdaSetImmediate(Symbol::HASH_HASH_I8))
        }
        Symbol::NUM_I16 | Symbol::NUM_SIGNED16 => {
            Some(SingleLambdaSetImmediate(Symbol::HASH_HASH_I16))
        }
        Symbol::NUM_I32 | Symbol::NUM_SIGNED32 => {
            Some(SingleLambdaSetImmediate(Symbol::HASH_HASH_I32))
        }
        Symbol::NUM_I64 | Symbol::NUM_SIGNED64 => {
            Some(SingleLambdaSetImmediate(Symbol::HASH_HASH_I64))
        }
        Symbol::NUM_I128 | Symbol::NUM_SIGNED128 => {
            Some(SingleLambdaSetImmediate(Symbol::HASH_HASH_I128))
        }
        Symbol::NUM_DEC | Symbol::NUM_DECIMAL => {
            Some(SingleLambdaSetImmediate(Symbol::HASH_HASH_DEC))
        }
        _ => None,
    }
}
