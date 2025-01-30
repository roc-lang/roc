use roc_module::{
    ident::{Lowercase, TagName},
    symbol::Symbol,
};
use roc_types::{
    subs::{Content, FlatType, GetSubsSlice, Subs, Variable},
    types::AliasKind,
};

use crate::util::{
    check_derivable_ext_var, debug_name_fn, debug_name_record, debug_name_tag, debug_name_tuple,
};

#[derive(Hash, Debug)]
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
    /// This means specifically an opaque type where the author hasn't requested that it derive Inspect (or implemented it)
    Opaque,
    Error,
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
            FlatInspectableKey::Error => "error".to_string(),
            FlatInspectableKey::Opaque => "opaque".to_string(),
        }
    }
}

impl FlatInspectable {
    pub(crate) fn from_var(subs: &Subs, var: Variable) -> FlatInspectable {
        use FlatInspectable::*;

        match *subs.get_content_without_compacting(var) {
            Content::Structure(flat_type) => match flat_type {
                FlatType::Apply(sym, _) => match sym {
                    Symbol::LIST_LIST => Key(FlatInspectableKey::List()),
                    Symbol::SET_SET => Key(FlatInspectableKey::Set()),
                    Symbol::DICT_DICT => Key(FlatInspectableKey::Dict()),
                    Symbol::STR_STR => Immediate(Symbol::INSPECT_STR),
                    _ => Immediate(Symbol::INSPECT_OPAQUE),
                },
                FlatType::Record(fields, ext) => {
                    let (fields_iter, ext) = fields.unsorted_iterator_and_ext(subs, ext);

                    // TODO someday we can put #[cfg(debug_assertions)] around this, but for now let's always do it.
                    check_derivable_ext_var(subs, ext, |ext| {
                        matches!(ext, Content::Structure(FlatType::EmptyRecord))
                    }).expect("Compiler error: unexpected nonempty ext var when deriving Inspect for record");

                    let mut field_names = Vec::with_capacity(fields.len());
                    for (field_name, _) in fields_iter {
                        field_names.push(field_name.clone());
                    }

                    field_names.sort();

                    Key(FlatInspectableKey::Record(field_names))
                }
                FlatType::Tuple(elems, ext) => {
                    let (elems_iter, ext) = elems.sorted_iterator_and_ext(subs, ext);

                    // TODO someday we can put #[cfg(debug_assertions)] around this, but for now let's always do it.
                    check_derivable_ext_var(subs, ext, |_| {
                        false
                    }).expect("Compiler error: unexpected nonempty ext var when deriving Inspect for tuple");

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

                    // TODO someday we can put #[cfg(debug_assertions)] around this, but for now let's always do it.
                    check_derivable_ext_var(subs, ext.var(), |ext| {
                        matches!(ext, Content::Structure(FlatType::EmptyTagUnion))
                    }).expect("Compiler error: unexpected nonempty ext var when deriving Inspect for tag union");

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
                FlatType::EmptyTagUnion => Key(FlatInspectableKey::TagUnion(Vec::new())),
                FlatType::Func(..) => Immediate(Symbol::INSPECT_FUNCTION),
                FlatType::EffectfulFunc => {
                    unreachable!("There must have been a bug in the solver, because we're trying to derive Inspect on a non-concrete type.");
                }
            },
            Content::Alias(sym, _, real_var, kind) => match Self::from_builtin_alias(sym) {
                Some(lambda) => lambda,

                _ => {
                    match kind {
                        AliasKind::Structural => Self::from_var(subs, real_var),
                        // Special case, an unbound `Frac *` will become a `Dec`.
                        AliasKind::Opaque
                            if matches!(
                                *subs.get_content_without_compacting(real_var),
                                Content::FlexVar(_) | Content::FlexAbleVar(_, _)
                            ) =>
                        {
                            Immediate(Symbol::INSPECT_DEC)
                        }
                        AliasKind::Opaque if sym.is_builtin() => Self::from_var(subs, real_var),
                        AliasKind::Opaque => {
                            // There are two cases in which `Inspect` can be derived for an opaque
                            // type.
                            //   1. An opaque type claims to implement `Inspect` and asks us to
                            //      auto-derive it. E.g.
                            //
                            //      ```text
                            //      Op := {} implements [Inspect]
                            //      ```
                            //
                            //      In this case, we generate a synthetic implementation during
                            //      canonicalization that defers to `inspect`ing the inner type. As
                            //      such, this case is never reached in this branch.
                            //
                            //   2. An opaque type does not explicitly claim to implement
                            //      `Inspect`. In this case, we print a default opaque string for
                            //      the opaque type.
                            Immediate(Symbol::INSPECT_OPAQUE)
                        }
                    }
                }
            },
            Content::RangedNumber(range) => {
                Self::from_var(subs, range.default_compilation_variable())
            }
            Content::RecursionVar { structure, .. } => Self::from_var(subs, structure),
            Content::Error => Key(FlatInspectableKey::Error),
            Content::FlexVar(_)
            | Content::RigidVar(_)
            | Content::FlexAbleVar(_, _)
            | Content::RigidAbleVar(_, _)
            | Content::LambdaSet(_)
            | Content::ErasedLambda
            | Content::Pure
            | Content::Effectful => {
                unreachable!("There must have been a bug in the solver, because we're trying to derive Inspect on a non-concrete type.");
            }
        }
    }

    pub(crate) const fn from_builtin_alias(symbol: Symbol) -> Option<FlatInspectable> {
        use FlatInspectable::*;

        match symbol {
            Symbol::BOOL_BOOL => Some(Immediate(Symbol::INSPECT_BOOL)),
            Symbol::NUM_U8 | Symbol::NUM_UNSIGNED8 => Some(Immediate(Symbol::INSPECT_U8)),
            Symbol::NUM_U16 | Symbol::NUM_UNSIGNED16 => Some(Immediate(Symbol::INSPECT_U16)),
            Symbol::NUM_U32 | Symbol::NUM_UNSIGNED32 => Some(Immediate(Symbol::INSPECT_U32)),
            Symbol::NUM_U64 | Symbol::NUM_UNSIGNED64 => Some(Immediate(Symbol::INSPECT_U64)),
            Symbol::NUM_U128 | Symbol::NUM_UNSIGNED128 => Some(Immediate(Symbol::INSPECT_U128)),
            Symbol::NUM_I8 | Symbol::NUM_SIGNED8 => Some(Immediate(Symbol::INSPECT_I8)),
            Symbol::NUM_I16 | Symbol::NUM_SIGNED16 => Some(Immediate(Symbol::INSPECT_I16)),
            Symbol::NUM_I32 | Symbol::NUM_SIGNED32 => Some(Immediate(Symbol::INSPECT_I32)),
            Symbol::NUM_I64 | Symbol::NUM_SIGNED64 => Some(Immediate(Symbol::INSPECT_I64)),
            Symbol::NUM_I128 | Symbol::NUM_SIGNED128 => Some(Immediate(Symbol::INSPECT_I128)),
            Symbol::NUM_DEC | Symbol::NUM_DECIMAL => Some(Immediate(Symbol::INSPECT_DEC)),
            Symbol::NUM_F32 | Symbol::NUM_BINARY32 => Some(Immediate(Symbol::INSPECT_F32)),
            Symbol::NUM_F64 | Symbol::NUM_BINARY64 => Some(Immediate(Symbol::INSPECT_F64)),
            _ => None,
        }
    }
}
