use roc_module::symbol::Symbol;
use roc_types::subs::{Content, FlatType, Subs, Variable};

use crate::DeriveError;

#[derive(Hash)]
pub enum FlatHash {
    // `hash` is always of form `hasher, a -> hasher` where `hasher` and `a` are opaque, so all
    // immediates must have exactly one lambda set!
    SingleLambdaSetImmediate(Symbol),
    Key(FlatHashKey),
}

#[derive(Hash, PartialEq, Eq, Debug, Clone)]
pub enum FlatHashKey {}

impl FlatHashKey {
    pub(crate) fn debug_name(&self) -> String {
        unreachable!() // yet
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
                FlatType::Record(_fields, _ext) => {
                    Err(Underivable) // yet
                }
                FlatType::TagUnion(_tags, _ext) | FlatType::RecursiveTagUnion(_, _tags, _ext) => {
                    Err(Underivable) // yet
                }
                FlatType::FunctionOrTagUnion(_name_index, _, _) => {
                    Err(Underivable) // yet
                }
                FlatType::EmptyRecord => Err(Underivable), // yet
                FlatType::EmptyTagUnion => {
                    Err(Underivable) // yet
                }
                //
                FlatType::Erroneous(_) => Err(Underivable),
                FlatType::Func(..) => Err(Underivable),
            },
            Content::Alias(sym, _, real_var, _) => match sym {
                Symbol::NUM_U8 | Symbol::NUM_UNSIGNED8 => {
                    Ok(SingleLambdaSetImmediate(Symbol::HASH_ADD_U8))
                }
                Symbol::NUM_U16 | Symbol::NUM_UNSIGNED16 => {
                    Ok(SingleLambdaSetImmediate(Symbol::HASH_ADD_U16))
                }
                Symbol::NUM_U32 | Symbol::NUM_UNSIGNED32 => {
                    Ok(SingleLambdaSetImmediate(Symbol::HASH_ADD_U32))
                }
                Symbol::NUM_U64 | Symbol::NUM_UNSIGNED64 => {
                    Ok(SingleLambdaSetImmediate(Symbol::HASH_ADD_U64))
                }
                Symbol::NUM_U128 | Symbol::NUM_UNSIGNED128 => {
                    Ok(SingleLambdaSetImmediate(Symbol::HASH_ADD_U128))
                }
                Symbol::NUM_I8 | Symbol::NUM_SIGNED8 => {
                    Ok(SingleLambdaSetImmediate(Symbol::HASH_ADD_I8))
                }
                Symbol::NUM_I16 | Symbol::NUM_SIGNED16 => {
                    Ok(SingleLambdaSetImmediate(Symbol::HASH_ADD_I16))
                }
                Symbol::NUM_I32 | Symbol::NUM_SIGNED32 => {
                    Ok(SingleLambdaSetImmediate(Symbol::HASH_ADD_I32))
                }
                Symbol::NUM_I64 | Symbol::NUM_SIGNED64 => {
                    Ok(SingleLambdaSetImmediate(Symbol::HASH_ADD_I64))
                }
                Symbol::NUM_I128 | Symbol::NUM_SIGNED128 => {
                    Ok(SingleLambdaSetImmediate(Symbol::HASH_ADD_I128))
                }
                // NB: I believe it is okay to unwrap opaques here because derivers are only used
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
