//! Derivers for the `Hash` ability.

use roc_derive_key::hash::FlatHashKey;
use roc_module::symbol::Symbol;

use crate::{util::Env, DerivedBody};

pub(crate) fn derive_hash(
    _env: &mut Env<'_>,
    key: FlatHashKey,
    _def_symbol: Symbol,
) -> DerivedBody {
    match key {
        FlatHashKey::Record(_) => todo!(),
    }
}
