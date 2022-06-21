//! To avoid duplicating derived implementations for the same type, derived implementations are
//! addressed by a key of their type content. However, different derived implementations can be
//! reused based on different properties of the type. For example:
//!
//! - `Eq` does not care about surface type representations; its derived implementations can be
//!   uniquely addressed by the [`Layout`][crate::layout::Layout] of a type.
//! - `Encoding` must care about surface type representations; for example, `{ a: "" }` and
//!   `{ b: "" }` have different derived implementations. However, it does not need to distinguish
//!   between e.g. required and optional record fields.
//! - `Decoding` is like encoding, but has some differences. For one, it *does* need to distinguish
//!   between required and optional record fields.
//!
//! For these reasons the content keying is based on a strategy as well, which are the variants of
//! [`DeriveKey`].

pub mod encoding;

use std::sync::{Arc, RwLock};

use encoding::{FlatEncodable, FlatEncodableKey};

use roc_collections::MutMap;
use roc_module::symbol::{IdentIds, ModuleId, Symbol};
use roc_types::subs::{Subs, Variable};

#[derive(Hash, PartialEq, Eq, Debug)]
#[repr(u8)]
pub enum DeriveKey {
    Encoding(FlatEncodableKey),
    #[allow(unused)]
    Decoding,
}

#[derive(Hash, PartialEq, Eq, Debug)]
pub enum Derived {
    /// If a derived implementation name is well-known ahead-of-time, we can inline the symbol
    /// directly rather than associating a key for an implementation to be made later on.
    Immediate(Symbol),
    /// Key of the derived implementation to use. This allows association of derived implementation
    /// names to a key, when the key is known ahead-of-time but the implementation (and it's name)
    /// is yet-to-be-made.
    Key(DeriveKey),
}

impl Derived {
    pub fn encoding(subs: &Subs, var: Variable) -> Self {
        match encoding::FlatEncodable::from_var(subs, var) {
            FlatEncodable::Immediate(imm) => Derived::Immediate(imm),
            FlatEncodable::Key(repr) => Derived::Key(DeriveKey::Encoding(repr)),
        }
    }
}

/// Map of [`DeriveKey`]s to their derived symbols.
#[derive(Debug, Default)]
pub struct DerivedMethods {
    map: MutMap<DeriveKey, Symbol>,
    derived_ident_ids: IdentIds,
}

impl DerivedMethods {
    pub fn get_or_insert(&mut self, key: DeriveKey) -> Symbol {
        let symbol = self.map.entry(key).or_insert_with(|| {
            let ident_id = self.derived_ident_ids.gen_unique();

            Symbol::new(ModuleId::DERIVED, ident_id)
        });
        *symbol
    }
}

/// Thread-sharable [`DerivedMethods`].
pub type GlobalDerivedMethods = Arc<RwLock<DerivedMethods>>;
