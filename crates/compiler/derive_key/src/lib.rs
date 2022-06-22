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

use std::sync::{Arc, Mutex};

use encoding::{FlatEncodable, FlatEncodableKey};

use roc_collections::MutMap;
use roc_module::symbol::{IdentIds, ModuleId, Symbol};
use roc_types::subs::{Subs, Variable};

#[derive(Debug, PartialEq)]
pub enum DeriveError {
    /// Unbound variable present in the type-to-derive. It may be possible to derive for this type
    /// once the unbound variable is resolved.
    UnboundVar,
    /// The type is underivable for the given ability member.
    Underivable,
}

#[derive(Hash, PartialEq, Eq, Debug, Clone)]
#[repr(u8)]
pub enum DeriveKey {
    ToEncoder(FlatEncodableKey),
    #[allow(unused)]
    Decoding,
}

impl DeriveKey {
    pub fn debug_name(&self) -> String {
        match self {
            DeriveKey::ToEncoder(key) => format!("toEncoder_{}", key.debug_name()),
            DeriveKey::Decoding => todo!(),
        }
    }
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
    pub fn encoding(subs: &Subs, var: Variable) -> Result<Self, DeriveError> {
        match encoding::FlatEncodable::from_var(subs, var)? {
            FlatEncodable::Immediate(imm) => Ok(Derived::Immediate(imm)),
            FlatEncodable::Key(repr) => Ok(Derived::Key(DeriveKey::ToEncoder(repr))),
        }
    }
}

/// Map of [`DeriveKey`]s to their derived symbols.
#[derive(Debug, Default)]
pub struct DerivedSymbols {
    map: MutMap<DeriveKey, Symbol>,
    derived_ident_ids: IdentIds,
    #[cfg(debug_assertions)]
    stolen: bool,
}

impl DerivedSymbols {
    pub fn get_or_insert(&mut self, key: DeriveKey) -> Symbol {
        #[cfg(debug_assertions)]
        {
            debug_assert!(!self.stolen, "attempting to add to stolen symbols!");
        }

        let symbol = self.map.entry(key).or_insert_with_key(|key| {
            let ident_id = if cfg!(debug_assertions) || cfg!(feature = "debug-derived-symbols") {
                let debug_name = key.debug_name();
                debug_assert!(
                    self.derived_ident_ids.get_id(&debug_name).is_none(),
                    "duplicate debug name for different derive key"
                );
                let ident_id = self.derived_ident_ids.get_or_insert(&debug_name);

                // This is expensive, but yields much better symbols when debugging.
                // TODO: hide behind debug_flags?
                ModuleId::DERIVED.register_debug_idents(&self.derived_ident_ids);

                ident_id
            } else {
                self.derived_ident_ids.gen_unique()
            };

            Symbol::new(ModuleId::DERIVED, ident_id)
        });
        *symbol
    }

    pub fn iter_all(&self) -> impl Iterator<Item = (&DeriveKey, &Symbol)> {
        self.map.iter()
    }

    /// Generate a unique symbol. This should only be used when generating code inside the Derived
    /// module; other modules should use [`Self::get_or_insert`] to generate a symbol for a derived
    /// ability member usage.
    pub fn gen_unique(&mut self) -> Symbol {
        let ident_id = self.derived_ident_ids.gen_unique();
        Symbol::new(ModuleId::DERIVED, ident_id)
    }

    /// Steal all created derived ident Ids.
    /// After this is called, [`Self::get_or_insert`] may no longer be called.
    pub fn steal(&mut self) -> IdentIds {
        let mut ident_ids = Default::default();
        std::mem::swap(&mut self.derived_ident_ids, &mut ident_ids);

        #[cfg(debug_assertions)]
        {
            debug_assert!(!self.stolen);
            self.stolen = true;
        }

        ident_ids
    }

    pub fn return_ident_ids(&mut self, ident_ids: IdentIds) {
        #[cfg(debug_assertions)]
        {
            debug_assert!(self.stolen);
            self.stolen = false;
        }

        self.derived_ident_ids = ident_ids;
    }
}

/// Thread-sharable [`DerivedMethods`].
pub type GlobalDerivedSymbols = Arc<Mutex<DerivedSymbols>>;
