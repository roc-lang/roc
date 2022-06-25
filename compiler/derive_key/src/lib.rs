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

        ident_ids
    }
}

/// Thread-sharable [`DerivedMethods`].
pub type GlobalDerivedSymbols = Arc<Mutex<DerivedSymbols>>;
