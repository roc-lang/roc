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
//!   between required and default value record fields.
//!
//! For these reasons the content keying is based on a strategy as well, which are the variants of
//! [`DeriveKey`].

pub mod decoding;
pub mod encoding;
pub mod hash;
pub mod inspect;
mod util;

use decoding::{FlatDecodable, FlatDecodableKey};
use encoding::{FlatEncodable, FlatEncodableKey};
use hash::{FlatHash, FlatHashKey};

use inspect::{FlatInspectable, FlatInspectableKey};
use roc_module::symbol::Symbol;
use roc_types::subs::{Subs, Variable};

#[derive(Debug, PartialEq, Eq)]
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
    Decoder(FlatDecodableKey),
    Hash(FlatHashKey),
    ToInspector(FlatInspectableKey),
}

impl DeriveKey {
    pub fn debug_name(&self) -> String {
        match self {
            DeriveKey::ToEncoder(key) => format!("to_encoder_{}", key.debug_name()),
            DeriveKey::Decoder(key) => format!("decoder_{}", key.debug_name()),
            DeriveKey::Hash(key) => format!("hash_{}", key.debug_name()),
            DeriveKey::ToInspector(key) => format!("to_inspector_{}", key.debug_name()),
        }
    }
}

#[derive(Hash, Clone, PartialEq, Eq, Debug)]
pub enum Derived {
    /// If a derived implementation name is well-known ahead-of-time, we can inline the symbol
    /// directly rather than associating a key for an implementation to be made later on.
    ///
    /// Immediates refer to ability members that are "inlined" at the derivation call site.
    Immediate(Symbol),
    /// Like an [Derived::Immediate], but with the additional constraint that the immediate
    /// symbol is statically known to have exactly one lamdba set.
    /// This unlocks some optimization opportunities, as regioned lambda sets do not need to be
    /// chased.
    SingleLambdaSetImmediate(Symbol),
    /// Key of the derived implementation to use. This allows association of derived implementation
    /// names to a key, when the key is known ahead-of-time but the implementation (and it's name)
    /// is yet-to-be-made.
    Key(DeriveKey),
}

/// The builtin ability member to derive.
#[derive(Clone, Copy, Debug)]
pub enum DeriveBuiltin {
    ToEncoder,
    Decoder,
    Hash,
    IsEq,
    ToInspector,
}

impl TryFrom<Symbol> for DeriveBuiltin {
    type Error = Symbol;

    fn try_from(value: Symbol) -> Result<Self, Self::Error> {
        match value {
            Symbol::ENCODE_TO_ENCODER => Ok(DeriveBuiltin::ToEncoder),
            Symbol::DECODE_DECODER => Ok(DeriveBuiltin::Decoder),
            Symbol::HASH_HASH => Ok(DeriveBuiltin::Hash),
            Symbol::BOOL_IS_EQ => Ok(DeriveBuiltin::IsEq),
            Symbol::INSPECT_TO_INSPECTOR => Ok(DeriveBuiltin::ToInspector),
            _ => Err(value),
        }
    }
}

impl Derived {
    pub fn builtin(
        builtin: DeriveBuiltin,
        subs: &Subs,
        var: Variable,
    ) -> Result<Self, DeriveError> {
        match builtin {
            DeriveBuiltin::ToEncoder => match encoding::FlatEncodable::from_var(subs, var)? {
                FlatEncodable::Immediate(imm) => Ok(Derived::Immediate(imm)),
                FlatEncodable::Key(repr) => Ok(Derived::Key(DeriveKey::ToEncoder(repr))),
            },
            DeriveBuiltin::Decoder => match decoding::FlatDecodable::from_var(subs, var)? {
                FlatDecodable::Immediate(imm) => Ok(Derived::Immediate(imm)),
                FlatDecodable::Key(repr) => Ok(Derived::Key(DeriveKey::Decoder(repr))),
            },
            DeriveBuiltin::Hash => match hash::FlatHash::from_var(subs, var)? {
                FlatHash::SingleLambdaSetImmediate(imm) => {
                    Ok(Derived::SingleLambdaSetImmediate(imm))
                }
                FlatHash::Key(repr) => Ok(Derived::Key(DeriveKey::Hash(repr))),
            },
            DeriveBuiltin::IsEq => {
                // If obligation checking passes, we always lower derived implementations of `isEq`
                // to the `Eq` low-level, to be fulfilled by the backends.
                Ok(Derived::SingleLambdaSetImmediate(
                    Symbol::BOOL_STRUCTURAL_EQ,
                ))
            }
            DeriveBuiltin::ToInspector => match FlatInspectable::from_var(subs, var) {
                FlatInspectable::Immediate(imm) => Ok(Derived::Immediate(imm)),
                FlatInspectable::Key(repr) => Ok(Derived::Key(DeriveKey::ToInspector(repr))),
            },
        }
    }

    pub fn builtin_with_builtin_symbol(
        builtin: DeriveBuiltin,
        symbol: Symbol,
    ) -> Result<Self, DeriveError> {
        match builtin {
            DeriveBuiltin::ToEncoder => match encoding::FlatEncodable::from_builtin_symbol(symbol)?
            {
                FlatEncodable::Immediate(imm) => Ok(Derived::Immediate(imm)),
                FlatEncodable::Key(repr) => Ok(Derived::Key(DeriveKey::ToEncoder(repr))),
            },
            DeriveBuiltin::Decoder => match decoding::FlatDecodable::from_builtin_symbol(symbol)? {
                FlatDecodable::Immediate(imm) => Ok(Derived::Immediate(imm)),
                FlatDecodable::Key(repr) => Ok(Derived::Key(DeriveKey::Decoder(repr))),
            },
            DeriveBuiltin::Hash => match hash::FlatHash::from_builtin_symbol(symbol)? {
                FlatHash::SingleLambdaSetImmediate(imm) => {
                    Ok(Derived::SingleLambdaSetImmediate(imm))
                }
                FlatHash::Key(repr) => Ok(Derived::Key(DeriveKey::Hash(repr))),
            },
            DeriveBuiltin::IsEq => {
                // If obligation checking passes, we always lower derived implementations of `isEq`
                // to the `Eq` low-level, to be fulfilled by the backends.
                Ok(Derived::SingleLambdaSetImmediate(
                    Symbol::BOOL_STRUCTURAL_EQ,
                ))
            }
            DeriveBuiltin::ToInspector => {
                match inspect::FlatInspectable::from_builtin_alias(symbol).unwrap() {
                    FlatInspectable::Immediate(imm) => Ok(Derived::Immediate(imm)),
                    FlatInspectable::Key(repr) => Ok(Derived::Key(DeriveKey::ToInspector(repr))),
                }
            }
        }
    }
}
