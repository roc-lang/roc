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
//! For these reasons the content keying is based on a [`Strategy`] as well.

pub mod encoding;

use encoding::FlatEncodable;

use roc_types::subs::{Subs, Variable};

#[derive(Hash)]
#[repr(u8)]
enum Strategy {
    Encoding,
    #[allow(unused)]
    Decoding,
}

#[derive(Hash)]
pub struct DeriveKey<R>
where
    R: std::hash::Hash,
{
    strategy: Strategy,
    pub repr: R,
}

impl<'a> DeriveKey<FlatEncodable<'a>> {
    pub fn encoding(subs: &'a Subs, var: Variable) -> Self {
        DeriveKey {
            strategy: Strategy::Encoding,
            repr: encoding::FlatEncodable::from_var(subs, var),
        }
    }
}
