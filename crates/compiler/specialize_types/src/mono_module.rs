use bumpalo::Bump;
use roc_solve::module::Solved;
use roc_types::subs::Subs;

use crate::{foreign_symbol::ForeignSymbols, mono_type::MonoTypes, DebugInfo};

pub struct MonoModule {
    mono_types: MonoTypes,
    foreign_symbols: ForeignSymbols,
    // TODO [mono2]: interner type
    interned_strings: Vec<String>,
    debug_info: DebugInfo,
}

impl MonoModule {
    pub fn from_typed_can_module(_subs: &Solved<Subs>) -> Self {
        Self {
            mono_types: MonoTypes::new(),
            foreign_symbols: ForeignSymbols::new(),
            interned_strings: Vec::new(),
            debug_info: DebugInfo,
        }
    }
}

/// TODO move this to its own crate
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct InternedStrId(u32);

/// TODO move this to its own crate
#[derive(Debug, Default)]
pub struct Interns<'a> {
    interned: Vec<&'a str>,
}

impl<'a> Interns<'a> {
    pub fn new() -> Self {
        Self {
            interned: Vec::new(),
        }
    }

    pub fn get_str(&self, _arena: &'a Bump, id: InternedStrId) -> &'a str {
        let index = id.0 as usize;

        #[cfg(debug_assertions)]
        {
            assert!(self.interned.get(index).is_some(), "Got an InternedStringId ({index}) that was outside the bounds of the backing array. This should never happen!");
        }

        // Safety: We should only ever give out InternedStrId values that are in this range.
        unsafe { self.interned.get_unchecked(index) }
    }

    pub fn get_id(&mut self, _arena: &'a Bump, string: &'a str) -> InternedStrId {
        match self
            .interned
            .iter()
            .position(|&interned| interned == string)
        {
            Some(index) => InternedStrId(index as u32),
            None => {
                let answer = InternedStrId(self.interned.len() as u32);

                self.interned.push(string);

                answer
            }
        }
    }

    pub fn try_get_id(&self, _arena: &'a Bump, string: &'a str) -> Option<InternedStrId> {
        self.interned
            .iter()
            .position(|&interned| interned == string)
            .map(|index| InternedStrId(index as u32))
    }
}
