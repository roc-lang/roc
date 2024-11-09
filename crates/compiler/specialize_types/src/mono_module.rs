use bumpalo::Bump;
use roc_solve::module::Solved;
use roc_types::subs::Subs;

use crate::{foreign_symbol::ForeignSymbols, mono_type::MonoTypes, DebugInfo};

pub struct MonoModule {
    mono_types: MonoTypes,
    foreign_symbols: ForeignSymbols,
    interned_strings: Vec<String>,
    debug_info: DebugInfo,
}

impl MonoModule {
    pub fn from_typed_can_module(subs: &Solved<Subs>) -> Self {
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
#[derive(Debug)]
pub struct Interns<'a> {
    interned: Vec<&'a str>,
}

impl<'a> Interns<'a> {
    pub fn new() -> Self {
        Self {
            interned: Vec::new(),
        }
    }

    pub fn get(&mut self, _arena: &'a Bump, string: &'a str) -> InternedStrId {
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

    pub fn try_get(&self, _arena: &'a Bump, string: &'a str) -> Option<InternedStrId> {
        match self
            .interned
            .iter()
            .position(|&interned| interned == string)
        {
            Some(index) => Some(InternedStrId(index as u32)),
            None => None,
        }
    }
}
