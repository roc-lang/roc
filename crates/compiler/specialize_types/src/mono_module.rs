use roc_types::subs::Subs;

use crate::{foreign_symbol::ForeignSymbols, mono_type::MonoTypes, DebugInfo};

pub struct MonoModule {
    mono_types: MonoTypes,
    foreign_symbols: ForeignSymbols,
    interned_strings: Vec<String>,
    debug_info: DebugInfo,
}

impl MonoModule {
    pub fn from_typed_can_module(subs: &Subs) -> Self {
        Self {
            mono_types: MonoTypes::new(),
            foreign_symbols: ForeignSymbols::new(),
            interned_strings: Vec::new(),
            debug_info: DebugInfo,
        }
    }
}

#[derive(Clone, Copy, Debug)]
pub struct InternedStrId(u32);
