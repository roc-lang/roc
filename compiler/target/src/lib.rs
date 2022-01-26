#![warn(clippy::dbg_macro)]
// See github.com/rtfeldman/roc/issues/800 for discussion of the large_enum_variant check.
#![allow(clippy::large_enum_variant)]

pub struct TargetInfo {
    architecture: Architecture,
}

pub enum Architecture {
    X86_64,
    X86_32,
    Aarch64,
    Arm,
    Wasm32,
}

impl Architecture {
    pub const fn ptr_width(&self) -> u32 {
        use Architecture::*;

        match self {
            X86_64 | Aarch64 | Arm => 8,
            X86_32 | Wasm32 => 4,
        }
    }
}

impl From<target_lexicon::Architecture> for Architecture {
    fn from(target: target_lexicon::Architecture) -> Self {
        match target {
            target_lexicon::Architecture::X86_64 => Architecture::X86_64,
            target_lexicon::Architecture::X86_32(_) => Architecture::X86_32,
            target_lexicon::Architecture::Aarch64(_) => Architecture::Aarch64,
            target_lexicon::Architecture::Arm(_) => Architecture::Arm,
            target_lexicon::Architecture::Wasm32 => Architecture::Wasm32,
            _ => unreachable!("unsupported architecture"),
        }
    }
}
