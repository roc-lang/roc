#![warn(clippy::dbg_macro)]
// See github.com/rtfeldman/roc/issues/800 for discussion of the large_enum_variant check.
#![allow(clippy::large_enum_variant)]

use strum_macros::{EnumCount, EnumIter};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TargetInfo {
    pub architecture: Architecture,
}

impl TargetInfo {
    pub const fn ptr_width(&self) -> PtrWidth {
        self.architecture.ptr_width()
    }

    pub const fn ptr_size(&self) -> usize {
        match self.ptr_width() {
            PtrWidth::Bytes4 => 4,
            PtrWidth::Bytes8 => 8,
        }
    }

    pub const fn ptr_alignment_bytes(&self) -> usize {
        self.architecture.ptr_alignment_bytes()
    }

    pub const fn default_aarch64() -> Self {
        TargetInfo {
            architecture: Architecture::Aarch64,
        }
    }

    pub const fn default_x86_64() -> Self {
        TargetInfo {
            architecture: Architecture::X86_64,
        }
    }

    pub const fn default_wasm32() -> Self {
        TargetInfo {
            architecture: Architecture::Wasm32,
        }
    }
}

impl From<&target_lexicon::Triple> for TargetInfo {
    fn from(triple: &target_lexicon::Triple) -> Self {
        let architecture = Architecture::from(triple.architecture);

        Self { architecture }
    }
}

impl From<Architecture> for TargetInfo {
    fn from(architecture: Architecture) -> Self {
        Self { architecture }
    }
}

#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum PtrWidth {
    Bytes4 = 4,
    Bytes8 = 8,
}

/// These should be sorted alphabetically!
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, EnumIter, EnumCount)]
#[repr(u8)]
pub enum Architecture {
    Aarch32,
    Aarch64,
    Wasm32,
    X86_32,
    X86_64,
    Windows64,
}

impl Architecture {
    pub const fn ptr_width(&self) -> PtrWidth {
        use Architecture::*;

        match self {
            X86_64 | Aarch64 | Windows64 => PtrWidth::Bytes8,
            X86_32 | Aarch32 | Wasm32 => PtrWidth::Bytes4,
        }
    }

    pub const fn ptr_alignment_bytes(&self) -> usize {
        self.ptr_width() as usize
    }
}

impl From<target_lexicon::Architecture> for Architecture {
    fn from(target: target_lexicon::Architecture) -> Self {
        match target {
            target_lexicon::Architecture::X86_64 => Architecture::X86_64,
            target_lexicon::Architecture::X86_32(_) => Architecture::X86_32,
            target_lexicon::Architecture::Aarch64(_) => Architecture::Aarch64,
            target_lexicon::Architecture::Arm(_) => Architecture::Aarch32,
            target_lexicon::Architecture::Wasm32 => Architecture::Wasm32,
            _ => unreachable!("unsupported architecture"),
        }
    }
}
