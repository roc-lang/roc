#![warn(clippy::dbg_macro)]
// See github.com/rtfeldman/roc/issues/800 for discussion of the large_enum_variant check.
#![allow(clippy::large_enum_variant)]

#[derive(Debug, Clone, Copy)]
pub struct TargetInfo {
    pub architecture: Architecture,
}

impl TargetInfo {
    pub const fn ptr_width(&self) -> PtrWidth {
        self.architecture.ptr_width()
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

#[repr(u8)]
#[derive(Debug, Clone, Copy)]
pub enum PtrWidth {
    Bytes4 = 4,
    Bytes8 = 8,
}

#[derive(Debug, Clone, Copy)]
pub enum Architecture {
    X86_64,
    X86_32,
    Aarch64,
    Arm,
    Wasm32,
}

impl Architecture {
    pub const fn ptr_width(&self) -> PtrWidth {
        use Architecture::*;

        match self {
            X86_64 | Aarch64 | Arm => PtrWidth::Bytes8,
            X86_32 | Wasm32 => PtrWidth::Bytes4,
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
