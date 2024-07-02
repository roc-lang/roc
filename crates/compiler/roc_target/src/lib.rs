//! Provides types and helpers for compiler targets such as `default_x86_64`.
#![warn(clippy::dbg_macro)]
// See github.com/roc-lang/roc/issues/800 for discussion of the large_enum_variant check.
#![allow(clippy::large_enum_variant)]

use std::str::FromStr;

use roc_error_macros::user_error;
use strum_macros::{EnumCount, EnumIter};
use target_lexicon::Triple;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum OperatingSystem {
    Freestanding,
    Linux,
    Mac,
    Windows,
}

impl std::fmt::Display for OperatingSystem {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let arch_str = match self {
            OperatingSystem::Freestanding => "freestanding",
            OperatingSystem::Linux => "linux",
            OperatingSystem::Mac => "macos",
            OperatingSystem::Windows => "windows",
        };
        write!(f, "{}", arch_str)
    }
}

#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum PtrWidth {
    Bytes4 = 4,
    Bytes8 = 8,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, EnumIter, EnumCount)]
pub enum Architecture {
    Aarch32,
    Aarch64,
    Wasm32,
    X86_32,
    X86_64,
}

impl std::fmt::Display for Architecture {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let arch_str = match self {
            Architecture::Aarch32 => "arm",
            Architecture::Aarch64 => "arm64",
            Architecture::Wasm32 => "wasm32",
            Architecture::X86_32 => "x86_32",
            Architecture::X86_64 => "x86_64",
        };
        write!(f, "{}", arch_str)
    }
}

impl Architecture {
    pub const fn ptr_width(&self) -> PtrWidth {
        use Architecture::*;

        match self {
            X86_64 | Aarch64 => PtrWidth::Bytes8,
            X86_32 | Aarch32 | Wasm32 => PtrWidth::Bytes4,
        }
    }

    pub const fn ptr_alignment_bytes(&self) -> usize {
        self.ptr_width() as usize
    }
}

#[derive(Debug, Copy, Clone, EnumIter, PartialEq, Eq)]
pub enum Target {
    LinuxX32,
    LinuxX64,
    LinuxArm64,
    MacX64,
    MacArm64,
    WinX32,
    WinX64,
    WinArm64,
    Wasm32,
}

impl Target {
    pub const fn architecture(&self) -> Architecture {
        use Target::*;
        match self {
            LinuxX32 | WinX32 => Architecture::X86_32,
            LinuxX64 | WinX64 | MacX64 => Architecture::X86_64,
            LinuxArm64 | WinArm64 | MacArm64 => Architecture::Aarch64,
            Wasm32 => Architecture::Wasm32,
        }
    }

    pub const fn operating_system(&self) -> OperatingSystem {
        use Target::*;
        match self {
            LinuxX32 | LinuxX64 | LinuxArm64 => OperatingSystem::Linux,
            MacX64 | MacArm64 => OperatingSystem::Mac,
            WinX32 | WinX64 | WinArm64 => OperatingSystem::Windows,
            Wasm32 => OperatingSystem::Freestanding,
        }
    }

    pub const fn arch_os(&self) -> (Architecture, OperatingSystem) {
        (self.architecture(), self.operating_system())
    }

    pub const fn ptr_width(&self) -> PtrWidth {
        self.architecture().ptr_width()
    }

    pub const fn ptr_size(&self) -> usize {
        match self.ptr_width() {
            PtrWidth::Bytes4 => 4,
            PtrWidth::Bytes8 => 8,
        }
    }

    pub const fn max_by_value_size(&self) -> usize {
        // Pass values larger than 4 machine words by reference.
        // This is a reasonable default for most architectures. We want to pass large values by
        // reference because it's more efficient than copying them around on the stack, and puts
        // less pressure on CPU registers.
        self.ptr_size() * 4
    }

    pub const fn ptr_alignment_bytes(&self) -> usize {
        self.architecture().ptr_alignment_bytes()
    }

    pub const fn object_file_ext(&self) -> &str {
        use Target::*;
        match self {
            LinuxX32 | LinuxX64 | LinuxArm64 | MacX64 | MacArm64 => "o",
            WinX32 | WinX64 | WinArm64 => "obj",
            Wasm32 => "wasm",
        }
    }

    pub const fn static_library_file_ext(&self) -> &str {
        use Target::*;
        match self {
            LinuxX32 | LinuxX64 | LinuxArm64 | MacX64 | MacArm64 => "a",
            WinX32 | WinX64 | WinArm64 => "lib",
            Wasm32 => "wasm",
        }
    }

    pub const fn executable_file_ext(&self) -> Option<&str> {
        use Target::*;
        match self {
            LinuxX32 | LinuxX64 | LinuxArm64 | MacX64 | MacArm64 => None,
            WinX32 | WinX64 | WinArm64 => Some("exe"),
            Wasm32 => Some("wasm"),
        }
    }

    pub fn prebuilt_static_object(&self) -> String {
        use Target::*;
        match self {
            LinuxX32 | LinuxX64 | LinuxArm64 | MacX64 | MacArm64 | Wasm32 => {
                format!("{}-{}.o", self.operating_system(), self.architecture())
            }
            WinX32 | WinX64 | WinArm64 => {
                format!("{}-{}.obj", self.operating_system(), self.architecture())
            }
        }
    }

    pub fn prebuilt_static_library(&self) -> String {
        use Target::*;
        match self {
            LinuxX32 | LinuxX64 | LinuxArm64 | MacX64 | MacArm64 | Wasm32 => {
                format!("{}-{}.a", self.operating_system(), self.architecture())
            }
            WinX32 | WinX64 | WinArm64 => {
                format!("{}-{}.lib", self.operating_system(), self.architecture())
            }
        }
    }

    pub fn prebuilt_surgical_host(&self) -> String {
        format!("{}-{}.rh", self.operating_system(), self.architecture())
    }
}

pub enum ParseError {
    InvalidTargetString,
}

impl FromStr for Target {
    type Err = ParseError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        use Target::*;
        match s {
            "system" => Ok(Self::default()),
            "linux-x32" => Ok(LinuxX32),
            "linux-x64" => Ok(LinuxX64),
            "linux-arm64" => Ok(LinuxArm64),
            // TODO: Can we change these to just `mac`.
            // Currently, we need to keep it as `macos` to match platform naming.
            "macos-x64" => Ok(MacX64),
            "macos-arm64" => Ok(MacArm64),
            "windows-x32" => Ok(WinX32),
            "windows-x64" => Ok(WinX64),
            "windows-arm64" => Ok(WinArm64),
            "wasm32" => Ok(Wasm32),
            _ => Err(ParseError::InvalidTargetString),
        }
    }
}

impl From<Target> for &'static str {
    fn from(target: Target) -> Self {
        Self::from(&target)
    }
}

impl From<&Target> for &'static str {
    fn from(target: &Target) -> Self {
        use Target::*;
        match target {
            LinuxX32 => "linux-x32",
            LinuxX64 => "linux-x64",
            LinuxArm64 => "linux-arm64",
            // TODO: Can we change these to just `mac`.
            // Currently, we need to keep it as `macos` to match platform naming.
            MacX64 => "macos-x64",
            MacArm64 => "macos-arm64",
            WinX32 => "windows-x32",
            WinX64 => "windows-x64",
            WinArm64 => "windows-arm64",
            Wasm32 => "wasm32",
        }
    }
}

impl Default for Target {
    fn default() -> Self {
        Triple::host().into()
    }
}

impl From<&Triple> for Target {
    fn from(triple: &Triple) -> Self {
        use target_lexicon::*;
        match triple {
            Triple {
                architecture: Architecture::X86_32(_),
                operating_system: OperatingSystem::Linux,
                ..
            } => Target::LinuxX32,
            Triple {
                architecture: Architecture::X86_64,
                operating_system: OperatingSystem::Linux,
                ..
            } => Target::LinuxX64,
            Triple {
                architecture: Architecture::Aarch64(_),
                operating_system: OperatingSystem::Linux,
                ..
            } => Target::LinuxArm64,
            Triple {
                architecture: Architecture::X86_32(_),
                operating_system: OperatingSystem::Windows,
                ..
            } => Target::WinX32,
            Triple {
                architecture: Architecture::X86_64,
                operating_system: OperatingSystem::Windows,
                ..
            } => Target::WinX64,
            Triple {
                architecture: Architecture::Aarch64(_),
                operating_system: OperatingSystem::Windows,
                ..
            } => Target::WinArm64,
            Triple {
                architecture: Architecture::X86_64,
                operating_system: OperatingSystem::MacOSX { .. } | OperatingSystem::Darwin,
                ..
            } => Target::MacX64,
            Triple {
                architecture: Architecture::Aarch64(_),
                operating_system: OperatingSystem::MacOSX { .. } | OperatingSystem::Darwin,
                ..
            } => Target::MacArm64,
            Triple {
                architecture: Architecture::Wasm32,
                ..
            } => Target::Wasm32,
            _ => {
                user_error!("Target triple ({}) is not currently supported by the roc compiler. Feel free to file an issue to request support", triple);
            }
        }
    }
}

impl From<Triple> for Target {
    fn from(triple: Triple) -> Self {
        Target::from(&triple)
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum TargetFromTripleError {
    TripleUnsupported,
}

impl TryFrom<(Architecture, OperatingSystem)> for Target {
    type Error = TargetFromTripleError;

    fn try_from(arch_os: (Architecture, OperatingSystem)) -> Result<Self, Self::Error> {
        match arch_os {
            (Architecture::X86_32, OperatingSystem::Linux) => Ok(Target::LinuxX32),
            (Architecture::X86_64, OperatingSystem::Linux) => Ok(Target::LinuxX64),
            (Architecture::Aarch64, OperatingSystem::Linux) => Ok(Target::LinuxArm64),
            (Architecture::X86_32, OperatingSystem::Windows) => Ok(Target::WinX32),
            (Architecture::X86_64, OperatingSystem::Windows) => Ok(Target::WinX64),
            (Architecture::Aarch64, OperatingSystem::Windows) => Ok(Target::WinArm64),
            (Architecture::X86_64, OperatingSystem::Mac) => Ok(Target::MacX64),
            (Architecture::Aarch64, OperatingSystem::Mac) => Ok(Target::MacArm64),
            (Architecture::Wasm32, _) => Ok(Target::Wasm32),
            _ => Err(TargetFromTripleError::TripleUnsupported),
        }
    }
}

impl std::fmt::Display for Target {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", Into::<&'static str>::into(self))
    }
}
