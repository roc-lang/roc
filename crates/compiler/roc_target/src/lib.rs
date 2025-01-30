//! Provides types and helpers for compiler targets such as `default_x86_64`.
#![warn(clippy::dbg_macro)]
// See github.com/roc-lang/roc/issues/800 for discussion of the large_enum_variant check.
#![allow(clippy::large_enum_variant)]

use std::path::{Path, PathBuf};
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
            Architecture::Aarch32 => "aarch32",
            Architecture::Aarch64 => "aarch64",
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

#[derive(Debug, PartialEq, Eq)]
pub struct SurgicalHostArtifacts {
    pub metadata: PathBuf,
    pub preprocessed_host: PathBuf,
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

    // file extension for an object file
    pub const fn object_file_ext(&self) -> &str {
        use Target::*;
        match self {
            LinuxX32 | LinuxX64 | LinuxArm64 | MacX64 | MacArm64 => "o",
            WinX32 | WinX64 | WinArm64 => "obj",
            Wasm32 => "wasm",
        }
    }

    // file extension for a static library file
    pub const fn static_library_file_ext(&self) -> &str {
        use Target::*;
        match self {
            LinuxX32 | LinuxX64 | LinuxArm64 | MacX64 | MacArm64 => "a",
            WinX32 | WinX64 | WinArm64 => "lib",
            Wasm32 => "wasm",
        }
    }

    // file extension for a dynamic/shared library file
    pub const fn dynamic_library_file_ext(&self) -> &str {
        use Target::*;
        match self {
            LinuxX32 | LinuxX64 | LinuxArm64 => "so",
            MacX64 | MacArm64 => "dylib",
            WinX32 | WinX64 | WinArm64 => "dll",
            Wasm32 => "wasm",
        }
    }

    // file extension for an executable file
    pub const fn executable_file_ext(&self) -> Option<&str> {
        use Target::*;
        match self {
            LinuxX32 | LinuxX64 | LinuxArm64 | MacX64 | MacArm64 => None,
            WinX32 | WinX64 | WinArm64 => Some("exe"),
            Wasm32 => Some("wasm"),
        }
    }

    // file name for a prebuilt host object file
    // used for legacy linking
    pub fn prebuilt_static_object(&self) -> String {
        use Target::*;
        match self {
            LinuxX32 | LinuxX64 | LinuxArm64 | MacX64 | MacArm64 | Wasm32 => {
                format!("{}.o", self)
            }
            WinX32 | WinX64 | WinArm64 => {
                format!("{}.obj", self)
            }
        }
    }

    // file name for a prebuilt host static library file
    // used for legacy linking
    pub fn prebuilt_static_library(&self) -> String {
        use Target::*;
        match self {
            LinuxX32 | LinuxX64 | LinuxArm64 | MacX64 | MacArm64 | Wasm32 => {
                format!("{}.a", self)
            }
            WinX32 | WinX64 | WinArm64 => {
                format!("{}.lib", self)
            }
        }
    }

    // file name for a preprocessed host executable file
    // used for surgical linking
    pub fn prebuilt_surgical_host(&self) -> String {
        format!("{}.rh", self) // short for roc host
    }

    // file name for a preprocessed host metadata file
    // used for surgical linking
    pub fn metadata_file_name(&self) -> String {
        format!("metadata_{}.rm", self) // short for roc metadata
    }

    // file name for a stubbed app dynamic library file
    pub fn stub_app_lib_file_name(&self) -> String {
        format!("libapp.{}", self.dynamic_library_file_ext())
    }

    /// Search for a prebuilt legacy host in the platform main directory.
    pub fn find_legacy_host(&self, platform_main_roc: &Path) -> Result<PathBuf, String> {
        let static_library_path = platform_main_roc.with_file_name(self.prebuilt_static_library());

        let static_object_path = platform_main_roc.with_file_name(self.prebuilt_static_object());

        let generic_host_path: PathBuf = platform_main_roc
            .with_file_name("libhost")
            .with_extension(self.static_library_file_ext());

        if static_library_path.exists() {
            Ok(static_library_path)
        } else if generic_host_path.exists() {
            Ok(generic_host_path)
        } else if static_object_path.exists() {
            Ok(static_object_path)
        } else {
            Err(format!(
                "Failed to find any legacy linking files; I need one of these three paths to exist:\n    {}\n    {}\n    {}",
                static_library_path.display(),
                static_object_path.display(),
                generic_host_path.display(),
            )
            .to_string())
        }
    }

    /// Search for a prebuilt surgical host in the platform main directory.
    pub fn find_surgical_host(
        &self,
        platform_main_roc: &Path,
    ) -> Result<SurgicalHostArtifacts, String> {
        let surgical_metadata = platform_main_roc.with_file_name(self.metadata_file_name());
        let surgical_host_path = platform_main_roc.with_file_name(self.prebuilt_surgical_host());

        let generic_host_path: PathBuf = platform_main_roc.with_file_name("host.rh");
        let generic_metadata: PathBuf = platform_main_roc.with_file_name("metadata_host.rm");

        if generic_host_path.exists() && generic_metadata.exists() {
            Ok(SurgicalHostArtifacts {
                metadata: generic_metadata,
                preprocessed_host: generic_host_path,
            })
        } else if surgical_host_path.exists() && surgical_metadata.exists() {
            Ok(SurgicalHostArtifacts {
                metadata: surgical_metadata,
                preprocessed_host: surgical_host_path,
            })
        } else {
            // TODO further improve the error message

            Err(format!(
                "Either the generic host files or the surgical host files must exist. \
                File status: \
                Generic host ({}): {}, \
                Generic metadata ({}): {}, \
                Surgical host ({}): {}, \
                Surgical metadata ({}): {}",
                generic_host_path.display(),
                if generic_host_path.exists() {
                    "present"
                } else {
                    "missing"
                },
                generic_metadata.display(),
                if generic_metadata.exists() {
                    "present"
                } else {
                    "missing"
                },
                surgical_host_path.display(),
                if surgical_host_path.exists() {
                    "present"
                } else {
                    "missing"
                },
                surgical_metadata.display(),
                if surgical_metadata.exists() {
                    "present"
                } else {
                    "missing"
                }
            ))
        }
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
