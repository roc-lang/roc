use parse_display::{Display, FromStr};
use strum_macros::EnumIter;
use target_lexicon::{Architecture, BinaryFormat, OperatingSystem, Triple, X86_32Architecture};

#[derive(Display, FromStr, EnumIter)]
pub enum Backend {
    #[display("host")]
    Host,
    #[display("x86_32")]
    X86_32,
    #[display("x86_64")]
    X86_64,
    #[display("wasm32")]
    Wasm32,
}

impl Default for Backend {
    fn default() -> Self {
        Backend::Host
    }
}

impl Backend {
    pub fn to_triple(&self) -> Triple {
        let mut triple = Triple::unknown();

        match self {
            Backend::Host => Triple::host(),
            Backend::X86_32 => {
                triple.architecture = Architecture::X86_32(X86_32Architecture::I386);
                triple.binary_format = BinaryFormat::Elf;

                // TODO make this user-specified?
                triple.operating_system = OperatingSystem::Linux;

                triple
            }
            Backend::X86_64 => {
                triple.architecture = Architecture::X86_64;
                triple.binary_format = BinaryFormat::Elf;

                triple
            }
            Backend::Wasm32 => {
                triple.architecture = Architecture::Wasm32;
                triple.binary_format = BinaryFormat::Wasm;

                triple
            }
        }
    }
}
