use target_lexicon::Triple;

mod elf64;
mod pe;

/// an empty shared library, that we build on top of
const DUMMY_ELF64: &[u8] = include_bytes!("../../dummy-elf64-x86-64.so");

pub fn generate(target: &Triple, custom_names: &[String]) -> object::read::Result<Vec<u8>> {
    match target.binary_format {
        target_lexicon::BinaryFormat::Elf => elf64::create_dylib_elf64(DUMMY_ELF64, custom_names),
        target_lexicon::BinaryFormat::Macho => todo!("macho dylib creation"),
        target_lexicon::BinaryFormat::Coff => Ok(pe::synthetic_dll(custom_names)),
        other => unimplemented!("dylib creation for {:?}", other),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    use object::Object;

    fn check_exports(target: &Triple) {
        let custom_names = ["foo".to_string(), "bar".to_string()];

        let bytes = generate(target, &custom_names).unwrap();
        let object = object::File::parse(bytes.as_slice()).unwrap();

        let exports = object.exports().unwrap();
        for custom in custom_names {
            assert!(
                exports.iter().any(|e| e.name() == custom.as_bytes()),
                "missing {}",
                &custom
            );
        }
    }

    #[test]
    fn check_exports_elf64() {
        let target = target_lexicon::Triple {
            architecture: target_lexicon::Architecture::X86_64,
            operating_system: target_lexicon::OperatingSystem::Linux,
            binary_format: target_lexicon::BinaryFormat::Elf,
            ..target_lexicon::Triple::host()
        };

        check_exports(&target);
    }

    #[test]
    fn check_exports_coff() {
        let target = target_lexicon::Triple {
            architecture: target_lexicon::Architecture::X86_64,
            operating_system: target_lexicon::OperatingSystem::Windows,
            binary_format: target_lexicon::BinaryFormat::Coff,
            ..target_lexicon::Triple::host()
        };

        check_exports(&target);
    }
}
