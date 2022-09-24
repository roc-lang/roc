use target_lexicon::Triple;

mod elf64;
mod pe;

#[cfg(test)]
pub(crate) use pe::synthetic_dll;

pub fn generate(target: &Triple, custom_names: &[String]) -> object::read::Result<Vec<u8>> {
    match target.binary_format {
        target_lexicon::BinaryFormat::Elf => elf64::create_dylib_elf64(custom_names),
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
        // NOTE: this does not work
        //
        //        let target = target_lexicon::Triple {
        //            architecture: target_lexicon::Architecture::X86_64,
        //            operating_system: target_lexicon::OperatingSystem::Windows,
        //            binary_format: target_lexicon::BinaryFormat::Coff,
        //            ..target_lexicon::Triple::host()
        //        };
        //
        //        check_exports(&target);
        //
        // Because our exports point back into the .edata section, they are considered "forward"
        // exports: they aren't actually defined in this .dll and hence not considered exports by
        // the `object` crate.
    }

    #[test]
    fn check_exports_coff_manual() {
        let target = target_lexicon::Triple {
            architecture: target_lexicon::Architecture::X86_64,
            operating_system: target_lexicon::OperatingSystem::Windows,
            binary_format: target_lexicon::BinaryFormat::Coff,
            ..target_lexicon::Triple::host()
        };

        let custom_names = ["foo".to_string(), "bar".to_string()];

        let bytes = generate(&target, &custom_names).unwrap();
        let object = object::read::pe::PeFile64::parse(bytes.as_slice()).unwrap();

        let exports = {
            let mut exports = Vec::new();
            if let Some(export_table) = object.export_table().unwrap() {
                for (name_pointer, _address_index) in export_table.name_iter() {
                    let name = export_table.name_from_pointer(name_pointer).unwrap();

                    // here the standard `.exports()` checks for `if !export_table.is_forward(address)`
                    exports.push(name)
                }
            }
            exports
        };

        for custom in custom_names {
            assert!(
                exports.iter().any(|name| *name == custom.as_bytes()),
                "missing {}",
                &custom
            );
        }
    }
}
