use roc_target::{OperatingSystem, Target};

mod elf64;
mod macho;
mod pe;

#[cfg(test)]
pub(crate) use pe::synthetic_dll;

#[cfg(test)]
pub(crate) use elf64::create_dylib_elf64;

pub(crate) use pe::APP_DLL;

pub fn generate(target: Target, custom_names: &[String]) -> object::read::Result<Vec<u8>> {
    match target.operating_system() {
        OperatingSystem::Linux => elf64::create_dylib_elf64(custom_names),
        OperatingSystem::Mac => macho::create_dylib_macho(custom_names, target),
        OperatingSystem::Windows => Ok(pe::synthetic_dll(custom_names)),
        other => unimplemented!("dylib creation for {:?}", other),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    use object::Object;
    use target_lexicon::Triple;

    fn check_exports(triple: &Triple) {
        let target = triple.into();
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
        let custom_names = ["foo".to_string(), "bar".to_string()];

        let bytes = generate(Target::WinX64, &custom_names).unwrap();
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
