use object::write;
use object::{Architecture, BinaryFormat, Endianness, SymbolFlags, SymbolKind, SymbolScope};
use roc_error_macros::internal_error;
use std::path::Path;
use std::process::Command;
use target_lexicon::Triple;

// TODO: Eventually do this from scratch and in memory instead of with ld.
pub fn create_dylib_macho(
    custom_names: &[String],
    triple: &Triple,
) -> object::read::Result<Vec<u8>> {
    let dummy_obj_file = tempfile::Builder::new()
        .prefix("roc_lib")
        .suffix(".o")
        .tempfile()
        .unwrap_or_else(|e| internal_error!("{}", e));
    let tmp = tempfile::tempdir().unwrap_or_else(|e| internal_error!("{}", e));
    let dummy_lib_file = tmp.path().to_path_buf().with_file_name("libapp.so");

    let obj_target = BinaryFormat::MachO;
    let obj_arch = match triple.architecture {
        target_lexicon::Architecture::X86_64 => Architecture::X86_64,
        target_lexicon::Architecture::Aarch64(_) => Architecture::Aarch64,
        _ => {
            // We should have verified this via supported() before calling this function
            unreachable!()
        }
    };
    let mut out_object = write::Object::new(obj_target, obj_arch, Endianness::Little);

    let text_section = out_object.section_id(write::StandardSection::Text);

    for name in custom_names {
        out_object.add_symbol(write::Symbol {
            name: name.as_bytes().to_vec(),
            value: 0,
            size: 0,
            kind: SymbolKind::Text,
            scope: SymbolScope::Dynamic,
            weak: false,
            section: write::SymbolSection::Section(text_section),
            flags: SymbolFlags::None,
        });
    }

    std::fs::write(
        dummy_obj_file.path(),
        out_object.write().expect("failed to build output object"),
    )
    .expect("failed to write object to file");

    // This path only exists on macOS Big Sur, and it causes ld errors
    // on Catalina if it's specified with -L, so we replace it with a
    // redundant -lSystem if the directory isn't there.
    let big_sur_path = "/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/lib";
    let big_sur_fix = if Path::new(big_sur_path).exists() {
        "-L/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/lib"
    } else {
        "-lSystem" // We say -lSystem twice in the case of non-Big-Sur OSes, but it's fine.
    };

    let ld_flag_soname = "-install_name";
    let ld_prefix_args = [big_sur_fix, "-lSystem", "-dylib"];

    let output = Command::new("ld")
        .args(ld_prefix_args)
        .args([
            ld_flag_soname,
            dummy_lib_file.file_name().unwrap().to_str().unwrap(),
            dummy_obj_file.path().to_str().unwrap(),
            "-o",
            dummy_lib_file.to_str().unwrap(),
        ])
        .output()
        .unwrap();

    // Extend the lifetime of the tempfile so it doesn't get dropped
    // (and thus deleted) before the linker process is done using it!
    let _ = dummy_obj_file;

    if !output.status.success() {
        match std::str::from_utf8(&output.stderr) {
            Ok(stderr) => panic!(
                "Failed to link dummy shared library - stderr of the `ld` command was:\n{}",
                stderr
            ),
            Err(utf8_err) => panic!(
                "Failed to link dummy shared library  - stderr of the `ld` command was invalid utf8 ({:?})",
                utf8_err
            ),
        }
    }

    Ok(std::fs::read(dummy_lib_file).expect("Failed to load dummy library"))
}
