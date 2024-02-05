use object::write;
use object::{Architecture, BinaryFormat, Endianness, SymbolFlags, SymbolKind, SymbolScope};
use roc_error_macros::internal_error;
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
    let dummy_lib_file = tmp.path().to_path_buf().with_file_name("libapp.dylib");

    let obj_target = BinaryFormat::MachO;
    let (obj_arch, zig_target) = match triple.architecture {
        target_lexicon::Architecture::X86_64 => (Architecture::X86_64, "x86_64-macos-none"),
        target_lexicon::Architecture::Aarch64(_) => (Architecture::Aarch64, "aarch64-macos-none"),
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

    let output = Command::new("zig")
        .arg("build-lib")
        .args([
            "-target",
            zig_target,
            format!("-femit-bin={}", dummy_lib_file.as_path().display()).as_str(),
            "-dynamic",
            dummy_obj_file.path().to_str().unwrap(),
        ])
        .output()
        .unwrap();

    // Extend the lifetime of the tempfile so it doesn't get dropped
    // (and thus deleted) before the linker process is done using it!
    let _ = dummy_obj_file;

    if !output.status.success() {
        match std::str::from_utf8(&output.stderr) {
            Ok(stderr) => panic!(
                "Failed to link dummy shared library - stderr of the `zig build-lib` command was:\n{stderr}"
            ),
            Err(utf8_err) => panic!(
                "Failed to link dummy shared library  - stderr of the `zig build-lib` command was invalid utf8 ({utf8_err:?})"
            ),
        }
    }

    Ok(std::fs::read(dummy_lib_file).expect("Failed to load dummy library"))
}
