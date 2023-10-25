use tempfile::NamedTempFile;

const HOST_WASM: &[u8] = include_bytes!(concat!(env!("OUT_DIR"), "/builtins-wasm32.o"));
// TODO: in the future, we should use Zig's cross-compilation to generate and store these
// for all targets, so that we can do cross-compilation!
const HOST_UNIX: &[u8] = include_bytes!(concat!(env!("OUT_DIR"), "/builtins-host.o"));
const HOST_WINDOWS: &[u8] =
    include_bytes!(concat!(env!("OUT_DIR"), "/builtins-windows-x86_64.obj"));

pub fn host_wasm_tempfile() -> std::io::Result<NamedTempFile> {
    let tempfile = tempfile::Builder::new()
        .prefix("host_bitcode")
        .suffix(".wasm")
        .rand_bytes(8)
        .tempfile()?;

    std::fs::write(tempfile.path(), HOST_WASM)?;

    Ok(tempfile)
}

fn current_host_tempfile() -> &'static std::io::Result<NamedTempFile> {
    use std::sync::OnceLock;

    static TMP: OnceLock<std::io::Result<NamedTempFile>> = OnceLock::new();

    fn helper() -> std::io::Result<NamedTempFile> {
        let tempfile = tempfile::Builder::new()
            .prefix("host_bitcode")
            .suffix(".o")
            .rand_bytes(8)
            .tempfile()?;

        std::fs::write(tempfile.path(), HOST_UNIX)?;

        Ok(tempfile)
    }

    TMP.get_or_init(helper)
}

fn host_windows_tempfile() -> &'static std::io::Result<NamedTempFile> {
    use std::sync::OnceLock;

    static TMP: OnceLock<std::io::Result<NamedTempFile>> = OnceLock::new();

    fn helper() -> std::io::Result<NamedTempFile> {
        let tempfile = tempfile::Builder::new()
            .prefix("host_bitcode")
            .suffix(".obj")
            .rand_bytes(8)
            .tempfile()?;

        std::fs::write(tempfile.path(), HOST_WINDOWS)?;

        Ok(tempfile)
    }

    TMP.get_or_init(helper)
}

pub fn host_tempfile(target: &target_lexicon::Triple) -> &'static std::io::Result<NamedTempFile> {
    use target_lexicon::Triple;

    match target {
        Triple {
            // architecture: target_lexicon::Architecture::X86_64,
            // operating_system: target_lexicon::OperatingSystem::Linux,
            binary_format: target_lexicon::BinaryFormat::Elf,
            ..
        } => current_host_tempfile(),

        // macho support is incomplete
        Triple {
            // operating_system: target_lexicon::OperatingSystem::Darwin,
            binary_format: target_lexicon::BinaryFormat::Macho,
            ..
        } => current_host_tempfile(),

        Triple {
            architecture: target_lexicon::Architecture::X86_64,
            operating_system: target_lexicon::OperatingSystem::Windows,
            binary_format: target_lexicon::BinaryFormat::Coff,
            ..
        } => host_windows_tempfile(),

        other => unimplemented!("no host for {other:?} architecture"),
    }
}
