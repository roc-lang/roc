use tempfile::NamedTempFile;

const HOST_WASM: &[u8] = include_bytes!(concat!(env!("OUT_DIR"), "/builtins-wasm32.o"));
// TODO: in the future, we should use Zig's cross-compilation to generate and store these
// for all targets, so that we can do cross-compilation!
#[cfg(unix)]
const HOST_UNIX: &[u8] = include_bytes!(concat!(env!("OUT_DIR"), "/builtins-host.o"));
#[cfg(windows)]
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

#[cfg(unix)]
fn host_unix_tempfile() -> std::io::Result<NamedTempFile> {
    let tempfile = tempfile::Builder::new()
        .prefix("host_bitcode")
        .suffix(".o")
        .rand_bytes(8)
        .tempfile()?;

    std::fs::write(tempfile.path(), HOST_UNIX)?;

    Ok(tempfile)
}

#[cfg(windows)]
fn host_windows_tempfile() -> std::io::Result<NamedTempFile> {
    let tempfile = tempfile::Builder::new()
        .prefix("host_bitcode")
        .suffix(".obj")
        .rand_bytes(8)
        .tempfile()?;

    std::fs::write(tempfile.path(), HOST_WINDOWS)?;

    Ok(tempfile)
}

pub fn host_tempfile() -> std::io::Result<NamedTempFile> {
    #[cfg(unix)]
    {
        host_unix_tempfile()
    }

    #[cfg(windows)]
    {
        host_windows_tempfile()
    }

    #[cfg(not(any(windows, unix)))]
    {
        unreachable!()
    }
}
