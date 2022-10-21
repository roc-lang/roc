use indoc::indoc;
use roc_linker::pe;
use std::path::Path;

fn main() {
    #[cfg(windows)]
    let dir = tempfile::tempdir().unwrap();
    #[cfg(windows)]
    let dir = dir.path();
    // let dir = Path::new("C:Users\\folkert\\Documents\\Github\\roc\\linktest");

    #[cfg(unix)]
    let dir = Path::new("/tmp/roc");

    test_basics(dir);

    let mut bytes = std::fs::read(dir.join("app.exe")).unwrap();

    #[cfg(windows)]
    unsafe {
        memexec::memexec_exe(&bytes).unwrap()
    };

    #[cfg(unix)]
    {}
}

fn zig_host_app(dir: &Path, host_zig: &str, app_zig: &str) {
    let zig = std::env::var("ROC_ZIG").unwrap_or_else(|_| "zig".into());

    std::fs::write(dir.join("host.zig"), host_zig.as_bytes()).unwrap();
    std::fs::write(dir.join("app.zig"), app_zig.as_bytes()).unwrap();

    // we need to compile the app first
    let output = std::process::Command::new(&zig)
        .current_dir(dir)
        .args(&[
            "build-obj",
            "app.zig",
            "-target",
            "x86_64-windows-gnu",
            "--strip",
            "-rdynamic",
            "-OReleaseFast",
        ])
        .output()
        .unwrap();

    if !output.status.success() {
        use std::io::Write;

        std::io::stdout().write_all(&output.stdout).unwrap();
        std::io::stderr().write_all(&output.stderr).unwrap();

        panic!("zig build-obj failed");
    }

    // open our app object; we'll copy sections from it later
    let file = std::fs::File::open(dir.join("app.obj")).unwrap();
    let roc_app = unsafe { memmap2::Mmap::map(&file) }.unwrap();

    let roc_app_sections = pe::AppSections::from_data(&*roc_app);
    let symbols = roc_app_sections.roc_symbols;

    // make the dummy dylib based on the app object
    let names: Vec<_> = symbols.iter().map(|s| s.name.clone()).collect();
    let dylib_bytes = roc_linker::generate_dylib::pe::synthetic_dll(&names);
    std::fs::write(dir.join("libapp.obj"), dylib_bytes).unwrap();

    // now we can compile the host (it uses libapp.obj, hence the order here)
    let output = std::process::Command::new(&zig)
        .current_dir(dir)
        .args(&[
            "build-exe",
            "libapp.obj",
            "host.zig",
            "-lc",
            "-target",
            "x86_64-windows-gnu",
            "-rdynamic",
            "--strip",
            "-rdynamic",
            "-OReleaseFast",
        ])
        .output()
        .unwrap();

    if !output.status.success() {
        use std::io::Write;

        std::io::stdout().write_all(&output.stdout).unwrap();
        std::io::stderr().write_all(&output.stderr).unwrap();

        panic!("zig build-exe failed");
    }

    pe::preprocess_windows(
        &dir.join("host.exe"),
        &dir.join("metadata"),
        &dir.join("preprocessedhost"),
        false,
        false,
    )
    .unwrap();

    std::fs::copy(&dir.join("preprocessedhost"), &dir.join("app.exe")).unwrap();

    pe::surgery_pe(&dir.join("app.exe"), &dir.join("metadata"), &*roc_app);
}

#[no_mangle]
fn test_basics(dir: &Path) {
    zig_host_app(
        dir,
        indoc!(
            r#"
                const std = @import("std");

                extern fn roc_magic1() callconv(.C) u8;

                pub fn main() u8 {
                    return roc_magic1();
                }
                "#
        ),
        indoc!(
            r#"
                export fn roc_magic1() u8 {
                    return 32;
                }
                "#
        ),
    );
}

#[cfg(windows)]
mod windows_roc_platform_functions {
    use core::ffi::c_void;

    /// # Safety
    /// The Roc application needs this.
    #[no_mangle]
    pub unsafe fn roc_alloc(size: usize, _alignment: u32) -> *mut c_void {
        libc::malloc(size)
    }

    /// # Safety
    /// The Roc application needs this.
    #[no_mangle]
    pub unsafe fn roc_realloc(
        c_ptr: *mut c_void,
        new_size: usize,
        _old_size: usize,
        _alignment: u32,
    ) -> *mut c_void {
        libc::realloc(c_ptr, new_size)
    }

    /// # Safety
    /// The Roc application needs this.
    #[no_mangle]
    pub unsafe fn roc_dealloc(c_ptr: *mut c_void, _alignment: u32) {
        libc::free(c_ptr)
    }
}
