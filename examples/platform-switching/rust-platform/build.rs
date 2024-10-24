use std::path::{Path, PathBuf};

fn main() {
    #[cfg(not(windows))]
    println!("cargo:rustc-link-lib=dylib=app");

    #[cfg(windows)]
    println!("cargo:rustc-link-lib=dylib=libapp");

    #[cfg(target_os = "macos")]
    let dylib_file_name = "libapp.dylib";

    #[cfg(target_os = "linux")]
    let dylib_file_name = "libapp.so";

    #[cfg(target_os = "windows")]
    let dylib_file_name = "libapp.dll";

    // Get the build cache directory (OUT_DIR)
    let out_dir = std::env::var("OUT_DIR").unwrap();

    let lib_app = workspace_root()
        .join("examples")
        .join("platform-switching")
        .join("rust-platform")
        .join(dylib_file_name);

    let out_path = Path::new(&out_dir).join(dylib_file_name);

    // copy the dylib to the output build cache
    std::fs::copy(lib_app, out_path).unwrap();

    // Search for static libraries in the cache directory
    println!("cargo:rustc-link-search=native={out_dir}");
}

pub fn workspace_root() -> PathBuf {
    let root = std::env::var("ROC_WORKSPACE_DIR").expect("Can't find the ROC_WORKSPACE_DIR variable expected to be set in .cargo/config.toml. Are you running tests outside of cargo?");
    PathBuf::from(root)
}
