fn main() {
    let workspace_dir = workspace_dir();
    let platform_dir = workspace_dir.join("platform");
    let app_stub_path = platform_dir.join("libapp.roc");
    let platform_main = platform_dir.join("main.roc");

    println!("cargo:rustc-link-search={}", platform_dir.display());

    #[cfg(not(windows))]
    println!("cargo:rustc-link-lib=dylib=app");

    #[cfg(windows)]
    println!("cargo:rustc-link-lib=dylib=libapp");

    // watch the platform/main.roc and rebuild app stub if it changes
    println!("cargo:rerun-if-changed={}", platform_main.display());

    // build the app stub dynamic library
    std::process::Command::new("roc")
        .args(&[
            "build",
            "--lib",
            format!("{}", app_stub_path.display()).as_str(),
        ])
        .status()
        .expect("unable to build the app stub dynamic library 'platform/libapp.roc'");

    #[cfg(target_os = "macos")]
    let app_stub_extension = "libapp.dylib";

    #[cfg(target_os = "windows")]
    let app_stub_extension = "libapp.obj";

    #[cfg(target_os = "linux")]
    let app_stub_extension = "libapp.so";

    let expected_dylib_path = app_stub_path.with_file_name(app_stub_extension);

    // watch and rerun if there is a change in the dynamic library
    println!("cargo:rerun-if-changed={}", expected_dylib_path.display());

    // confirm the app stub dynamic library we built above is available
    match std::fs::metadata(&expected_dylib_path) {
        Ok(metadata) if metadata.is_file() => {
            println!(
                "cargo:warning=SUCCESSFULLY BUILT APP STUB DYNAMIC LIBRARY {:?}",
                expected_dylib_path
            );
        }
        _ => {
            println!(
                "cargo:warning=APP STUB DYNAMIC LIBRARY WAS NOT BUILT CORRECTLY {:?}",
                expected_dylib_path
            );
        }
    }
}

/// helper to get the path to the workspace
fn workspace_dir() -> std::path::PathBuf {
    let output = std::process::Command::new(env!("CARGO"))
        .arg("locate-project")
        .arg("--workspace")
        .arg("--message-format=plain")
        .output()
        .unwrap()
        .stdout;
    let cargo_path = std::path::Path::new(std::str::from_utf8(&output).unwrap().trim());
    cargo_path.parent().unwrap().to_path_buf()
}
