use std::process::Command;

fn main() {
    println!("cargo:rerun-if-changed=build.rs");

    if std::env::var("CARGO_FEATURE_WASM").is_ok() {
        Command::new("cargo")
            .args([
                "build",
                "--target",
                "wasm32-wasi",
                "-p",
                "roc_repl_wasm",
                "--no-default-features",
                "--features",
                "wasi_test",
                "--release",
                "--target-dir",
                "../../.ignore/repl_test_target",
            ])
            .spawn()
            .unwrap();
    }
}
