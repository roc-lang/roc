// Adapted from https://github.com/TheDan64/scoped_alloca
// by Daniel Kolsoi, licensed under the Apache License 2.0
// Thank you, Dan!

use std::env;
use std::fs::create_dir;
use std::path::Path;
use std::process::Command;

fn main() {
    let manifest_dir = env::var("CARGO_MANIFEST_DIR").unwrap();
    let cargo_dir = Path::new(manifest_dir.as_str());
    let lib_dir = cargo_dir.join("lib");
    let alloca_c = cargo_dir.join("src/alloca.c");
    let alloca_o = lib_dir.join("alloca.o");
    let liballoca_a = lib_dir.join("liballoca.a");

    println!("cargo:rustc-link-search=native={}", lib_dir.display());

    // No need to recompile alloca static lib every time. We could
    // add a feature flag to do so if needed, though
    if liballoca_a.is_file() {
        return;
    }

    if !lib_dir.is_dir() {
        create_dir(&lib_dir).unwrap();
    }

    let clang_output = Command::new("clang")
        .arg("-c")
        .arg(alloca_c)
        .arg("-o")
        .arg(&alloca_o)
        .output()
        .expect("Could not execute clang");

    assert!(clang_output.status.success(), "{:?}", clang_output);

    let ar_output = Command::new("ar")
        .arg("-q")
        .arg(liballoca_a)
        .arg(alloca_o)
        .output()
        .expect("Could not execute ar");

    assert!(ar_output.status.success(), "{:?}", ar_output);
}
