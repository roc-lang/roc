// ⚠️ READ THIS BEFORE MODIFYING THIS FILE! ⚠️
//
// This file is a fixture template. If the file you're looking at is
// in the fixture-templates/ directory, then you're all set - go ahead
// and modify it, and it will modify all the fixture tests.
//
// If this file is in the fixtures/ directory, on the other hand, then
// it is gitignored and will be overwritten the next time tests run.
// So you probably don't want to modify it by hand! Instead, modify the
// file with the same name in the fixture-templates/ directory.

fn main() {
    #[cfg(not(windows))]
    println!("cargo:rustc-link-lib=dylib=app");

    #[cfg(windows)]
    println!("cargo:rustc-link-lib=dylib=libapp");

    println!("cargo:rustc-link-search=.");
}
