// We need set these linker flags to prevent `cargo build` for the workspace from failing with the following error:
//
// ```
// = note: Undefined symbols for architecture arm64:
//           "_roc__main_for_host_1_exposed_generic", referenced from:
//               _main in rustplatform-df9e357e0cc989a6.rustplatform.863be87f3956573-cgu.0.rcgu.o
//         ld: symbol(s) not found for architecture arm64
//         clang-16: error: linker command failed with exit code 1 (use -v to see invocation)
// ```
//
// This is ok, because this static library will be linked later by roc and the symbols will be resolved.
//
fn main() {
    // Get the target triple
    let target = std::env::var("TARGET").unwrap();

    // Set the entry point to main
    println!("cargo:rustc-link-arg=-e");
    println!("cargo:rustc-link-arg=main");

    // Add platform-specific flags for undefined symbols
    if target.contains("apple") {
        println!("cargo:rustc-link-arg=-undefined");
        println!("cargo:rustc-link-arg=dynamic_lookup");
    } else if target.contains("linux") {
        println!("cargo:rustc-link-arg=--allow-undefined");
    } else if target.contains("windows") {
        println!("cargo:rustc-link-arg=/FORCE:UNRESOLVED"); // Windows MSVC
    }

    // Force rebuild if this build script changes
    println!("cargo:rerun-if-changed=build.rs");
}
