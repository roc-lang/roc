fn main() {
    // workaround for issue https://github.com/NixOS/nixpkgs/issues/166205 . This println can be removed when this issue is fixed. Upgrading to LLVM 14 could also fix this issue.
    // also see https://github.com/NixOS/nixpkgs/pull/181485
    #[cfg(all(target_os = "macos", target_arch = "aarch64"))]
    println!("cargo:rustc-link-lib=c++abi")
}
