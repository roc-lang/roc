fn main() {
    // println!("cargo:rustc-link-lib=dylib=libapp");
    // println!("cargo:rustc-link-search=.");

    cc::Build::new().object("foo.lib").compile("app");
}
