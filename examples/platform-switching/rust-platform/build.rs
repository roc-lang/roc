fn main() {
    // println!("cargo:rustc-link-lib=dylib=libapp");
    // println!("cargo:rustc-link-search=.");

    cc::Build::new().file("libapp.dll").compile("app");
}
