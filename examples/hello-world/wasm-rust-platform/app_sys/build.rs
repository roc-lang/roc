fn main() {
    println!("cargo:rustc-link-lib=static=roc_app");
    println!("cargo:rustc-link-search=.");
}