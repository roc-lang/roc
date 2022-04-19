//use roc_cli;

/*
TODO: run roc CLI and put its output in OUT_DIR
*/
fn main() {
    println!("cargo:rustc-link-lib=dylib=app");
    println!("cargo:rustc-link-search=.");
}
