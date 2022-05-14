
 use std::process::Command;

fn main() {
    let bc_file = "/home/anton/gitrepos/2roc/roc/examples/hello-world/wasm-rust-platform/app/helloWeb.bc";

    // llc-13 -O3 -filetype=obj roc_app_wasm.bc -o roc_app_wasm.o
    Command::new("llc-13")
        .arg("-O3")
        .arg("-filetype=obj")
        .arg(bc_file)
        .arg("-o")
        .arg("/home/anton/gitrepos/2roc/roc/examples/hello-world/wasm-rust-platform/target/wasm32-unknown-unknown/roc_app_wasm.o")
        .output()
        .unwrap();

    // ar rcs libroc_app.a roc_app_wasm.o
    Command::new("ar")
        .arg("rcs")
        .arg("/home/anton/gitrepos/2roc/roc/examples/hello-world/wasm-rust-platform/target/wasm32-unknown-unknown/libroc_app.a")
        .arg("/home/anton/gitrepos/2roc/roc/examples/hello-world/wasm-rust-platform/target/wasm32-unknown-unknown/roc_app_wasm.o")
        .output()
        .unwrap();

    println!("cargo:rustc-link-lib=static=roc_app");
    println!("cargo:rustc-link-search=/home/anton/gitrepos/2roc/roc/examples/hello-world/wasm-rust-platform/target/wasm32-unknown-unknown");
}