
 use std::process::Command;

fn main() {
    // to generate file:
    // cargo run build --target wasm32 --no-link ./examples/hello-world/wasm-rust-platform/app/helloWeb.roc
    let bc_file = "/home/anton/gitrepos/trunk_roc/roc/examples/hello-world/wasm-rust-platform/app/helloWeb.bc";

    // llc-13 -O3 -filetype=obj roc_app_wasm.bc -o roc_app_wasm.o
    Command::new("llc-13")
        .arg("-O3")
        .arg("-filetype=obj")
        .arg(bc_file)
        .arg("-o")
        .arg("/home/anton/gitrepos/trunk_roc/roc/examples/hello-world/wasm-rust-platform/target/wasm32-unknown-unknown/roc_app_wasm.o")
        .output()
        .unwrap();

    // ar rcs libroc_app.a roc_app_wasm.o
    Command::new("ar")
        .arg("rcs")
        .arg("/home/anton/gitrepos/trunk_roc/roc/examples/hello-world/wasm-rust-platform/target/wasm32-unknown-unknown/libroc_app.a") //.a file is the output
        .arg("/home/anton/gitrepos/trunk_roc/roc/examples/hello-world/wasm-rust-platform/target/wasm32-unknown-unknown/roc_app_wasm.o")
        .output()
        .unwrap();

    println!("cargo:rustc-link-lib=static=roc_app");
    println!("cargo:rustc-link-search=/home/anton/gitrepos/trunk_roc/roc/examples/hello-world/wasm-rust-platform/target/wasm32-unknown-unknown");
}