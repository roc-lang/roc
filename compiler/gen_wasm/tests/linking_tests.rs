use std::process::Command;

const TEST_HOST_SOURCE: &str = "tests/linking_tests_host.c";
const TEST_HOST_TARGET: &str = "tests/linking_tests_host.wasm";

#[test]
fn build_host() {
    let args = [
        "build-obj",
        "-target",
        "wasm32-freestanding-musl",
        TEST_HOST_SOURCE,
        &format!("-femit-bin={}", TEST_HOST_TARGET),
    ];

    println!("zig {}", args.join(" "));

    Command::new("zig")
        .args(args)
        .output()
        .expect("failed to compile host");
}
