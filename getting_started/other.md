
1. [Install Rust](https://rustup.rs/)
2. [Build roc from source](../BUILDING_FROM_SOURCE.md)
3. Run examples with:
    ```
    # Rust
    cargo run examples/hello-world/rust-platform/helloRust.roc
    # Zig
    cargo run examples/hello-world/zig-platform/helloZig.roc
    # C
    cargo run examples/hello-world/c-platform/helloC.roc
    ```
4. See [here](../README.md#examples) for the other examples.

**Tip:** when programming in roc, we recommend to execute `./roc check myproject/Foo.roc` before `./roc myproject/Foo.roc` or `./roc build myproject/Foo.roc`. `./roc check` can produce clear error messages in cases where building/running may panic.