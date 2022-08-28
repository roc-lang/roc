# Roc installation guide for other systems

1. [Install Rust](https://rustup.rs/)

1. [Build Roc from source](../BUILDING_FROM_SOURCE.md)

1. Run examples:

    ```sh
    cargo run examples/platform-switching/rust-platform/rocLovesRust.roc

    # This requires installing the Zig compiler, too.
    cargo run examples/platform-switching/zig-platform/rocLovesZig.roc

    # This requires installing the `clang` C compiler, too.
    cargo run examples/platform-switching/c-platform/rocLovesC.roc
    ```
