# Roc installation guide for x86 macOS systems

## How to install Roc

1. Download the latest nightly from the assets [here](https://github.com/roc-lang/roc/releases).

1. To prevent "roc can't be opened because Apple can't check it...":

    ```sh
    xattr -d com.apple.quarantine roc_nightly-macos_x86_64-<VERSION>.tar.gz
    ```

1. Untar the archive:

    ```sh
    roc_nightly-macos_x86_64-<VERSION>.tar.gz
    ```

## How to install `examples/` dependencies

1. Install the Rust compiler, for examples with Rust-based platforms:

    ```sh
    curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh
    ```

1. Install the Zig compiler, for examples with Zig-based platforms:

    ```sh
    brew install zig
    ```

1. Run examples:

    ```sh
    # Note: If you installed rust in this terminal session, you'll need to open a new one first!
    ./roc examples/platform-switching/rocLovesRust.roc

    ./roc examples/platform-switching/zig-platform/rocLovesZig.roc

    ./roc examples/platform-switching/c-platform/rocLovesC.roc
    ```
