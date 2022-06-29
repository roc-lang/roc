0. Download the latest nightly from the assets [here](https://github.com/rtfeldman/roc/releases).
0. To prevent "roc can't be opened because Apple can't check it...":
    ```
    xattr -d com.apple.quarantine roc_nightly-macos_x86_64-<VERSION>.tar.gz 
    ```
0. Untar the archive:
    ```
    roc_nightly-macos_x86_64-<VERSION>.tar.gz 
    ```
0. To be able to run examples:
    - for the Rust example:
    ```
    curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh
    ```
    - for the zig example:
    ```
    brew install zig
    ```
0. Run examples with:
    ```
    # Rust. If you installed rust in this terminal you'll need to open a new one first!
    ./roc examples/hello-world/rust-platform/helloRust.roc
    # Zig
    ./roc examples/hello-world/zig-platform/helloZig.roc
    ```
0. See [here](../README.md#examples) for the other examples.