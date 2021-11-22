1. Download the latest nightly from the assets [here](https://github.com/rtfeldman/roc/releases).
2. Untar the archive:
    ```
    tar -xf roc_nightly-linux_x86_64-<VERSION>.tar.gz
    ```
3. To be able to run examples:
    - for the Rust example:
    ```
    curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh
    ```
    - for the zig example:
    ```
    wget https://ziglang.org/download/0.8.1/zig-linux-x86_64-0.8.1.tar.xz
    tar -xf zig-linux-x86_64-0.8.1.tar.xz
    sudo ln -s  $(pwd)/zig-linux-x86_64-0.8.1/zig /usr/bin/zig
    ```
    - for the C example:
    ```
    sudo apt install clang-12
    sudo ln -s /usr/bin/clang-12 /usr/bin/clang
    ```
4. Run examples with:
    ```
    # Rust
    ./roc examples/hello-rust/Hello.roc
    # Zig
    ./roc examples/hello-zig/Hello.roc
    # C
    ./roc examples/hello-world/Hello.roc
    ```
5. See [here](../README.md#examples) for the other examples.
