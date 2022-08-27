0. Download the latest nightly from the assets [here](https://github.com/roc-lang/roc/releases).
0. Untar the archive:
    ```
    tar -xf roc_nightly-linux_x86_64-<VERSION>.tar.gz
    ```
0. To be able to run examples:
    - for the Rust example:
    ```
    curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh
    ```
    - for the zig example:
    ```
    wget https://ziglang.org/download/0.9.1/zig-linux-x86_64-0.9.1.tar.xz
    tar -xf zig-linux-x86_64-0.9.1.tar.xz
    sudo ln -s  $(pwd)/zig-linux-x86_64-0.9.1/zig /usr/local/bin/zig
    ```
    - for the C example:
    ```
    # On a Debian-based distro like Ubuntu
    sudo apt update && sudo apt install build-essential clang
    
    # On an RPM-based distro like Fedora
    sudo dnf check-update && sudo dnf install clang
    ```
0. Run examples with:
    ```
    # Rust. If you installed rust in this terminal you'll need to open a new one first!
    ./roc examples/platform-switching/rust-platform/rocLovesRust.roc
    # Zig
    ./roc examples/platform-switching/zig-platform/rocLovesZig.roc
    # C
    ./roc examples/platform-switching/c-platform/rocLovesC.roc
    ```
0. See [here](../README.md#examples) for the other examples.
