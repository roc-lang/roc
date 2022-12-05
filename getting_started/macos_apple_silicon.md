# Roc installation guide for Apple silicon systems

## How to install Roc

:warning: We do not yet officially support MacOS 13. But, as long as you are not using a zig or wasm platform most things should work fine.

In order to develop in Roc, you need to install the Roc CLI,
which includes the Roc compiler and various helpful utilities.

1. Download the latest nightly from the assets [here](https://github.com/roc-lang/roc/releases).

1. To prevent "roc can't be opened because Apple can't check it...":

    ```sh
    xattr -d com.apple.quarantine roc_nightly-macos_apple_silicon-<VERSION>.tar.gz
    ```

1. Untar the archive:

    ```sh
    mkdir roc_nightly-macos_apple_silicon-<VERSION>
    tar xf roc_nightly-macos_apple_silicon-<VERSION>.tar.gz --directory roc_nightly-macos_apple_silicon-<VERSION>
    cd roc_night<TAB TO AUTOCOMPLETE>
    ```

1. Install llvm 13:

    ```sh
    brew install llvm@13
    ```

## How to install Roc platform dependencies

In order to compile Roc apps (either in `examples/` or in your own projects),
you need to install one or more of these platform language compilers, too.

1. Install the Rust compiler, for apps with Rust-based platforms:

    ```sh
    curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh
    ```

1. If you'd like to use Zig-based platforms: download [zig 0.9.1](https://ziglang.org/download/0.9.1/zig-macos-aarch64-0.9.1.tar.xz), extract the archive and add `export PATH=$PATH:~/path/to/zig` to your shell startup script (.profile, .zshrc, …). Note: zig 0.9.1 is not available on homebrew.

1. Run examples:

    ```sh
    # Note: If you installed rust in this terminal session, you'll need to open a new one first!
    ./roc examples/platform-switching/rocLovesRust.roc

    ./roc examples/platform-switching/rocLovesZig.roc

    ./roc examples/platform-switching/rocLovesC.roc
    ```
