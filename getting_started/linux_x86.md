# Roc installation guide for x86 Linux systems

## How to install Roc

In order to develop in Roc, you need to install the Roc CLI,
which includes the Roc compiler and various helpful utilities.

1. Download the latest nightly from the assets [here](https://github.com/roc-lang/roc/releases).

1. Untar the archive:

    ```sh
    tar -xf roc_nightly-linux_x86_64-<VERSION>.tar.gz --one-top-level
    cd roc_night<TAB TO AUTOCOMPLETE>
    ```

## How to install Roc platform dependencies

In order to compile Roc apps (either in `examples/` or in your own projects),
you need to install one or more of these platform language compilers, too.

1. Install the Rust compiler, for apps with Rust-based platforms:

    ```sh
    curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh
    ```

1. Install the Zig compiler, for apps with Zig-based platforms:

    ```sh
    wget https://ziglang.org/download/0.9.1/zig-linux-x86_64-0.9.1.tar.xz
    tar -xf zig-linux-x86_64-0.9.1.tar.xz
    sudo ln -s  $(pwd)/zig-linux-x86_64-0.9.1/zig /usr/local/bin/zig
    ```

1. Install a C compiler, for apps with C-based platforms:

    ```sh
    # On a Debian-based distro like Ubuntu
    sudo apt update && sudo apt install build-essential clang
    
    # On an RPM-based distro like Fedora
    sudo dnf check-update && sudo dnf install clang
    ```

    ```sh
    # Note: If you installed Rust in this terminal session, you'll need to open a new one first!
    ./roc examples/platform-switching/rocLovesRust.roc

    ./roc examples/platform-switching/rocLovesZig.roc

    ./roc examples/platform-switching/rocLovesC.roc
    ```
