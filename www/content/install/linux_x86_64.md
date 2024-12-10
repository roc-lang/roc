# Linux x86_64

## How to install Roc

In order to develop in Roc, you need to install the Roc CLI,
which includes the Roc compiler and some helpful utilities.

1. Download the latest nightly from the assets [here](https://github.com/roc-lang/roc/releases).

1. Untar the archive:

    ```sh
    tar -xf roc_nightly-linux_x86_64-latest.tar.gz
    cd roc_night<TAB TO AUTOCOMPLETE>
    ```

1. To be able to run the `roc` command anywhere on your system; add the line below to your shell startup script (.profile, .zshrc, ...):

    ```sh
    export PATH=$PATH:~/path/to/roc_nightly-linux_x86_64-<VERSION>
    ```

1. Check everything worked by executing `roc version`

1. Install libc development files and ld (binutils) if you'd like to use the repl (`roc repl`):
   - On Ubuntu/Debian:

    ```sh
    sudo apt install libc-dev binutils
    ```

   - On ArchLinux/Manjaro:

   ```sh
   sudo pacman -S glibc binutils
   ```

   - on Fedora:

   ```sh
   sudo dnf install glibc-devel binutils
   ```

## How to install Roc platform dependencies

This step is not necessary if you only want to use the [basic-cli platform](https://github.com/roc-lang/basic-cli), like in the tutorial.
But, if you want to compile Roc apps with other platforms (either in [`examples/`](https://github.com/roc-lang/roc/tree/main/examples) or in your own projects),
you'll need to install one or more of these platform languages too.

1. Install the Rust compiler, for apps with Rust-based platforms:

    ```sh
    curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh
    ```

1. For Zig-based platforms: [download zig 0.13.0](https://ziglang.org/download/0.13.0/zig-linux-x86_64-0.13.0.tar.xz), extract the archive and add export PATH=$PATH:~/path/to/zig to your shell startup script (.profile, .zshrc, …).

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

## Next Steps

- [editor setup](https://www.roc-lang.org/install#editor-extensions)
- [tutorial](https://www.roc-lang.org/tutorial)
- [examples](https://www.roc-lang.org/examples)
