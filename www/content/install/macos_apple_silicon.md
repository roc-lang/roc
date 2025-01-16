# Apple Silicon

## How to install Roc

In order to develop in Roc, you need to install the Roc CLI,
which includes the Roc compiler and some helpful utilities.

1. Download the latest roc nightly using the terminal:

    ```sh
    curl -OL https://github.com/roc-lang/roc/releases/download/nightly/roc_nightly-macos_apple_silicon-latest.tar.gz
    ```

1. Untar the archive:

    ```sh
    tar xf roc_nightly-macos_apple_silicon-latest.tar.gz
    cd roc_night<TAB TO AUTOCOMPLETE>
    ```

1. Install llvm 18 and z3:

    ```sh
    brew install llvm@18 z3
    ```

1. To be able to run the `roc` command anywhere on your system; add the line below to your shell startup script (.profile, .zshrc, ...):

    ```sh
    export PATH=$PATH:~/path/to/roc_nightly-macos_apple_silicon-<VERSION>
    ```

1. Check everything worked by executing `roc version`


1. Download and run hello world:

    ```sh
    curl -OL https://raw.githubusercontent.com/roc-lang/examples/refs/heads/main/examples/HelloWorld/main.roc
    roc main.roc
    ```

## Next Steps

- [editor setup](https://www.roc-lang.org/install#editor-extensions)
- [tutorial](https://www.roc-lang.org/tutorial)
- [examples](https://www.roc-lang.org/examples)
