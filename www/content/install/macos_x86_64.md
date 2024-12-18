# MacOS x86_64

## How to install Roc

In order to develop in Roc, you need to install the Roc CLI,
which includes the Roc compiler and some helpful utilities.

1. Download the latest roc nightly using the terminal:

    ```sh
    curl -OL https://github.com/roc-lang/roc/releases/download/nightly/roc_nightly-macos_x86_64-latest.tar.gz
    ```

1. Untar the archive:

    ```sh
    tar xf roc_nightly-macos_x86_64-latest.tar.gz
    cd roc_night<TAB TO AUTOCOMPLETE>
    ```

1. To be able to run the `roc` command anywhere on your system; add the line below to your shell startup script (.profile, .zshrc, ...):

    ```sh
    export PATH=$PATH:~/path/to/roc_nightly-macos_x86_64-<VERSION>
    ```

1. Check everything worked by executing `roc version`

1. Run [hello world](https://github.com/roc-lang/examples/blob/main/examples/HelloWorld/main.roc):

    ```sh
    roc main.roc
    ```

## Next Steps

- [editor setup](https://www.roc-lang.org/install#editor-extensions)
- [tutorial](https://www.roc-lang.org/tutorial)
- [examples](https://www.roc-lang.org/examples)
