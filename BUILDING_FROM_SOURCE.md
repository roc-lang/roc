# Building the Roc compiler from source


## Installing LLVM

To build the compiler, you need a particular version of LLVM installed on your system.

To see which version of LLVM you need, take a look at `Cargo.toml`, in particular the `branch` section of the `inkwell` dependency. It should have something like `llvmX-Y` where X and Y are the major and minor revisions of LLVM you need.

For Ubuntu, I used the `Automatic installation script` at [apt.llvm.org](https://apt.llvm.org)
