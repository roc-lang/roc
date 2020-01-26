# Building the Roc compiler from source


## Installing LLVM

To build the compiler, you need a particular version of LLVM installed on your system.

To see which version of LLVM you need, take a look at `Cargo.toml`, in particular the `branch` section of the `inkwell` dependency. It should have something like `llvmX-Y` where X and Y are the major and minor revisions of LLVM you need.

For Ubuntu, I used the `Automatic installation script` at [apt.llvm.org](https://apt.llvm.org) - but there are plenty of alternative options at http://releases.llvm.org/download.html

You may run into an error like this:
```
    Updating git repository `https://github.com/TheDan64/inkwell`
error: failed to load source for a dependency on `inkwell`

Caused by:
  Unable to update https://github.com/TheDan64/inkwell?branch=llvm8-0#d0f5c1e1

Caused by:
  revspec 'd0f5c1e198853bc06d8427fbafb7b068032d1d1a' not found; class=Reference (4); code=NotFound (-3)
```

This seems to be caused by cargo being out of date (even if it's freshly installed), and can be fixed with `cargo update`. 
