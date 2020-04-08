# Building the Roc compiler from source


## Installing LLVM

To build the compiler, you need a particular version of LLVM installed on your system.

To see which version of LLVM you need, take a look at `Cargo.toml`, in particular the `branch` section of the `inkwell` dependency. It should have something like `llvmX-Y` where X and Y are the major and minor revisions of LLVM you need.

For Ubuntu, I used the `Automatic installation script` at [apt.llvm.org](https://apt.llvm.org) - but there are plenty of alternative options at http://releases.llvm.org/download.html

## Use LLD for the linker

Using [`lld` for Rust's linker](https://github.com/rust-lang/rust/issues/39915#issuecomment-538049306)
makes build times a lot faster, and I highly recommend it.

Create `~/.config/cargo` and add this to it:

```
[build]
# Link with lld, per https://github.com/rust-lang/rust/issues/39915#issuecomment-538049306
# Use target-cpu=native, per https://deterministic.space/high-performance-rust.html
rustflags = ["-C", "link-arg=-fuse-ld=lld", "-C", "target-cpu=native"]
```

Then install `lld` version 9 (e.g. with `$ sudo apt-get install lld-9`)
and add make sure there's a `ld.lld` executable on your `PATH` which
is symlinked to `lld-9`.

That's it! Enjoy the faster builds.
