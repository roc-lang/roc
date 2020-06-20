# Building the Roc compiler from source


## Installing LLVM

To build the compiler, you need a particular version of LLVM installed on your system.

To see which version of LLVM you need, take a look at `Cargo.toml`, in particular the `branch` section of the `inkwell` dependency. It should have something like `llvmX-Y` where X and Y are the major and minor revisions of LLVM you need.

For Ubuntu, I used the `Automatic installation script` at [apt.llvm.org](https://apt.llvm.org) - but there are plenty of alternative options at http://releases.llvm.org/download.html

### Troubleshooting LLVM installation on Linux

On some Linux systems we've seen the error "failed to run custom build command for x11".
On Ubuntu, running `sudo apt-get install cmake libx11-dev` fixed this.

### Troubleshooting LLVM installation on Windows

Installing LLVM's prebuilt binaries doesn't seem to be enough for the `llvm-sys` crate that Roc depends on, so I had to build LLVM from source
on Windows. After lots of help from [**@IanMacKenzie**](https://github.com/IanMacKenzie) (thank you, Ian!), here's what worked for me:

1. I downloaded and installed [Build Tools for Visual Studio 2019](https://visualstudio.microsoft.com/thank-you-downloading-visual-studio/?sku=BuildTools&rel=16) (a full Visual Studio install should work tool; the Build Tools are just the CLI tools, which is all I wanted)
2. In the installation configuration, under "additional components" I had to check both "C++ ATL for latest v142 build tools (x86 & x64)" and also "C++/CLI support for v142 build tools"
3. I launched the "x64 Native Tools Command Prompt for Visual Studio 2019" application (note: not the similarly-named "x86" one!)
4. I followed most of the steps under LLVM's [building from source instructions](https://github.com/llvm/llvm-project#getting-the-source-code-and-building-llvm) up to the `cmake -G ...` command, which didn't work for me. Instead, at that point I did the following step.
5. I ran `cmake -G "NMake Makefiles" -DCMAKE_BUILD_TYPE=Release ../llvm` to generate a NMake makefile.
6. Once that completed, I ran `nmake` to build LLVM. (This took about 2 hours on my laptop.)
7. Finally, I set an environment variable `LLVM_SYS_100_PREFIX` to point to the `build` directory where I ran the `cmake` command.

Once all that was done, `cargo` ran successfully for Roc!

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
