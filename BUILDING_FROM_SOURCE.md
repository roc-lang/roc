# Building the Roc compiler from source


## Installing LLVM and libc++abi

To build the compiler, you need both `libc++abi` and a particular version of LLVM installed on your system. Some systems may already have `libc++abi` on them, but if not, you may need to install it. (On Ubuntu, this can be done with `apt-get install libc++abi-dev`.)

To see which version of LLVM you need, take a look at `Cargo.toml`, in particular the `branch` section of the `inkwell` dependency. It should have something like `llvmX-Y` where X and Y are the major and minor revisions of LLVM you need.

For Ubuntu and Debian, you can use the `Automatic installation script` at [apt.llvm.org](https://apt.llvm.org):
```
sudo bash -c "$(wget -O - https://apt.llvm.org/llvm.sh)"
```

For macOS, you can run `brew install llvm` (but before you do so, check the version with `brew info llvm`--if it's 10.0.1, you may need to install a slightly older version. See below for details.)

There are also plenty of alternative options at http://releases.llvm.org/download.html

## Troubleshooting

Create an issue if you run into problems not listed here.
That will help us improve this document for everyone who reads it in the future!

### LLVM installation on Linux

On some Linux systems we've seen the error "failed to run custom build command for x11".
On Ubuntu, running `sudo apt install pkg-config cmake libx11-dev` fixed this.

If you encounter `cannot find -lz` run `sudo apt install zlib1g-dev`.

### LLVM installation on macOS

It looks like LLVM 10.0.1 [has some issues with libxml2 on macOS](https://discourse.brew.sh/t/llvm-config-10-0-1-advertise-libxml2-tbd-as-system-libs/8593). You can install the older 10.0.0_3 by doing

```
$ brew install https://raw.githubusercontent.com/Homebrew/homebrew-core/6616d50fb0b24dbe30f5e975210bdad63257f517/Formula/llvm.rb
# "pinning" ensures that homebrew doesn't update it automatically
$ brew pin llvm
```

If that doesn't work and you get a `brew` error `Error: Calling Installation of llvm from a GitHub commit URL is disabled! Use 'brew extract llvm' to stable tap on GitHub instead.` while trying the above solution, you can follow the steps extracting the formula into your private tap (one public version is at `sladwig/tap/llvm`). If installing LLVM still fails, it might help to run `sudo xcode-select -r` before installing again.

### LLVM installation on Windows

Installing LLVM's prebuilt binaries doesn't seem to be enough for the `llvm-sys` crate that Roc depends on, so I had to build LLVM from source
on Windows. After lots of help from [**@IanMacKenzie**](https://github.com/IanMacKenzie) (thank you, Ian!), here's what worked for me:

1. I downloaded and installed [Build Tools for Visual Studio 2019](https://visualstudio.microsoft.com/thank-you-downloading-visual-studio/?sku=BuildTools&rel=16) (a full Visual Studio install should work tool; the Build Tools are just the CLI tools, which is all I wanted)
1. In the installation configuration, under "additional components" I had to check both "C++ ATL for latest v142 build tools (x86 & x64)" and also "C++/CLI support for v142 build tools"
1. I launched the "x64 Native Tools Command Prompt for Visual Studio 2019" application (note: not the similarly-named "x86" one!)
1. Make sure [Python 2.7](https://www.python.org/) and [CMake 3.17](http://cmake.org/) are installed on your system.
1. I followed most of the steps under LLVM's [building from source instructions](https://github.com/llvm/llvm-project#getting-the-source-code-and-building-llvm) up to the `cmake -G ...` command, which didn't work for me. Instead, at that point I did the following step.
1. I ran `cmake -G "NMake Makefiles" -DCMAKE_BUILD_TYPE=Release ../llvm` to generate a NMake makefile.
1. Once that completed, I ran `nmake` to build LLVM. (This took about 2 hours on my laptop.)
1. Finally, I set an environment variable `LLVM_SYS_100_PREFIX` to point to the `build` directory where I ran the `cmake` command.


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
