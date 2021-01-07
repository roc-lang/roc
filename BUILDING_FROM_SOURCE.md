# Building the Roc compiler from source


## Installing LLVM, Python, Zig, valgrind, libunwind, and libc++-dev

To build the compiler, you need these installed:

* `libunwind` (macOS should already have this one installed)
* `libc++-dev`
* Python 2.7 (Windows only), `python-is-python3` (Ubuntu)
* [Zig](https://ziglang.org/) 0.7.1 or greater
* a particular version of LLVM (see below)

To run the test suite (via `cargo test`), you additionally need to install:

* [`valgrind`](https://www.valgrind.org/) (needs special treatment to [install on macOS](https://stackoverflow.com/a/61359781)
Alternatively, you can use `cargo test --no-fail-fast` or `cargo test -p specific_tests` to skip over the valgrind failures & tests.

For debugging LLVM IR, we use [DebugIR](https://github.com/vaivaswatha/debugir). This dependency is only required to build with the `--debug` flag, and for normal developtment you should be fine without it. 

### libunwind & libc++-dev

MacOS systems should already have `libunwind`, but other systems will need to install it (On Ubuntu, this can be donw with `sudo apt-get install libunwind-dev`).
Some systems may already have `libc++-dev` on them, but if not, you may need to install it. (On Ubuntu, this can be done with `sudo apt-get install libc++-dev`.)

### Zig

If you're on MacOS, you can install with `brew install zig`
If you're on Ubuntu and use Snap, you can install with `snap install zig --classic --beta`
For any other OS, checkout the [Zig installation page](https://github.com/ziglang/zig/wiki/Install-Zig-from-a-Package-Manager)

### LLVM

To see which version of LLVM you need, take a look at `Cargo.toml`, in particular the `branch` section of the `inkwell` dependency. It should have something like `llvmX-Y` where X and Y are the major and minor revisions of LLVM you need.

For Ubuntu and Debian, you can use the `Automatic installation script` at [apt.llvm.org](https://apt.llvm.org):
```
sudo bash -c "$(wget -O - https://apt.llvm.org/llvm.sh)"
```

For macOS, you can run `brew install llvm` (but before you do so, check the version with `brew info llvm`--if it's 10.0.1, you may need to install a slightly older version. See below for details.)

There are also plenty of alternative options at http://releases.llvm.org/download.html

## Using Nix

### Install

Using [nix](https://nixos.org/download.html) is a quick way to get an environment bootstrapped with a single command.

Anyone having trouble installing the proper version of LLVM themselves might also prefer this method.

First, install nix:

`curl -L https://nixos.org/nix/install | sh`

If MacOS and using a version >= 10.15:

`sh <(curl -L https://nixos.org/nix/install) --darwin-use-unencrypted-nix-store-volume`

You may prefer to setup up the volume manually by following nix documentation.

> You may need to restart your terminal

### Usage

Now with nix installed you just need to run one command:

`nix-shell`

> This may not output anything for a little while. This is normal, hang in there. Also make sure you are in the roc project root.

> Also, if you're on NixOS you'll need to enable opengl at the system-wide level. You can do this in configuration.nix with `hardware.opengl.enable = true;`. If you don't do this, nix-shell will fail!

You should be in a shell with everything needed to build already installed. Next run:

`cargo run repl`

You should be in a repl now. Have fun!

### Editor

When you want to run the editor from Ubuntu inside nix you need to install [nixGL](https://github.com/guibou/nixGL) as well:

```bash
nix-shell
git clone https://github.com/guibou/nixGL
cd nixGL
```

If you have an Nvidia graphics card, run:
```
nix-env -f ./ -iA nixVulkanNvidia
```
If you have integrated Intel graphics, run:
```
nix-env -f ./ -iA nixVulkanIntel
```
Check the [nixGL repo](https://github.com/guibou/nixGL) for other configurations.

Now you should be able to run the editor:
```bash
cd roc
nixVulkanNvidia cargo run edit `# replace Nvidia with the config you chose in the previous step`
```

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
