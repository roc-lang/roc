# Building the Roc compiler from source


## Installing LLVM, Zig, valgrind, and Python 2.7

To build the compiler, you need these installed:

* Python 2.7 (Windows only), `python-is-python3` (Ubuntu)
* [Zig](https://ziglang.org/), see below for version
* `libxkbcommon` - macOS seems to have it already; on Ubuntu or Debian you can get it with `apt-get install libxkbcommon-dev`
* On Debian/Ubuntu `sudo apt-get install pkg-config`
* LLVM, see below for version

To run the test suite (via `cargo test`), you additionally need to install:

* [`valgrind`](https://www.valgrind.org/) (needs special treatment to [install on macOS](https://stackoverflow.com/a/61359781)
Alternatively, you can use `cargo test --no-fail-fast` or `cargo test -p specific_tests` to skip over the valgrind failures & tests.

For debugging LLVM IR, we use [DebugIR](https://github.com/vaivaswatha/debugir). This dependency is only required to build with the `--debug` flag, and for normal developtment you should be fine without it. 

### libcxb libraries

You may see an error like this during builds:

```
/usr/bin/ld: cannot find -lxcb-render
/usr/bin/ld: cannot find -lxcb-shape
/usr/bin/ld: cannot find -lxcb-xfixes
```

If so, you can fix it like so:

```
sudo apt-get install libxcb-render0-dev libxcb-shape0-dev libxcb-xfixes0-dev
```

### Zig
**version: 0.8.0**

For any OS, you can use [`zigup`](https://github.com/marler8997/zigup) to manage zig installations.

If you prefer a package manager, you can try the following:
- For MacOS, you can install with `brew install zig`
- For, Ubuntu, you can use Snap, you can install with `snap install zig --classic --beta`
- For other systems, checkout this [page](https://github.com/ziglang/zig/wiki/Install-Zig-from-a-Package-Manager)

If you want to install it manually, you can also download Zig directly [here](https://ziglang.org/download/). Just make sure you download the right version, the bleeding edge master build is the first download link on this page.

### LLVM
**version: 12.0.x**

For macOS, you can install LLVM 12 using `brew install llvm@12` and then adding
`/usr/local/opt/llvm@12/bin` to your `PATH`. You can confirm this worked by
running `llc --version` - it should mention "LLVM version 12.0.0" at the top.
You may also need to manually specify a prefix env var like so:
```
export LLVM_SYS_120_PREFIX=/usr/local/opt/llvm@12
```

For Ubuntu and Debian:
```
sudo apt -y install lsb-release software-properties-common gnupg
wget https://apt.llvm.org/llvm.sh
chmod +x llvm.sh
./llvm.sh 12
```

If you use this script, you'll need to add `clang` and `llvm-as` to your `PATH`.
By default, the script installs them as `clang-12` and `llvm-as-12`,
respectively. You can address this with symlinks like so:

```
sudo ln -s /usr/bin/clang-12 /usr/bin/clang
```
```
sudo ln -s /usr/bin/llvm-as-12 /usr/bin/llvm-as
````

There are also alternative installation options at http://releases.llvm.org/download.html

[Troubleshooting](#troubleshooting)

## Using Nix

### Install

Using [nix](https://nixos.org/download.html) is a quick way to get an environment bootstrapped with a single command.

Anyone having trouble installing the proper version of LLVM themselves might also prefer this method.

If you are running ArchLinux or a derivative like Manjaro, you'll need to run `sudo sysctl -w kernel.unprivileged_userns_clone=1` before installing nix.

Install nix:

`curl -L https://nixos.org/nix/install | sh`

If you're on MacOS and using a OS version >= 10.15:

`sh <(curl -L https://nixos.org/nix/install) --darwin-use-unencrypted-nix-store-volume`

You may prefer to setup up the volume manually by following nix documentation.

> You may need to restart your terminal

### Usage

Now with nix installed, you just need to run one command:

`nix-shell`

> This may not output anything for a little while. This is normal, hang in there. Also make sure you are in the roc project root.

> Also, if you're on NixOS you'll need to enable opengl at the system-wide level. You can do this in configuration.nix with `hardware.opengl.enable = true;`. If you don't do this, nix-shell will fail!

You should be in a shell with everything needed to build already installed. Next run:

`cargo run repl`

You should be in a repl now. Have fun!

### Extra tips

If you plan on using `nix-shell` regularly, check out [direnv](https://direnv.net/) and [lorri](https://github.com/nix-community/lorri). Whenever you `cd` into `roc/`, they will automatically load the Nix dependencies into your current shell, so you never have to run nix-shell directly!

### Editor

The editor is a WIP and not ready yet to replace your favorite editor, although if you want to try it out on nix, read on.
`cargo run edit` should work from NixOS, if you use a nix-shell from inside another OS, follow the instructions below.

#### Nvidia GPU

Outside of a nix shell, execute the following:
```
nix-channel --add https://github.com/guibou/nixGL/archive/main.tar.gz nixgl && nix-channel --update
nix-env -iA nixgl.auto.nixVulkanNvidia
```
Running the editor does not work with `nix-shell --pure`.
```
nix-shell
```
460.91.03 may be different for you, type nixVulkanNvidia and press tab to autocomplete for your version.
```
nixVulkanNvidia-460.91.03 cargo run edit
```

#### Integrated Intel Graphics

:exclamation: ** Our Nix setup currently cannot run the editor with integrated intel graphics, see #1856 ** :exclamation:

Outside of a nix shell, run:

```bash
git clone https://github.com/guibou/nixGL
cd nixGL
nix-env -f ./ -iA nixVulkanIntel
```

cd to the roc repo, and run (without --pure):
```
nix-shell
nixVulkanIntel cargo run edit
```

#### Other configs

Check the [nixGL repo](https://github.com/guibou/nixGL) for other graphics configurations.

## Troubleshooting

Create an issue if you run into problems not listed here.
That will help us improve this document for everyone who reads it in the future!

### LLVM installation on Linux

For a current list of all dependency versions and their names in apt, see the Earthfile.

On some Linux systems we've seen the error "failed to run custom build command for x11".
On Ubuntu, running `sudo apt install pkg-config cmake libx11-dev` fixed this.

If you encounter `cannot find -lz` run `sudo apt install zlib1g-dev`.

If you encounter:
```
error: No suitable version of LLVM was found system-wide or pointed
       to by LLVM_SYS_120_PREFIX.
```
Add `export LLVM_SYS_120_PREFIX=/usr/lib/llvm-12` to your `~/.bashrc` or equivalent file for your shell.

### LLVM installation on macOS

If installing LLVM fails, it might help to run `sudo xcode-select -r` before installing again.

It might also be useful to add these exports to your shell:

```
export LDFLAGS="-L/usr/local/opt/llvm/lib -Wl,-rpath,/usr/local/opt/llvm/lib"
export CPPFLAGS="-I/usr/local/opt/llvm/include"
```

### LLVM installation on Windows

Installing LLVM's prebuilt binaries doesn't seem to be enough for the `llvm-sys` crate that Roc depends on, so I had to build LLVM from source
on Windows. After lots of help from [**@IanMacKenzie**](https://github.com/IanMacKenzie) (thank you, Ian!), here's what worked for me:

1. I downloaded and installed [Build Tools for Visual Studio 2019](https://visualstudio.microsoft.com/thank-you-downloading-visual-studio/?sku=BuildTools&rel=16) (a full Visual Studio install should work tool; the Build Tools are just the CLI tools, which is all I wanted)
1. In the installation configuration, under "additional components" I had to check both "C++ ATL for latest v142 build tools (x86 & x64)" and also "C++/CLI support for v142 build tools" [note: as of September 2021 this should no longer be necessary - the next time anyone tries this, please try it without this step and make a PR to delete this step if it's no longer needed!]
1. I launched the "x64 Native Tools Command Prompt for Visual Studio 2019" application (note: not the similarly-named "x86" one!)
1. Make sure [Python 2.7](https://www.python.org/) and [CMake 3.17](http://cmake.org/) are installed on your system.
1. I followed most of the steps under LLVM's [building from source instructions](https://github.com/llvm/llvm-project#getting-the-source-code-and-building-llvm) up to the `cmake -G ...` command, which didn't work for me. Instead, at that point I did the following step.
1. I ran `cmake -G "NMake Makefiles" -DCMAKE_BUILD_TYPE=Release ../llvm` to generate a NMake makefile.
1. Once that completed, I ran `nmake` to build LLVM. (This took about 2 hours on my laptop.)
1. Finally, I set an environment variable `LLVM_SYS_100_PREFIX` to point to the `build` directory where I ran the `cmake` command.


Once all that was done, `cargo` ran successfully for Roc!

### Build speed on WSL/WSL2

If your Roc project folder is in the Windows filesystem but you're compiling from Linux, rebuilds may be as much as 20x slower than they should be!
Disk access during linking seems to be the bottleneck. It's recommended to move your folder to the Linux filesystem.

## Use LLD for the linker

Using [`lld` for Rust's linker](https://github.com/rust-lang/rust/issues/39915#issuecomment-538049306)
makes build times a lot faster, and I highly recommend it.

Create `~/.cargo/config.toml` if it does not exist and add this to it:

```
[build]
# Link with lld, per https://github.com/rust-lang/rust/issues/39915#issuecomment-538049306
# Use target-cpu=native, per https://deterministic.space/high-performance-rust.html
rustflags = ["-C", "link-arg=-fuse-ld=lld", "-C", "target-cpu=native"]
```

Then install `lld` version 12 (e.g. with `$ sudo apt-get install lld-12`)
and add make sure there's a `ld.lld` executable on your `PATH` which
is symlinked to `lld-12`.

That's it! Enjoy the faster builds.
