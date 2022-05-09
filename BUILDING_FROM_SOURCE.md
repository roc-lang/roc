# Building the Roc compiler from source

## Using Nix

### On NixOS

[For NixOS only Linux x86_64 is supported for now](https://github.com/rtfeldman/roc/issues/2734).

NixOS users should make use of the nix flake by [enabling nix flakes](https://nixos.wiki/wiki/Flakes). Shell creation can be done by executing `nix develop` from the root of the repo. NixOS users that do not make use of this flake will get stuck on issue #1846.

### On Linux/MacOS x86_64/aarch64

#### Install

Using [nix](https://nixos.org/download.html) is a quick way to get an environment bootstrapped with a single command.

Anyone having trouble installing the proper version of LLVM themselves might also prefer this method.

If you are running ArchLinux or a derivative like Manjaro, you'll need to run `sudo sysctl -w kernel.unprivileged_userns_clone=1` before installing nix.

Install nix:

`curl -L https://nixos.org/nix/install | sh`

You will need to start a fresh terminal session to use nix.

#### Usage

Now with nix installed, you just need to run one command:

`nix-shell`

> This may not output anything for a little while. This is normal, hang in there. Also make sure you are in the roc project root.

> Also, if you're on NixOS you'll need to enable opengl at the system-wide level. You can do this in configuration.nix with `hardware.opengl.enable = true;`. If you don't do this, nix-shell will fail!

You should be in a shell with everything needed to build already installed.
Use `cargo run help` to see all subcommands.
To use the `repl` subcommand, execute `cargo run repl`.
Use `cargo build` to build the whole project.

> When using `nix-shell`, make sure that if you start `nix-shell` and then run `echo "$PATH" | tr ':' '\n'`, you see the `usr/bin` path listed after all the `/nix/…` paths. Otherwise you might get some nasty rust compilation errors!

#### Extra tips

If you plan on using `nix-shell` regularly, check out [direnv](https://direnv.net/) and [lorri](https://github.com/nix-community/lorri). Whenever you `cd` into `roc/`, they will automatically load the Nix dependencies into your current shell, so you never have to run nix-shell directly!

### Editor

The editor is a :construction:WIP:construction: and not ready yet to replace your favorite editor, although if you want to try it out on nix, read on.
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

## Manual Install

To build the compiler, you need these installed:

* [Zig](https://ziglang.org/), see below for version
* `libxkbcommon` - macOS seems to have it already; on Ubuntu or Debian you can get it with `apt-get install libxkbcommon-dev`
* On Debian/Ubuntu `sudo apt-get install pkg-config`
* LLVM, see below for version

To run the test suite (via `cargo test`), you additionally need to install:

* [`valgrind`](https://www.valgrind.org/) (needs special treatment to [install on macOS](https://stackoverflow.com/a/61359781)
Alternatively, you can use `cargo test --no-fail-fast` or `cargo test -p specific_tests` to skip over the valgrind failures & tests.

For debugging LLVM IR, we use [DebugIR](https://github.com/vaivaswatha/debugir). This dependency is only required to build with the `--debug` flag, and for normal development you should be fine without it.

### libxcb libraries

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
**version: 0.9.1**

For any OS, you can use [`zigup`](https://github.com/marler8997/zigup) to manage zig installations.

If you prefer a package manager, you can try the following:
- For MacOS, you can install with `brew install zig`
- For, Ubuntu, you can use Snap, you can install with `snap install zig --classic --beta`
- For other systems, checkout this [page](https://github.com/ziglang/zig/wiki/Install-Zig-from-a-Package-Manager)

If you want to install it manually, you can also download Zig directly [here](https://ziglang.org/download/). Just make sure you download the right version, the bleeding edge master build is the first download link on this page.

### LLVM
**version: 13.0.x**

For macOS, you can install LLVM 13 using `brew install llvm@13` and then adding
`$(brew --prefix llvm@13)/bin` to your `PATH`. You can confirm this worked by
running `llc --version` - it should mention "LLVM version 13.0.0" at the top.
You may also need to manually specify a prefix env var like so:
```
export LLVM_SYS_130_PREFIX=/usr/local/opt/llvm@13
```

For Ubuntu and Debian:
```
sudo apt -y install lsb-release software-properties-common gnupg
wget https://apt.llvm.org/llvm.sh
chmod +x llvm.sh
./llvm.sh 13
```

If you use this script, you'll need to add `clang` to your `PATH`.
By default, the script installs it as `clang-13`. You can address this with symlinks like so:

```
sudo ln -s /usr/bin/clang-13 /usr/bin/clang
```

There are also alternative installation options at http://releases.llvm.org/download.html

[Troubleshooting](#troubleshooting)

### Building

Use `cargo build` to build the whole project.
Use `cargo run help` to see all subcommands.
To use the `repl` subcommand, execute `cargo run repl`.

### LLVM installation on Linux

For a current list of all dependency versions and their names in apt, see the Earthfile.

On some Linux systems we've seen the error "failed to run custom build command for x11".
On Ubuntu, running `sudo apt install pkg-config cmake libx11-dev` fixed this.

If you encounter `cannot find -lz` run `sudo apt install zlib1g-dev`.

If you encounter:
```
error: No suitable version of LLVM was found system-wide or pointed
       to by LLVM_SYS_130_PREFIX.
```
Add `export LLVM_SYS_130_PREFIX=/usr/lib/llvm-13` to your `~/.bashrc` or equivalent file for your shell.

### LLVM installation on macOS

If installing LLVM fails, it might help to run `sudo xcode-select -r` before installing again.

It might also be useful to add these exports to your shell:

```
export LDFLAGS="-L/usr/local/opt/llvm/lib -Wl,-rpath,/usr/local/opt/llvm/lib"
export CPPFLAGS="-I/usr/local/opt/llvm/include"
```

### LLVM installation on Windows

**Warning** While `cargo build` works on windows, linking roc programs does not yet, see issue #2608. This also means the repl, the editor and many tests will not work on windows.
Installing LLVM's prebuilt binaries doesn't seem to be enough for the `llvm-sys` crate that Roc depends on, so I had to follow the steps below:

1. I downloaded and installed [Build Tools for Visual Studio 2019](https://visualstudio.microsoft.com/thank-you-downloading-visual-studio/?sku=BuildTools&rel=16) (a full Visual Studio install should work too; the Build Tools are just the CLI tools, which is all I wanted)
1. Download the custom LLVM 7z archive [here](https://github.com/PLC-lang/llvm-package-windows/releases/tag/v13.0.1).
1. [Download 7-zip](https://www.7-zip.org/) to be able to extract this archive.
1. Extract the 7z file to where you want to permanently keep the folder.
1. In powershell, set the `LLVM_SYS_130_PREFIX` environment variable (check [here](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.core/about/about_environment_variables?view=powershell-7.2#saving-changes-to-environment-variables) to make this a permanent environment variable):
```
[Environment]::SetEnvironmentVariable(
   "Path",
   [Environment]::GetEnvironmentVariable("Path", "User") + ";C:\Users\anton\Downloads\LLVM-13.0.1-win64\bin",
   "User"
)
```


Once all that was done, `cargo build` ran successfully for Roc!

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

Then install `lld` version 13 (e.g. with `$ sudo apt-get install lld-13`)
and add make sure there's a `ld.lld` executable on your `PATH` which
is symlinked to `lld-13`.

That's it! Enjoy the faster builds.
