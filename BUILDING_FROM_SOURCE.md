# Building the Roc compiler from source

If you run into any problems getting Roc built from source, please ask for help in the `#beginners` channel on [Roc Zulip](https://roc.zulipchat.com) (the fastest way), or create an issue in this repo!

## Using Devcontainer

### Codespaces
To access the browser-based editor, you can change the URL of your repository to https://github.dev/roc-lang/roc, replacing github.com with github.dev or go to the roc repo on https://github.com/roc-lang/roc and press . (period key).

Or use it locally in [VSCode](https://code.visualstudio.com/docs/remote/codespaces)

### Docker

Or even run it in your local Docker environment [Developing inside a Container](https://code.visualstudio.com/docs/devcontainers/containers)

## Using Nix

On MacOS and Linux, we highly recommend using [nix](https://nixos.org/download.html) to quickly install all dependencies necessary to build roc.

:warning: If you tried to run `cargo` in the repo folder before installing nix, make sure to execute `cargo clean` first. To prevent you from executing `cargo` outside of nix, tools like [direnv](https://github.com/nix-community/nix-direnv) and [lorri](https://github.com/nix-community/lorri) can put you in a nix shell automatically when you `cd` into the directory.

### On Linux x86_64 or MacOS aarch64/arm64/x86_64

#### Installing Nix

If you are running ArchLinux or a derivative like Manjaro, you'll need to run `sudo sysctl -w kernel.unprivileged_userns_clone=1` before installing nix.

Install nix (not necessary on NixOS):

- If you are using WSL (Windows subsystem for Linux):

```sh
sh <(curl -L https://nixos.org/nix/install) --no-daemon
```

- For everything else:

```sh
sh <(curl -L https://nixos.org/nix/install) --daemon
```

Open a new terminal and edit either `~/.config/nix/nix.conf` or `/etc/nix/nix.conf` and add:

```text
experimental-features = nix-command flakes
```

If Nix was installed in multi-user mode, make sure to restart the nix-daemon.
If you don't know how to do this, restarting your computer will also do the job.

#### Usage

Now with nix set up, you just need to run one command from the roc project root directory:

```sh
nix develop
```

You should be in a shell with everything needed to build already installed.
Use `cargo run help` to see all subcommands.
To use the `repl` subcommand, execute `cargo run repl`.
Use `cargo build` to build the whole project.

Read the instructions [here](devtools/README.md) to make nix work well with your development tools (vscode, vim, rust-analyzer...)

#### Extra tips

If you want to load all dependencies automatically whenever you `cd` into `roc`, check out [direnv](https://direnv.net/).
Then you will no longer need to execute `nix develop` first.

## Troubleshooting

Create an issue if you run into problems not listed here.
That will help us improve this document for everyone who reads it in the future!

## Manual Install

To build the compiler, you need these installed:

- [Zig](https://ziglang.org/), see below for version
- On Debian/Ubuntu `sudo apt-get install pkg-config`
- LLVM, see below for version
- [rust](https://rustup.rs/)

To run the test suite (via `cargo test`), you additionally need to install:

- [`valgrind`](https://www.valgrind.org/) (needs special treatment to [install on macOS](https://stackoverflow.com/a/61359781)
Alternatively, you can use `cargo test --no-fail-fast` or `cargo test -p specific_tests` to skip over the valgrind failures & tests.

For emitting LLVM IR for debugging purposes, the `--emit-llvm-ir` flag can be used.

### libz libzstd libraries

You may see an error like this during builds:

```text
/usr/bin/ld: cannot find -lz: No such file or directory
/usr/bin/ld: cannot find -lzstd: No such file or directory
```

If so, you can fix it like so:

```sh
sudo apt-get install libz-dev libzstd-dev
```

#### Macos zstd not found

If you still hit `ld: library 'zstd' not found` even after doing `brew install zstd z3`,
add these lines to `.cargo/config.toml`:
```
[target.aarch64-apple-darwin]
rustflags = ["-C", "link-args=-L/opt/homebrew/lib"]
```

### Zig

**version: 0.13.0**

For any OS, you can use [`zigup`](https://github.com/marler8997/zigup) to manage zig installations.

If you prefer a package manager, you can try the following:

- MacOS: `brew install zig`
- Systems with snap (such as Ubuntu): `snap install zig --classic --beta`
- Other systems: refer to the [zig documentation](https://github.com/ziglang/zig/wiki/Install-Zig-from-a-Package-Manager)

If you want to install it manually, you can [download the binary](https://ziglang.org/download/#release-0.13.0) and place it on your PATH.
Apart from the binary, the archive contains a `lib` folder, which needs to be copied next to the binary.

> WINDOWS NOTE: when you unpack the Zig archive on windows, the result is nested in an extra directory. The instructions on the zig website will seem to not work. So, double-check that the path to zig executable does not include the same directory name twice.

### LLVM

**version: 18.0.x**

See below for operating system specific installation instructions.

### Building

Use `cargo build` to build the whole project.
Use `cargo run help` to see all subcommands.
To use the `repl` subcommand, execute `cargo run repl`.

The default is a developer build. For an optimized build, use:

```sh
cargo build --release --bin roc
```

### LLVM installation on Linux

For Ubuntu and Debian:

```sh
sudo apt -y install lsb-release software-properties-common gnupg
wget https://apt.llvm.org/llvm.sh
chmod +x llvm.sh
./llvm.sh 18
```

If you use this script, you'll need to add `clang` to your `PATH`.
By default, the script installs it as `clang-18`. You can address this with symlinks like so:

```sh
sudo ln -s /usr/bin/clang-18 /usr/bin/clang
```

There are also alternative installation options at <http://releases.llvm.org/download.html>

[Troubleshooting](#troubleshooting)

For Fedora:

```sh
sudo dnf install llvm18 llvm18-devel
```

#### LLVM Linux troubleshooting

On some Linux systems we've seen the error "failed to run custom build command for x11".
On Ubuntu, running `sudo apt install pkg-config cmake libx11-dev` fixed this.

If you encounter `cannot find -lz` run `sudo apt install zlib1g-dev`.

If you encounter:

```text
error: No suitable version of LLVM was found system-wide or pointed
       to by LLVM_SYS_180_PREFIX.
```

Add `export LLVM_SYS_180_PREFIX=/usr/lib/llvm-18` to your `~/.bashrc` or equivalent file for your shell.

### LLVM installation on MacOS

For macOS, you can install LLVM 18 using `brew install llvm@18` and then adding
`$(brew --prefix llvm@18)/bin` to your `PATH`. You can confirm this worked by
running `llc --version` - it should mention "LLVM version 18.x.x" at the top.
You may also need to manually specify a prefix env var like so:

```sh
export LLVM_SYS_180_PREFIX=$(brew --prefix llvm@18)
```

#### LLVM MacOS troubleshooting

If installing LLVM fails, it might help to run `sudo xcode-select -r` before installing again.

It might also be useful to add these exports to your shell:

```sh
export LDFLAGS="-L/usr/local/opt/llvm/lib -Wl,-rpath,/usr/local/opt/llvm/lib"
export CPPFLAGS="-I/usr/local/opt/llvm/include"
```

### LLVM installation on Windows

**Warning** While `cargo build` works on windows, linking roc programs does not yet, see issue #2608. This also means the repl, and many tests will not work on windows.
The official LLVM pre-built binaries for Windows lack features that roc needs. Instead:

1. Download the custom LLVM 7z archive [here](https://github.com/roc-lang/llvm-package-windows/releases/download/v18.1.8/LLVM-18.1.8-win64.7z).
1. [Download 7-zip](https://www.7-zip.org/) to be able to extract this archive.
1. Extract the 7z file to where you want to permanently keep the folder. We recommend you pick a path without any spaces in it.
1. In powershell, set the `LLVM_SYS_180_PREFIX` environment variable (check [here](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.core/about/about_environment_variables?view=powershell-7.2#saving-environment-variables-with-the-system-control-panel) to make this a permanent environment variable):

```text
<# ! Replace YOUR_USERNAME ! #>
$env:LLVM_SYS_180_PREFIX = 'C:\Users\YOUR_USERNAME\Downloads\LLVM-18.1.8-win64'
```

Once all that was done, `cargo build` ran successfully for Roc!

#### LLVM Windows troubleshooting

If you see the build failing because some internal file is not available, it might be your anti-virus program. Cargo's behavior is kind of similar to a virus (downloading files from the internet, creating many files), and this has been known to cause problems.

### Build speed on WSL/WSL2

If your Roc project folder is in the Windows filesystem but you're compiling from Linux, rebuilds may be as much as 20x slower than they should be!
Disk access during linking seems to be the bottleneck. It's recommended to move your folder to the Linux filesystem.

## Use LLD for the linker

Using [`lld` for Rust's linker](https://github.com/rust-lang/rust/issues/39915#issuecomment-538049306)
makes build times a lot faster, and I highly recommend it.

Create `~/.cargo/config.toml` if it does not exist and add this to it:

```toml
[build]
# Link with lld, per https://github.com/rust-lang/rust/issues/39915#issuecomment-538049306
# Use target-cpu=native, per https://deterministic.space/high-performance-rust.html
rustflags = ["-C", "link-arg=-fuse-ld=lld", "-C", "target-cpu=native"]
```

Then install `lld` version 18 (e.g. with `$ sudo apt-get install lld-18`)
and add make sure there's a `ld.lld` executable on your `PATH` which
is symlinked to `lld-18`.

That's it! Enjoy the faster builds.
