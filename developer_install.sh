#!/bin/bash

if [[ $EUID -ne 0 ]]; then
   echo "Please run this script as root. Use `sudo ./install.sh` or `su -` followed by `./install.sh`"
   exit 1
fi

apt -y install wget curl build-essential pkg-config libasound2-dev zlib1g-dev libxcb-shape0-dev libxcb-xfixes0-dev libxkbcommon-dev
# libasound2-dev for editor sounds
# libxcb-shape0-dev libxcb-xfixes0-dev for editor clipboard
# libxkbcommon-dev for editor, not sure what exactly :p
# build-essential for cc (for rust)
# zlib1g-dev for inkwell

# rust
curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs > rustup.sh
chmod +x rustup.sh
./rustup.sh -y
source $HOME/.cargo/env
rustup install 1.54
# TODO actually use this toolchain
rustup component add clippy
rustup component add rustfmt
# zig
wget -c https://ziglang.org/download/0.8.0/zig-linux-x86_64-0.8.0.tar.xz --no-check-certificate
tar -xf zig-linux-x86_64-0.8.0.tar.xz
ln -s ${PWD}/zig-linux-x86_64-0.8.0/zig /usr/bin/zig
# llvm
apt -y install lsb-release software-properties-common
wget https://apt.llvm.org/llvm.sh
chmod +x llvm.sh
./llvm.sh 12
ln -s /usr/bin/llvm-as-12 /usr/bin/llvm-as
ln -s /usr/bin/clang-12 /usr/bin/clang # for compiling the c platform
# use lld as rust linker for faster linking
ln -s /usr/bin/lld-12 /usr/bin/ld.lld

apt -y install valgrind

echo "Done installing."
echo ""
echo "Add the following to your ~/.bashrc file or equivalent: RUSTFLAGS=\"-C link-arg=-fuse-ld=lld -C target-cpu=native\"."
echo ""
echo "Test your installation by running: cargo test --release."