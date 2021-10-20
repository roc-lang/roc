#!/bin/bash

if [[ $EUID -ne 0 ]]; then
   echo "Please run this script as root. Use `sudo ./install.sh` or `su -` followed by `./install.sh`"
   exit 1
fi

apt -y install wget curl
# rust
curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh
# zig
wget -c https://ziglang.org/download/0.8.0/zig-linux-x86_64-0.8.0.tar.xz --no-check-certificate
tar -xf zig-linux-x86_64-0.8.0.tar.xz
ln -s ./zig-linux-x86_64-0.8.0/zig /usr/bin/zig
# llvm
wget https://apt.llvm.org/llvm.sh
chmod +x llvm.sh
./llvm.sh 12
# use lld as a faster linker
# TODO ln -s /usr/bin/lld-12 /usr/bin/ld.lld
# TODO RUSTFLAGS="-C link-arg=-fuse-ld=lld -C target-cpu=native"
apt -y install valgrind