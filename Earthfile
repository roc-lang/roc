
VERSION 0.6
FROM rust:1.71.1-slim-buster # make sure to update rust-toolchain.toml too so that everything uses the same rust version
WORKDIR /earthbuild

prep-debian:
    RUN apt -y update

install-other-libs:
    FROM +prep-debian
    RUN apt -y install wget git
    RUN apt -y install libunwind-dev pkg-config zlib1g-dev
    RUN apt -y install unzip # for www/build.sh

install-zig-llvm:
    ARG ZIG_ARCH
    FROM +install-other-libs
    # zig
    RUN wget -c https://ziglang.org/download/0.11.0/zig-linux-$ZIG_ARCH-0.11.0.tar.xz --no-check-certificate
    RUN tar -xf zig-linux-$ZIG_ARCH-0.11.0.tar.xz
    RUN ln -s /earthbuild/zig-linux-$ZIG_ARCH-0.11.0/zig /bin/zig
    # zig builtins wasm tests
    RUN apt -y install build-essential
    # llvm
    RUN apt -y install lsb-release software-properties-common gnupg
    RUN wget https://apt.llvm.org/llvm.sh
    RUN chmod +x llvm.sh
    RUN ./llvm.sh 16
    RUN ln -s /usr/bin/clang-16 /usr/bin/clang
    # use lld as linker
    RUN ln -s /usr/bin/lld-16 /usr/bin/ld.lld
    ENV RUSTFLAGS="-C link-arg=-fuse-ld=lld -C target-cpu=native"
    RUN apt -y install libssl-dev
    RUN OPENSSL_NO_VENDOR=1 cargo install wasm-pack
    # sccache
    RUN cargo install sccache
    RUN sccache -V
    ENV RUSTC_WRAPPER=/usr/local/cargo/bin/sccache
    ENV SCCACHE_DIR=/earthbuild/sccache_dir
    ENV CARGO_INCREMENTAL=0 # no need to recompile package when using new function

copy-dirs:
    ARG ZIG_ARCH
    FROM +install-zig-llvm --ZIG_ARCH=$ZIG_ARCH
    COPY --dir crates examples Cargo.toml Cargo.lock version.txt .cargo www rust-toolchain.toml ./

build-nightly-release:
    ARG RELEASE_FOLDER_NAME
    ARG RUSTFLAGS
    ARG ZIG_ARCH=x86_64
    FROM +copy-dirs --ZIG_ARCH=$ZIG_ARCH
    COPY --dir .git LICENSE LEGAL_DETAILS ci ./
    # version.txt is used by the CLI: roc --version
    RUN ./ci/write_version.sh
    RUN RUSTFLAGS=$RUSTFLAGS cargo build --profile=release-with-lto --locked --bin roc
    # strip debug info
    RUN strip ./target/release-with-lto/roc
    RUN ./ci/package_release.sh $RELEASE_FOLDER_NAME
    RUN ls
    SAVE ARTIFACT ./$RELEASE_FOLDER_NAME.tar.gz AS LOCAL $RELEASE_FOLDER_NAME.tar.gz
