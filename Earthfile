FROM rust:1.66.1-slim-buster # make sure to update rust-toolchain.toml too so that everything uses the same rust version
WORKDIR /earthbuild

prep-debian:
    RUN apt -y update

install-other-libs:
    FROM +prep-debian
    RUN apt -y install wget git
    RUN apt -y install libunwind-dev pkg-config libx11-dev zlib1g-dev
    RUN apt -y install unzip # for www/build.sh

install-zig-llvm-valgrind:
    FROM +install-other-libs
    # editor
    RUN apt -y install libxkbcommon-dev
    # zig
    RUN wget -c https://ziglang.org/download/0.9.1/zig-linux-x86_64-0.9.1.tar.xz --no-check-certificate
    RUN tar -xf zig-linux-x86_64-0.9.1.tar.xz
    RUN ln -s /earthbuild/zig-linux-x86_64-0.9.1/zig /bin/zig
    # zig builtins wasm tests
    RUN apt -y install build-essential
    #RUN cargo install wasmer-cli --features "singlepass"
    #RUN cargo install bindgen
    # llvm
    RUN apt -y install lsb-release software-properties-common gnupg
    RUN wget https://apt.llvm.org/llvm.sh
    RUN chmod +x llvm.sh
    RUN ./llvm.sh 13
    RUN ln -s /usr/bin/clang-13 /usr/bin/clang
    # use lld as linker
    RUN ln -s /usr/bin/lld-13 /usr/bin/ld.lld
    ENV RUSTFLAGS="-C link-arg=-fuse-ld=lld -C target-cpu=native"
    # valgrind
    RUN apt -y install valgrind
    # wasm repl & tests
    RUN rustup target add wasm32-unknown-unknown wasm32-wasi
    RUN apt -y install libssl-dev
    RUN OPENSSL_NO_VENDOR=1 cargo install wasm-pack
    # criterion
    RUN cargo install cargo-criterion
    # sccache
    RUN cargo install sccache
    RUN sccache -V
    ENV RUSTC_WRAPPER=/usr/local/cargo/bin/sccache
    ENV SCCACHE_DIR=/earthbuild/sccache_dir
    ENV CARGO_INCREMENTAL=0 # no need to recompile package when using new function

copy-dirs:
    FROM +install-zig-llvm-valgrind
    COPY --dir crates examples Cargo.toml Cargo.lock version.txt .cargo www ./

test-zig:
    FROM +install-zig-llvm-valgrind
    COPY --dir crates/compiler/builtins/bitcode ./
    RUN cd bitcode && ./run-tests.sh

build-rust-test:
    FROM +copy-dirs
    RUN echo "deb http://deb.debian.org/debian testing main contrib non-free" >> /etc/apt/sources.list # to get gcc 10.3
    RUN apt -y update
    # RUN apt -y install gcc-10 g++-10 && rm /usr/bin/gcc && ln -s /usr/bin/gcc-10 /usr/bin/gcc # gcc-9 maybe causes segfault
    RUN gcc --version
    RUN --mount=type=cache,target=$SCCACHE_DIR \
        cargo test --locked --release --workspace --no-run && sccache --show-stats

test-rust:
    FROM +build-rust-test
    ENV ROC_WORKSPACE_DIR=/earthbuild
    ENV RUST_BACKTRACE=1
    # for race condition problem with cli test
    ENV ROC_NUM_WORKERS=1
    # run one of the benchmarks to make sure the host is compiled
    # not pre-compiling the host can cause race conditions
    RUN gcc --version
    RUN --mount=type=cache,target=$SCCACHE_DIR \
        cargo test --locked --release --workspace -- --skip expects_dev_and_test && sccache --show-stats
    # test the dev and wasm backend: they require an explicit feature flag.
    RUN --mount=type=cache,target=$SCCACHE_DIR \
        cargo test --locked --release --package test_gen --no-default-features --features gen-dev && sccache --show-stats
    # gen-wasm has some multithreading problems to do with the wasmer runtime. Run it single-threaded as a separate job
    #RUN --mount=type=cache,target=$SCCACHE_DIR \
    #    cargo test --locked --release --package test_gen --no-default-features --features gen-wasm -- --test-threads=1 && sccache --show-stats
    # run `roc test` on Str builtins
    RUN --mount=type=cache,target=$SCCACHE_DIR \
        cargo run --release -- test crates/compiler/builtins/roc/Str.roc && sccache --show-stats

test-all:
    BUILD +test-zig
    BUILD +test-rust

build-nightly-release:
    FROM +test-rust
    COPY --dir .git LICENSE LEGAL_DETAILS ci ./
    # version.txt is used by the CLI: roc --version
    RUN ./ci/write_version.sh
    RUN RUSTFLAGS="-C target-cpu=x86-64" cargo build --profile=release-with-lto --locked --bin roc
    # strip debug info
    RUN strip ./target/release-with-lto/roc
    RUN ./ci/package_release.sh roc_linux_x86_64_buster
    SAVE ARTIFACT ./roc_linux_x86_64_buster.tar.gz AS LOCAL roc_linux_x86_64_buster.tar.gz
