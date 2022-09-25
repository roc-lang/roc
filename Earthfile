FROM rust:1.61.0-slim-bullseye # make sure to update rust-toolchain.toml too so that everything uses the same rust version
WORKDIR /earthbuild

prep-debian:
    RUN apt -y update

install-other-libs:
    FROM +prep-debian
    RUN apt -y install wget git
    RUN apt -y install libxcb-shape0-dev libxcb-xfixes0-dev # for editor clipboard
    RUN apt -y install libasound2-dev # for editor sounds
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
    RUN cargo install wasmer-cli --features "singlepass"
    RUN cargo install bindgen
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
    COPY --dir crates examples Cargo.toml Cargo.lock version.txt www ./

# compile everything needed for benchmarks and output a self-contained dir from which benchmarks can be run.
prep-bench-folder:
    FROM +copy-dirs
    # to make use of avx, avx2, sse2, sse4.2... instructions
    ENV RUSTFLAGS="-C link-arg=-fuse-ld=lld -C target-cpu=native"
    ARG BENCH_SUFFIX=branch
    RUN cargo criterion -V
    RUN --mount=type=cache,target=$SCCACHE_DIR cd crates/cli && cargo criterion --no-run
    RUN mkdir -p bench-folder/crates/compiler/builtins/bitcode/src
    RUN mkdir -p bench-folder/target/release/deps
    RUN mkdir -p bench-folder/examples/benchmarks
    RUN cp examples/benchmarks/*.roc bench-folder/examples/benchmarks/
    RUN cp -r examples/benchmarks/platform bench-folder/examples/benchmarks/
    RUN cp crates/compiler/builtins/bitcode/src/str.zig bench-folder/crates/compiler/builtins/bitcode/src
    RUN cp target/release/roc bench-folder/target/release
    # copy the most recent time bench to bench-folder
    RUN cp target/release/deps/`ls -t target/release/deps/ | grep time_bench | head -n 1` bench-folder/target/release/deps/time_bench
    SAVE ARTIFACT bench-folder AS LOCAL bench-folder-$BENCH_SUFFIX
