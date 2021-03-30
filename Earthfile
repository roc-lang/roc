FROM rust:1.51-slim-buster
WORKDIR /earthbuild

prep-debian:
    RUN apt -y update

install-other-libs:
    FROM +prep-debian
    RUN apt -y install wget git
    RUN apt -y install libxcb-shape0-dev libxcb-xfixes0-dev # for editor clipboard
    RUN apt -y install libc++-dev libc++abi-dev g++ libunwind-dev pkg-config libx11-dev zlib1g-dev

install-zig-llvm-valgrind-clippy-rustfmt:
    FROM +install-other-libs
    # zig
    RUN wget -c https://ziglang.org/download/0.7.1/zig-linux-x86_64-0.7.1.tar.xz --no-check-certificate
    RUN tar -xf zig-linux-x86_64-0.7.1.tar.xz
    RUN ln -s /earthbuild/zig-linux-x86_64-0.7.1/zig /usr/bin/zig
    # llvm
    RUN apt -y install lsb-release software-properties-common gnupg
    RUN wget https://apt.llvm.org/llvm.sh
    RUN chmod +x llvm.sh
    RUN ./llvm.sh 10
    RUN ln -s /usr/bin/clang-10 /usr/bin/clang
    # use lld as linker
    RUN ln -s /usr/bin/lld-10 /usr/bin/ld.lld
    ENV RUSTFLAGS="-C link-arg=-fuse-ld=lld -C target-cpu=native"
    # valgrind
    RUN apt -y install autotools-dev cmake automake libc6-dbg
    RUN wget https://sourceware.org/pub/valgrind/valgrind-3.16.1.tar.bz2
    RUN tar -xf valgrind-3.16.1.tar.bz2
    # need to cd every time, every command starts at WORKDIR
    RUN cd valgrind-3.16.1 && ./autogen.sh
    RUN cd valgrind-3.16.1 && ./configure --disable-dependency-tracking
    RUN cd valgrind-3.16.1 && make -j`nproc`
    RUN cd valgrind-3.16.1 && make install
    # clippy
    RUN rustup component add clippy
    # rustfmt
    RUN rustup component add rustfmt
    # sccache
    RUN apt -y install libssl-dev
    RUN cargo install sccache
    RUN sccache -V
    ENV RUSTC_WRAPPER=/usr/local/cargo/bin/sccache
    ENV SCCACHE_DIR=/earthbuild/sccache_dir
    ENV CARGO_INCREMENTAL=0 # no need to recompile package when using new function
    RUN --mount=type=cache,target=$SCCACHE_DIR \
        cargo install cargo-chef

deps-image:
    FROM +install-zig-llvm-valgrind-clippy-rustfmt
    SAVE IMAGE roc-deps:latest

copy-dirs:
    FROM +install-zig-llvm-valgrind-clippy-rustfmt
    # If you edit this, make sure to update copy-dirs-and-cache below.
    COPY --dir cli compiler docs editor roc_std vendor examples Cargo.toml Cargo.lock ./

copy-dirs-and-cache:
    FROM +install-zig-llvm-valgrind-clippy-rustfmt
    COPY +save-cache/target ./target
    COPY +save-cache/cargo_home $CARGO_HOME
    # This needs to be kept in sync with copy-dirs above.
    # The reason this is at the end is to maximize caching.
    # Lines above this should be cached even if the code changes.
    COPY --dir cli compiler docs editor roc_std vendor examples Cargo.toml Cargo.lock ./

prepare-cache:
    FROM +copy-dirs
    RUN cargo chef prepare
    SAVE ARTIFACT recipe.json

save-cache:
    FROM +install-zig-llvm-valgrind-clippy-rustfmt
    COPY +prepare-cache/recipe.json ./
    RUN --mount=type=cache,target=$SCCACHE_DIR \
        cargo chef cook && sccache --show-stats # for clippy
    RUN --mount=type=cache,target=$SCCACHE_DIR \
        cargo chef cook --release --tests && sccache --show-stats
    SAVE ARTIFACT target
    SAVE ARTIFACT $CARGO_HOME cargo_home

test-zig:
    FROM +install-zig-llvm-valgrind-clippy-rustfmt
    COPY --dir compiler/builtins/bitcode ./
    RUN cd bitcode && ./run-tests.sh

check-clippy:
    FROM +copy-dirs-and-cache
    RUN cargo clippy -V
    RUN --mount=type=cache,target=$SCCACHE_DIR \
        cargo clippy -- -D warnings

check-rustfmt:
    FROM +copy-dirs
    RUN cargo fmt --version
    RUN cargo fmt --all -- --check

test-rust:
    FROM +copy-dirs-and-cache
    ENV RUST_BACKTRACE=1
    RUN --mount=type=cache,target=$SCCACHE_DIR \
        cargo test --release && sccache --show-stats

test-all:
    BUILD +test-zig
    BUILD +check-rustfmt
    BUILD +check-clippy
    BUILD +test-rust
    
