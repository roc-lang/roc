FROM rust:1.50-slim-buster
WORKDIR /earthbuild

prep-debian:
    RUN apt -y update

install-other-libs:
    FROM +prep-debian
    RUN apt -y install wget git
    RUN apt -y install libxcb-shape0-dev libxcb-xfixes0-dev # for editor clipboard
    RUN apt -y install libc++-dev libc++abi-dev libunwind-dev pkg-config libx11-dev zlib1g-dev

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
    RUN cd valgrind-3.16.1; ./autogen.sh
    RUN cd valgrind-3.16.1; ./configure --disable-dependency-tracking
    RUN cd valgrind-3.16.1; make -j`nproc`
    RUN cd valgrind-3.16.1; make install
    # clippy
    RUN rustup component add clippy
    # rustfmt
    RUN rustup component add rustfmt
    # sccache
    RUN apt -y install libssl-dev
    RUN cargo install sccache --version 0.2.15
    RUN sccache -V
    ENV RUSTC_WRAPPER=/usr/local/cargo/bin/sccache
    ENV SCCACHE_DIR=/earthbuild/sccache_dir
    ENV CARGO_INCREMENTAL=0 # no need to recompile package when using new function

deps-image:
    FROM +install-zig-llvm-valgrind-clippy-rustfmt
    SAVE IMAGE roc-deps:latest

copy-skeleton:
    # This is ridiculous and is needed because of how caching works in earthly currently.
    # All of these commands take a really long time to run as well due to caching.
    # There is an earthly bug that will hopefully fix this eventually: https://github.com/earthly/earthly/issues/786
    COPY Cargo.lock Cargo.toml ./
    COPY cli/Cargo.lock cli/Cargo.toml ./cli/
    COPY compiler/arena_pool/Cargo.toml ./compiler/arena_pool/
    COPY compiler/build/Cargo.toml ./compiler/build/
    COPY compiler/builtins/Cargo.toml ./compiler/builtins/
    COPY compiler/can/Cargo.toml ./compiler/can/
    COPY compiler/collections/Cargo.toml ./compiler/collections/
    COPY compiler/constrain/Cargo.toml ./compiler/constrain/
    COPY compiler/fmt/Cargo.toml ./compiler/fmt/
    COPY compiler/gen/Cargo.toml ./compiler/gen/
    COPY compiler/gen_dev/Cargo.toml ./compiler/gen_dev/
    COPY compiler/load/Cargo.toml ./compiler/load/
    COPY compiler/module/Cargo.toml ./compiler/module/
    COPY compiler/mono/Cargo.toml ./compiler/mono/
    COPY compiler/parse/Cargo.toml ./compiler/parse/
    COPY compiler/parse/fuzz/Cargo.lock compiler/parse/fuzz/Cargo.toml ./compiler/parse/fuzz/
    COPY compiler/problem/Cargo.toml ./compiler/problem/
    COPY compiler/region/Cargo.toml ./compiler/region/
    COPY compiler/reporting/Cargo.toml ./compiler/reporting/
    COPY compiler/solve/Cargo.toml ./compiler/solve/
    COPY compiler/str/Cargo.toml ./compiler/str/
    COPY compiler/types/Cargo.toml ./compiler/types/
    COPY compiler/unify/Cargo.toml ./compiler/unify/
    COPY docs/Cargo.toml ./docs/
    COPY editor/Cargo.toml ./editor/
    COPY examples/balance/platform/Cargo.lock examples/balance/platform/Cargo.toml ./examples/balance/platform/
    COPY examples/effect/thing/platform-dir/Cargo.lock examples/effect/thing/platform-dir/Cargo.toml ./examples/effect/thing/platform-dir/
    COPY examples/shared-quicksort/platform/Cargo.lock examples/shared-quicksort/platform/Cargo.toml ./examples/shared-quicksort/platform/
    COPY examples/tea/platform/Cargo.lock examples/tea/platform/Cargo.toml ./examples/tea/platform/
    COPY roc_std/Cargo.toml ./roc_std/
    COPY vendor/ena/Cargo.toml ./vendor/ena/
    COPY vendor/pathfinding/Cargo.toml ./vendor/pathfinding/
    COPY vendor/pretty/Cargo.toml ./vendor/pretty/
    RUN mkdir -p cli/src/ && touch cli/src/main.rs
    RUN mkdir -p compiler/arena_pool/src/ && touch compiler/arena_pool/src/lib.rs
    RUN mkdir -p compiler/build/src/ && touch compiler/build/src/lib.rs
    RUN mkdir -p compiler/builtins/src/ && touch compiler/builtins/src/lib.rs
    RUN mkdir -p compiler/can/src/ && touch compiler/can/src/lib.rs
    RUN mkdir -p compiler/collections/src/ && touch compiler/collections/src/lib.rs
    RUN mkdir -p compiler/constrain/src/ && touch compiler/constrain/src/lib.rs
    RUN mkdir -p compiler/fmt/src/ && touch compiler/fmt/src/lib.rs
    RUN mkdir -p compiler/gen/src/ && touch compiler/gen/src/lib.rs
    RUN mkdir -p compiler/gen_dev/src/ && touch compiler/gen_dev/src/lib.rs
    RUN mkdir -p compiler/load/src/ && touch compiler/load/src/lib.rs
    RUN mkdir -p compiler/module/src/ && touch compiler/module/src/lib.rs
    RUN mkdir -p compiler/mono/src/ && touch compiler/mono/src/lib.rs
    RUN mkdir -p compiler/parse/src/ && touch compiler/parse/src/lib.rs
    RUN mkdir -p compiler/parse/fuzz/src/ && touch compiler/parse/fuzz/src/lib.rs
    RUN mkdir -p compiler/problem/src/ && touch compiler/problem/src/lib.rs
    RUN mkdir -p compiler/region/src/ && touch compiler/region/src/lib.rs
    RUN mkdir -p compiler/reporting/src/ && touch compiler/reporting/src/lib.rs
    RUN mkdir -p compiler/solve/src/ && touch compiler/solve/src/lib.rs
    RUN mkdir -p compiler/str/src/ && touch compiler/str/src/lib.rs
    RUN mkdir -p compiler/types/src/ && touch compiler/types/src/lib.rs
    RUN mkdir -p compiler/unify/src/ && touch compiler/unify/src/lib.rs
    RUN mkdir -p docs/src/ && touch docs/src/main.rs
    RUN mkdir -p editor/src/ && touch editor/src/lib.rs
    RUN mkdir -p examples/balance/platform/src/ && touch examples/balance/platform/src/lib.rs
    RUN mkdir -p examples/effect/thing/platform-dir/src/ && touch examples/effect/thing/platform-dir/src/lib.rs
    RUN mkdir -p examples/shared-quicksort/platform/src/ && touch examples/shared-quicksort/platform/src/lib.rs
    RUN mkdir -p examples/tea/platform/src/ && touch examples/tea/platform/src/lib.rs
    RUN mkdir -p roc_std/src/ && touch roc_std/src/lib.rs
    RUN mkdir -p vendor/ena/src/ && touch vendor/ena/src/lib.rs
    RUN mkdir -p vendor/pathfinding/src/ && touch vendor/pathfinding/src/lib.rs
    RUN mkdir -p vendor/pretty/src/ && touch vendor/pretty/src/lib.rs

prepare-cache:
    FROM +copy-skeleton 
    RUN cargo install cargo-chef --version 0.1.15
    RUN cargo chef prepare
    SAVE ARTIFACT recipe.json

save-cache:
    FROM +install-zig-llvm-valgrind-clippy-rustfmt
    RUN --mount=type=cache,target=$SCCACHE_DIR \
        cargo install cargo-chef --version 0.1.15
    COPY +prepare-cache/recipe.json ./
    RUN --mount=type=cache,target=$SCCACHE_DIR \
        cargo chef cook; sccache --show-stats # for clippy
    RUN --mount=type=cache,target=$SCCACHE_DIR \
        cargo chef cook --release --tests; sccache --show-stats
    SAVE ARTIFACT target
    SAVE ARTIFACT $CARGO_HOME cargo_home

copy-dirs-and-cache:
    FROM +install-zig-llvm-valgrind-clippy-rustfmt
    # cache
    COPY +save-cache/target ./target
    COPY +save-cache/cargo_home $CARGO_HOME
    # roc dirs
    COPY --dir cli compiler docs editor roc_std vendor examples Cargo.toml Cargo.lock ./

test-zig:
    FROM +install-zig-llvm-valgrind-clippy-rustfmt
    COPY --dir compiler/builtins/bitcode ./
    RUN cd bitcode; ./run-tests.sh;

build-rust:
    FROM +copy-dirs-and-cache
    RUN --mount=type=cache,target=$SCCACHE_DIR \
        cargo build; sccache --show-stats # for clippy
    RUN --mount=type=cache,target=$SCCACHE_DIR \
        cargo test --release --no-run; sccache --show-stats

check-clippy:
    FROM +build-rust
    RUN cargo clippy -V
    RUN --mount=type=cache,target=$SCCACHE_DIR \
        cargo clippy -- -D warnings

check-rustfmt:
    FROM +copy-dirs-and-cache
    RUN cargo fmt --version
    RUN cargo fmt --all -- --check

test-rust:
    FROM +build-rust
    ENV RUST_BACKTRACE=1
    RUN --mount=type=cache,target=$SCCACHE_DIR \
        cargo test --release 

test-all:
    BUILD +test-zig
    BUILD +check-rustfmt
    BUILD +check-clippy
    BUILD +test-rust
    
