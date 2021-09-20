FROM rust:1.54-slim-bullseye
WORKDIR /earthbuild

prep-debian:
    RUN apt -y update

install-other-libs:
    FROM +prep-debian
    RUN apt -y install wget git
    RUN apt -y install libxcb-shape0-dev libxcb-xfixes0-dev # for editor clipboard
    RUN apt -y install libunwind-dev pkg-config libx11-dev zlib1g-dev

install-zig-llvm-valgrind-clippy-rustfmt:
    FROM +install-other-libs
    # zig
    RUN wget -c https://ziglang.org/download/0.8.0/zig-linux-x86_64-0.8.0.tar.xz --no-check-certificate
    RUN tar -xf zig-linux-x86_64-0.8.0.tar.xz
    RUN ln -s /earthbuild/zig-linux-x86_64-0.8.0/zig /usr/bin/zig
    # llvm
    RUN apt -y install lsb-release software-properties-common gnupg
    RUN wget https://apt.llvm.org/llvm.sh
    RUN chmod +x llvm.sh
    RUN ./llvm.sh 12
    RUN ln -s /usr/bin/clang-12 /usr/bin/clang
    RUN ln -s /usr/bin/llvm-as-12 /usr/bin/llvm-as
    # use lld as linker
    RUN ln -s /usr/bin/lld-12 /usr/bin/ld.lld
    ENV RUSTFLAGS="-C link-arg=-fuse-ld=lld -C target-cpu=native"
    # valgrind
    RUN apt -y install valgrind
    # clippy
    RUN rustup component add clippy
    # rustfmt
    RUN rustup component add rustfmt
    # criterion
    RUN cargo install cargo-criterion
    # wasm
    RUN apt -y install libxkbcommon-dev
    # sccache
    RUN apt -y install libssl-dev
    RUN cargo install sccache
    RUN sccache -V
    ENV RUSTC_WRAPPER=/usr/local/cargo/bin/sccache
    ENV SCCACHE_DIR=/earthbuild/sccache_dir
    ENV CARGO_INCREMENTAL=0 # no need to recompile package when using new function

copy-dirs:
    FROM +install-zig-llvm-valgrind-clippy-rustfmt
    COPY --dir cli compiler docs editor roc_std vendor examples linker Cargo.toml Cargo.lock ./

test-zig:
    FROM +install-zig-llvm-valgrind-clippy-rustfmt
    COPY --dir compiler/builtins/bitcode ./
    RUN cd bitcode && ./run-tests.sh

check-clippy:
    FROM +copy-dirs
    RUN cargo clippy -V
    RUN --mount=type=cache,target=$SCCACHE_DIR \
        cargo clippy -- -D warnings

check-rustfmt:
    FROM +copy-dirs
    RUN cargo fmt --version
    RUN cargo fmt --all -- --check

check-typos:
    RUN cargo install typos-cli --version 1.0.11 # version set to prevent confusion if the version is updated automatically
    COPY --dir .github ci cli compiler docs editor examples linker nightly_benches packages roc_std www *.md LEGAL_DETAILS shell.nix ./
    RUN typos

test-rust:
    FROM +copy-dirs
    ENV RUST_BACKTRACE=1
    # run one of the benchmarks to make sure the host is compiled
    # not pre-compiling the host can cause race conditions
    RUN echo "4" | cargo run --release examples/benchmarks/NQueens.roc
    RUN --mount=type=cache,target=$SCCACHE_DIR \
        cargo test --release && sccache --show-stats
    # run i386 (32-bit linux) cli tests
    RUN echo "4" | cargo run --release -- --backend=x86_32 examples/benchmarks/NQueens.roc
    RUN --mount=type=cache,target=$SCCACHE_DIR \
        cargo test --release --test cli_run i386 --features="i386-cli-run" && sccache --show-stats

verify-no-git-changes:
    FROM +test-rust
    # If running tests caused anything to be changed or added (without being
    # included in a .gitignore somewhere), fail the build!
    #
    # How it works: the `git ls-files` command lists all the modified or
    # uncommitted files in the working tree, the `| grep -E .` command returns a
    # zero exit code if it listed any files and nonzero otherwise (which is the
    # opposite of what we want), and the `!` at the start inverts the exit code.
    RUN ! git ls-files --deleted --modified --others --exclude-standard | grep -E .

test-all:
    BUILD +test-zig
    BUILD +check-rustfmt
    BUILD +check-clippy
    BUILD +test-rust
    BUILD +verify-no-git-changes

# compile everything needed for benchmarks and output a self-contained folder
prep-bench-folder:
    FROM +copy-dirs
    ARG BENCH_SUFFIX=branch
    RUN cargo criterion -V
    RUN --mount=type=cache,target=$SCCACHE_DIR cd cli && cargo criterion --no-run
    RUN mkdir -p bench-folder/compiler/builtins/bitcode/src
    RUN mkdir -p bench-folder/target/release/deps
    RUN mkdir -p bench-folder/examples/benchmarks
    RUN cp examples/benchmarks/*.roc bench-folder/examples/benchmarks/
    RUN cp -r examples/benchmarks/platform bench-folder/examples/benchmarks/
    RUN cp compiler/builtins/bitcode/src/str.zig bench-folder/compiler/builtins/bitcode/src
    RUN cp target/release/roc bench-folder/target/release
    # copy the most recent time bench to bench-folder
    RUN cp target/release/deps/`ls -t target/release/deps/ | grep time_bench | head -n 1` bench-folder/target/release/deps/time_bench
    SAVE ARTIFACT bench-folder AS LOCAL bench-folder-$BENCH_SUFFIX
