FROM debian:bullseye-slim
WORKDIR /earthbuild

prep-debian:
    RUN apt -y update

install-script:
    FROM +prep-debian
    COPY developer_install.sh ./
    RUN ./developer_install.sh
    # make sure cargo and rustup commands are available
    ENV PATH="/root/.cargo/bin:$PATH"

ci-extras:
    FROM +install-script
    # for sccache
    RUN apt -y install libssl-dev
    # for compilation caching
    RUN cargo install sccache
    RUN sccache -V
    ENV RUSTC_WRAPPER=/root/.cargo/bin/sccache
    ENV SCCACHE_DIR=/earthbuild/sccache_dir

test-zig:
    FROM +install-script
    COPY --dir compiler/builtins/bitcode ./
    RUN cd bitcode && ./run-tests.sh

copy-dirs:
    FROM +ci-extras
    COPY --dir cli compiler docs editor ast code_markup utils roc_std vendor examples linker Cargo.toml Cargo.lock version.txt ./

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
    COPY --dir .github ci cli compiler docs editor examples ast code_markup utils linker nightly_benches packages roc_std www *.md LEGAL_DETAILS shell.nix version.txt ./
    RUN typos

test-rust:
    FROM +copy-dirs
    ENV RUST_BACKTRACE=1
    # for race condition problem with cli test
    ENV ROC_NUM_WORKERS=1
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

build-nightly-release:
    FROM +test-rust
    COPY --dir .git ./
    # version.txt is used by the CLI: roc --version
    RUN printf "nightly pre-release, built from commit " > version.txt
    # write hash of latests commit to version.txt
    RUN git log --pretty=format:'%h' -n 1 >> version.txt
    RUN cargo build --release
    RUN cd ./target/release && tar -czvf roc_linux_x86_64.tar.gz ./roc
    SAVE ARTIFACT ./target/release/roc_linux_x86_64.tar.gz AS LOCAL roc_linux_x86_64.tar.gz

# compile everything needed for benchmarks and output a self-contained dir from which benchmarks can be run.
prep-bench-folder:
    FROM +copy-dirs
    # suffix is set to allow comparing benchmarks between branch and trunk.
    ARG BENCH_SUFFIX=branch
    # criterion runs and collects stats for the benchmarks
    RUN cargo install cargo-criterion
    RUN cargo criterion -V
    RUN --mount=type=cache,target=$SCCACHE_DIR cd cli && cargo criterion --no-run
    RUN mkdir -p bench-folder/compiler/builtins/bitcode/src
    RUN mkdir -p bench-folder/target/release/deps
    RUN mkdir -p bench-folder/examples/benchmarks
    RUN cp examples/benchmarks/*.roc bench-folder/examples/benchmarks/
    RUN cp -r examples/benchmarks/platform bench-folder/examples/benchmarks/
    RUN cp compiler/builtins/bitcode/src/str.zig bench-folder/compiler/builtins/bitcode/src
    RUN cp target/release/roc bench-folder/target/release
    # copy the most recent time_bench to bench-folder
    RUN cp target/release/deps/`ls -t target/release/deps/ | grep time_bench | head -n 1` bench-folder/target/release/deps/time_bench
    SAVE ARTIFACT bench-folder AS LOCAL bench-folder-$BENCH_SUFFIX