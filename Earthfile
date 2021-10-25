FROM debian:bullseye-slim
WORKDIR /earthbuild

prep-debian:
    RUN apt -y update

install-script:
    FROM +prep-debian
    COPY developer_install.sh ./
    RUN ./developer_install.sh

ci-extras:
    FROM +install-script
    # for compilation caching
    RUN cargo install sccache
    RUN sccache -V
    ENV RUSTC_WRAPPER=/usr/local/cargo/bin/sccache
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