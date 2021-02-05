FROM rust:1.49-slim-buster
WORKDIR /earthbuild

# TODO cache cargo packages

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
    RUN ln -s /usr/bin/lld-10 /usr/bin/lld
    # valgrind
    RUN apt -y install autotools-dev cmake automake 
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

deps-image:
    FROM +install-zig-llvm-valgrind-clippy-rustfmt
    SAVE IMAGE roc-deps:latest

build-rust-tests:
    FROM +install-zig-llvm-valgrind-clippy-rustfmt
    COPY --dir oldcache $CARGO_HOME/registry/cache
    COPY --dir oldindex $CARGO_HOME/registry/index
    COPY --dir oldbin $CARGO_HOME/bin
    COPY --dir oldgitdb $CARGO_HOME/git/db 
    COPY --dir cli compiler docs editor roc_std vendor examples Cargo.toml Cargo.lock ./
    RUN cargo test --release --no-run
    SAVE ARTIFACT $CARGO_HOME/registry/cache AS LOCAL oldcache
    SAVE ARTIFACT $CARGO_HOME/registry/index AS LOCAL oldindex
    SAVE ARTIFACT $CARGO_HOME/bin AS LOCAL oldbin
    SAVE ARTIFACT $CARGO_HOME/git/db AS LOCAL oldgitdb

test-zig:
    FROM +install-zig-llvm-valgrind-clippy-rustfmt
    COPY --dir compiler/builtins/bitcode ./
    RUN cd bitcode; ./run-tests.sh;

test-rust:
    FROM +build-rust-tests
    RUN cargo test --release
    RUN ls $CARGO_HOME

test-all:
    BUILD +test-zig
    BUILD +test-rust

cargo-home:
    RUN ls $CARGO_HOME


    