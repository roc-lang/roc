# Use the base image for rust
FROM mcr.microsoft.com/devcontainers/rust:1-1-bullseye

# Install required packages and configure features

# Install packages required by onCreateCommand
RUN apt-get update && \
    apt-get install -y pkg-config libz-dev libzstd-dev valgrind

# Install Zig 0.13.0
ARG ZIG_VERSION=0.13.0
RUN apt-get update && \
    apt-get install -y wget curl && \
    curl -o zig.tar.gz https://ziglang.org/download/${ZIG_VERSION}/zig-linux-x86_64-${ZIG_VERSION}.tar.xz && \
    tar -xf zig.tar.gz && \
    mv zig-linux-x86_64-${ZIG_VERSION} /usr/local/zig && \
    rm zig.tar.gz && \
    ln -s /usr/local/zig/zig /usr/local/bin/zig && \
    apt-get remove -y --purge wget curl && \
    apt-get autoremove -y

# Install LLVM 18
ARG LLVM_VERSION=18
RUN apt-get update && \
    apt-get install -y wget gnupg software-properties-common && \
    wget https://apt.llvm.org/llvm.sh && \
    chmod +x llvm.sh && \
    ./llvm.sh ${LLVM_VERSION} && \
    rm llvm.sh && \
    apt-get remove -y --purge wget gnupg software-properties-common && \
    apt-get autoremove -y