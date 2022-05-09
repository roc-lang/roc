#!/bin/bash
################################################################################
# Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
# See https://llvm.org/LICENSE.txt for license information.
# SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
################################################################################
#
# This script will install the llvm toolchain on the different
# Debian and Ubuntu versions

set -eux

# read optional command line argument
LLVM_VERSION=10
if [ "$#" -eq 1 ]; then
    LLVM_VERSION=$1
fi

DISTRO=$(lsb_release -is)
VERSION=$(lsb_release -sr)
DIST_VERSION="${DISTRO}_${VERSION}"

if [[ $EUID -ne 0 ]]; then
   echo "This script must be run as root!"
   exit 1
fi

declare -A LLVM_VERSION_PATTERNS
LLVM_VERSION_PATTERNS[9]="-9"
LLVM_VERSION_PATTERNS[10]="-10"
LLVM_VERSION_PATTERNS[11]=""

if [ ! ${LLVM_VERSION_PATTERNS[$LLVM_VERSION]+_} ]; then
    echo "This script does not support LLVM version $LLVM_VERSION"
    exit 3
fi

LLVM_VERSION_STRING=${LLVM_VERSION_PATTERNS[$LLVM_VERSION]}

# find the right repository name for the distro and version
case "$DIST_VERSION" in
    Debian_9* )       REPO_NAME="deb http://apt.llvm.org/stretch/  llvm-toolchain-stretch$LLVM_VERSION_STRING main" ;;
    Debian_10* )      REPO_NAME="deb http://apt.llvm.org/buster/   llvm-toolchain-buster$LLVM_VERSION_STRING  main" ;;
    Debian_unstable ) REPO_NAME="deb http://apt.llvm.org/unstable/ llvm-toolchain$LLVM_VERSION_STRING         main" ;;
    Debian_testing )  REPO_NAME="deb http://apt.llvm.org/unstable/ llvm-toolchain$LLVM_VERSION_STRING         main" ;;
    Ubuntu_16.04 )    REPO_NAME="deb http://apt.llvm.org/xenial/   llvm-toolchain-xenial$LLVM_VERSION_STRING  main" ;;
    Ubuntu_18.04 )    REPO_NAME="deb http://apt.llvm.org/bionic/   llvm-toolchain-bionic$LLVM_VERSION_STRING  main" ;;
    Ubuntu_18.10 )    REPO_NAME="deb http://apt.llvm.org/cosmic/   llvm-toolchain-cosmic$LLVM_VERSION_STRING  main" ;;
    Ubuntu_19.04 )    REPO_NAME="deb http://apt.llvm.org/disco/    llvm-toolchain-disco$LLVM_VERSION_STRING   main" ;;
    Ubuntu_19.10 )   REPO_NAME="deb http://apt.llvm.org/eoan/      llvm-toolchain-eoan$LLVM_VERSION_STRING    main" ;;
    Ubuntu_20.04 )   REPO_NAME="deb http://apt.llvm.org/focal/     llvm-toolchain-focal$LLVM_VERSION_STRING    main" ;;
    * )
        echo "Distribution '$DISTRO' in version '$VERSION' is not supported by this script (${DIST_VERSION})."
        exit 2
esac


# install everything
wget -O - https://apt.llvm.org/llvm-snapshot.gpg.key | apt-key add -
add-apt-repository "${REPO_NAME}"
apt-get update
apt-get install -y clang-$LLVM_VERSION lldb-$LLVM_VERSION lld-$LLVM_VERSION clangd-$LLVM_VERSION libc6-dbg libxcb-render0-dev libxcb-shape0-dev libxcb-xfixes0-dev

wget https://sourceware.org/pub/valgrind/valgrind-3.16.1.tar.bz2
tar -xf valgrind-3.16.1.tar.bz2
mv valgrind-3.16.1 ~
pushd ~/valgrind-3.16.1
apt-get install -y autotools-dev automake
./autogen.sh
./configure
make -j`nproc`
sudo make install
popd

# Report current valgrind version, to confirm it installed properly
valgrind --version

# install zig - can't use apt-get since we require at least a specific commit later then the most recent tag (0.6.0)
wget -c https://ziglang.org/download/0.9.1/zig-linux-x86_64-0.9.1.tar.xz --no-check-certificate
tar -xf zig-linux-x86_64-0.9.1.tar.xz
ln -s "$PWD/zig-linux-x86_64-0.9.1/zig" /usr/local/bin/zig

# test sccache
./ci/sccache -V
# copy sccache to prevent current working dir problems
cp ./ci/sccache /usr/local/bin/sccache
