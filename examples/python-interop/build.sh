#!/bin/sh

# https://vaneyckt.io/posts/safer_bash_scripts_with_set_euxo_pipefail/
set -euxo pipefail

# Could assume roc binary on path but this may be preferable
cargo build --release
../../target/release/roc build --lib

# Neither the application nor python needs a .0, so we can just rename it
mv libhello.so.1.0 libhello.so.1
# but one of which does expect plain libhello.so, so we symlink it
ln -sf libhello.so.1 libhello.so

# For Python to find libhello, it needs it to be in a known library path, so we export
export LD_LIBRARY_PATH=$(pwd):$LD_LIBRARY_PATH

# And we're done, now just pack it all together with python's build system
# setup.py will compile our demo.c to a shared library that depends on libhello.
# One of the nice things about CPython is that this demo shared library is simply importable in CPython
python -m venv .interop_env
source .interop_env/bin/activate
python setup.py install
echo "You may now enter your virtual environment."
echo "Run 'source .interop_env/bin/activate<.fish if running fish, .csh for csh/tcsh and no suffix at all for sh/bash/zsh>'."
echo "Try entring an interactive python shell, import demo and call demo.call_roc with your number of choice."
