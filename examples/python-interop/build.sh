#!/bin/sh

# https://vaneyckt.io/posts/safer_bash_scripts_with_set_euxo_pipefail/
set -euxo pipefail

# Could assume roc binary on path but this may be preferable
cargo build --release
../../target/release/roc build --lib libhello.roc

# For Python to find libhello, it needs it to be in a known library path, so we export
export LD_LIBRARY_PATH=$(pwd):$LD_LIBRARY_PATH

# Optional. Flag to indicate CPython which compiler to use
export cc=clang

# And we're done, now just pack it all together with python's build system
# setup.py will compile our demo.c to a shared library that depends on libhello.
# One of the nice things about CPython is that this demo shared library is simply importable in CPython
python -m venv .interop_env
source .interop_env/bin/activate
python setup.py install
set +x
echo "You may now enter your virtual environment.
In bash/zsh, run: source .interop_env/bin/activate
In fish, run: source .interop_env/bin/activate.fish
In csh/tcsh (really?), run: source .interop_env/bin/activate.csh
Then, try entring an interactive python shell, import demo and call demo.call_roc with your number of choice."
