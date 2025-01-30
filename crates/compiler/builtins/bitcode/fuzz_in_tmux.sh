#!/usr/bin/env bash
set -euxo pipefail

# Run from this directory.
SCRIPT_RELATIVE_DIR=`dirname "${BASH_SOURCE[0]}"`
cd $SCRIPT_RELATIVE_DIR

# First compile the fuzz target.
zig build-lib -static -fcompiler-rt -flto -fPIC src/fuzz_sort.zig
afl-clang-lto -o fuzz libfuzz_sort.a
AFL_LLVM_CMPLOG=1 afl-clang-lto -o fuzz-cmplog libfuzz_sort.a
AFL_LLVM_LAF_ALL=1 afl-clang-lto -o fuzz-cmpcov libfuzz_sort.a

# Setup fuzz directory with dummy input.
INPUT_DIR='.fuzz_data/input'
OUTPUT_DIR='.fuzz_data/output'
if [ ! -d .fuzz_data ]; then
  mkdir -p $INPUT_DIR
  echo '1234567887654321' > $INPUT_DIR/dummy_input
else
  # Resuming from existing run.
  INPUT_DIR='-'
fi

# Just hardcoding to 7 fuzzers (this avoids overwhelming 8 core machines).
BASE_CMD="AFL_TESTCACHE_SIZE=250 AFL_IMPORT_FIRST=1 afl-fuzz -i $INPUT_DIR -o $OUTPUT_DIR"

# I'm trying to follow the guide around secondary fuzzers, but I don't quite follow the wording.
# So I feel this may be correct, but it may also be more random then they expect.
# Overkill anyway...so this is fine.
tmux new-session -d -s "fuzz" "AFL_FINAL_SYNC=1 $BASE_CMD -M fuzzer01 ./fuzz"
tmux split-window -h "$BASE_CMD -S fuzzer02 -c ./fuzz-cmplog -m none -l 2AT -p explore ./fuzz"
tmux split-window -v -t 0.0 "$BASE_CMD -S fuzzer03 -c ./fuzz-cmplog -m none -L 0 -p exploit ./fuzz"
tmux split-window -v -t 0.2 "$BASE_CMD -S fuzzer04 -p explore ./fuzz-cmpcov"
tmux new-window "$BASE_CMD -S fuzzer05 -Z -p coe ./fuzz-cmpcov"
tmux split-window -h "$BASE_CMD -S fuzzer06 -P exploit ./fuzz"
tmux split-window -v -t 1.0 "AFL_DISABLE_TRIM=1 $BASE_CMD -S fuzzer07 -p explore ./fuzz"
tmux split-window -v -t 1.2 "htop"
tmux new-window "watch -c -n 30 afl-whatsup -s .fuzz_data/output"
tmux select-window -t 1
tmux select-window -t 0
tmux -2 a -t "fuzz"
