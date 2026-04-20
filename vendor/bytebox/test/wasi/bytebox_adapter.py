# Based on the wasmtime adapter in wasi-testsuite
import argparse
import subprocess
import sys
import os
import shlex

current_file_path = os.path.dirname(os.path.realpath(__file__))
bytebox_relative_path = "../../zig-out/bin/bytebox"
if sys.platform == 'Windows':
    bytebox_relative_path += ".exe"
BYTEBOX = os.path.join(current_file_path, bytebox_relative_path)

parser = argparse.ArgumentParser()
parser.add_argument("--version", action="store_true")
parser.add_argument("--test-file", action="store")
parser.add_argument("--arg", action="append", default=[])
parser.add_argument("--env", action="append", default=[])
parser.add_argument("--dir", action="append", default=[])

args = parser.parse_args()

if args.version:
    subprocess.run([BYTEBOX] + ["--version"])
    sys.exit(0)

TEST_FILE = args.test_file
PROG_ARGS = args.arg
ENV_ARGS = [j for i in args.env for j in ["--env", i]]
DIR_ARGS = [j for i in args.dir for j in ["--dir", i]]

ALL_ARGS = [BYTEBOX] + [TEST_FILE] + PROG_ARGS + ENV_ARGS + DIR_ARGS

r = subprocess.run(ALL_ARGS)
sys.exit(r.returncode)
