#!/usr/bin/env bash
#
# Verify that `v1` targets emit no instruction above their architecture's
# oldest revision, and that they still compute the same answers.
#
# Every assertion here comes with a positive control on the corresponding
# default-CPU target. Without one, a check that finds nothing looks identical
# to a check whose disassembly step silently produced no output, which is how a
# broken baseline check reports success forever.
set -euo pipefail

# The check builds Linux targets, runs the results to compare their answers,
# and reads them with objdump, so it needs a Linux x86-64 host. The property it
# verifies is a property of the emitted code, not of the machine that emitted
# it, so one host covers every target.
if [[ "$(uname -s)" != "Linux" || "$(uname -m)" != "x86_64" ]]; then
    echo "baseline codegen check is specific to Linux x86-64"
    exit 0
fi

roc_bin="$1"
repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
tmp_dir="$(mktemp -d)"
trap 'rm -rf -- "$tmp_dir"' EXIT

cd "$repo_root"
smoke="test/cli/baseline_cpu_smoke.roc"

build() { # target opt out
    "$roc_bin" build --opt="$2" --no-cache --target="$1" --output="$tmp_dir/$3" "$smoke" >/dev/null
}

# shellcheck source=ci/baseline_isa.sh
source ci/baseline_isa.sh

check_x86() {
    if [[ "$(uname -m)" != "x86_64" ]]; then
        echo "skipping x86-64 baseline check: needs an x86-64 host to run the binaries"
        return
    fi

    for opt in dev speed; do
        build x64v1musl "$opt" "v1-$opt"
        build x64musl "$opt" "default-$opt"

        local mnemonics
        mnemonics="$(x86_mnemonics "$tmp_dir/v1-$opt")"
        if [[ -z "$mnemonics" ]]; then
            echo "x64v1musl --opt=$opt disassembled to nothing; the check cannot pass vacuously" >&2
            exit 1
        fi
        # The whole binary: the host, the builtins, and the boxy runtime a dev
        # build links are Zig code compiled for the same baseline. That code
        # writes a count of trailing zeros of a value known to be nonzero as
        # `rep bsf`, an encoding a disassembler reads as `tzcnt` and a pre-BMI
        # CPU executes as `bsf` with the same result, so `tzcnt` is asserted
        # only for the procedures the Roc compiler emitted, below.
        if x86_above_baseline_mnemonics <<<"$mnemonics" | grep -vx tzcnt | grep -q .; then
            echo "x64v1musl --opt=$opt emitted instructions above the x86-64 baseline:" >&2
            x86_above_baseline_mnemonics <<<"$mnemonics" | grep -vx tzcnt >&2
            exit 1
        fi

        # The procedures the Roc compiler emitted: every above-baseline
        # instruction is ruled out, `tzcnt` included.
        local roc_mnemonics
        roc_mnemonics="$(x86_mnemonics "$tmp_dir/v1-$opt" roc__)"
        if [[ -z "$roc_mnemonics" ]]; then
            echo "x64v1musl --opt=$opt has no disassembled Roc procedure; the check cannot pass vacuously" >&2
            exit 1
        fi
        if x86_above_baseline_mnemonics <<<"$roc_mnemonics" | grep -q .; then
            echo "x64v1musl --opt=$opt emitted instructions above the x86-64 baseline in Roc procedures:" >&2
            x86_above_baseline_mnemonics <<<"$roc_mnemonics" >&2
            exit 1
        fi

        # Positive control: the same program's Roc procedures at the default
        # level must contain some of them, or the patterns above match nothing
        # and prove nothing.
        if ! x86_mnemonics "$tmp_dir/default-$opt" roc__ | x86_above_baseline_mnemonics | grep -q .; then
            echo "x64musl --opt=$opt emitted no above-baseline instruction; the check is not discriminating" >&2
            exit 1
        fi

        local v1_output default_output
        v1_output="$("$tmp_dir/v1-$opt")"
        default_output="$("$tmp_dir/default-$opt")"
        if [[ "$v1_output" != "$default_output" ]]; then
            echo "x64v1musl --opt=$opt disagreed with x64musl:" >&2
            echo "  v1:      $v1_output" >&2
            echo "  default: $default_output" >&2
            exit 1
        fi
    done
}

check_aarch64() {
    for opt in dev speed; do
        build arm64v1musl "$opt" "a64-v1-$opt"
        build arm64musl "$opt" "a64-default-$opt"

        # PMULL comes from the AES extension rather than base NEON, so it is
        # the aarch64 instruction a baseline binary must not contain. Scanning
        # aligned words of .text keeps this working without a cross
        # disassembler, which is not present on every runner.
        local v1_count default_count
        v1_count="$(python3 ci/count_aarch64_pmull.py "$tmp_dir/a64-v1-$opt")"
        default_count="$(python3 ci/count_aarch64_pmull.py "$tmp_dir/a64-default-$opt")"

        if [[ "$v1_count" != "0" ]]; then
            echo "arm64v1musl --opt=$opt emitted $v1_count PMULL instructions; Armv8.0-A has none" >&2
            exit 1
        fi
        if [[ "$default_count" == "0" ]]; then
            echo "arm64musl --opt=$opt emitted no PMULL; the check is not discriminating" >&2
            exit 1
        fi
    done
}

check_x86
check_aarch64
echo "baseline codegen check passed"
