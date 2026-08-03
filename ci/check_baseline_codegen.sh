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

roc_bin="$1"
repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
tmp_dir="$(mktemp -d)"
trap 'rm -rf -- "$tmp_dir"' EXIT

cd "$repo_root"
smoke="test/cli/baseline_cpu_smoke.roc"

build() { # target opt out
    "$roc_bin" build --opt="$2" --no-cache --target="$1" --output="$tmp_dir/$3" "$smoke" >/dev/null
}

# Instructions above the x86-64 baseline. Anchored at both ends so each
# alternative names a whole mnemonic: an unanchored `pext` also matches
# `pextrw`, which is SSE2 and perfectly legal here.
#
# `pinsrw`, `pextrw`, `pminub`, `pmaxub`, `pminsw`, and `pmaxsw` are absent on
# purpose for the same reason: those forms are SSE2, and only their
# byte/dword/qword siblings arrived with SSE4.1.
x86_above_baseline='^(v[a-z].*|popcnt|lzcnt|tzcnt|pshufb|pclmul.*|pext|pdep|pmovsx.*|pmovzx.*|pminsb|pmaxsb|pminuw|pmaxuw|pminud|pmaxud|pminsd|pmaxsd|pcmpgtq|pcmpeqq|palignr|phadd.*|phsub.*|pmulld|pmuldq|pabs[bwd]|pmaddubsw|pmulhrsw|blend.*|ptest|movbe|crc32.*|round[sp][sd]|insertps|pinsr[bdq]|pextr[bdq])$'

x86_mnemonics() { # binary -> distinct mnemonics in .text
    objdump -d --no-show-raw-insn --section=.text "$1" \
        | awk '{ if ($1 ~ /^[0-9a-f]+:$/) print $2 }' \
        | sort -u
}

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
        if grep -Eq "$x86_above_baseline" <<<"$mnemonics"; then
            echo "x64v1musl --opt=$opt emitted instructions above the x86-64 baseline:" >&2
            grep -E "$x86_above_baseline" <<<"$mnemonics" >&2
            exit 1
        fi

        # Positive control: the same program at the default level must contain
        # some of them, or the pattern above matches nothing and proves nothing.
        if ! x86_mnemonics "$tmp_dir/default-$opt" | grep -Eq "$x86_above_baseline"; then
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
