#!/usr/bin/env bash
#
# Run the SIMD differential corpus as a `v1` dev-backend program on the host
# architecture, and prove that the binary which ran it contains no instruction
# above the architecture's oldest revision.
#
# A `v1` target lowers every op whose default-level instructions sit above the
# baseline through a different instruction sequence, so it is a consumer of
# its own: only running the corpus checks those sequences lane by lane. The
# host CPU may well implement newer instructions than the baseline, so passing
# the corpus does not show that the binary avoids them; the disassembly scan
# does. As in ci/check_baseline_codegen.sh, the scan has a positive control on
# the default-level build of the same corpus, because a scan that silently
# finds nothing looks exactly like a passing one.
set -euo pipefail

case "$(uname -s)-$(uname -m)" in
    Linux-x86_64)
        v1_target=x64v1musl
        default_target=x64musl
        ;;
    Linux-aarch64)
        v1_target=arm64v1musl
        default_target=arm64musl
        ;;
    *)
        echo "the v1 SIMD differential runs natively only on Linux x86-64 and AArch64"
        exit 0
        ;;
esac

roc_bin="$1"
repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
tmp_dir="$(mktemp -d)"
trap 'rm -rf -- "$tmp_dir"' EXIT

cd "$repo_root"
corpus="test/simd/differential.roc"

# shellcheck source=ci/baseline_isa.sh
source ci/baseline_isa.sh

"$roc_bin" build --opt=dev --no-cache --target="$v1_target" --output="$tmp_dir/v1" "$corpus" >/dev/null
"$roc_bin" build --opt=dev --no-cache --target="$default_target" --output="$tmp_dir/default" "$corpus" >/dev/null

if [[ "$v1_target" == x64v1musl ]]; then
    # Only the procedures the dev backend emitted: the test platform's host and
    # the builtins it links are Zig code compiled for the same baseline, which
    # writes a count of trailing zeros as `rep bsf`, an encoding a disassembler
    # reads as `tzcnt` and a pre-BMI CPU executes as `bsf`.
    mnemonics="$(x86_mnemonics "$tmp_dir/v1" roc__)"
    if [[ -z "$mnemonics" ]]; then
        echo "$v1_target --opt=dev disassembled to nothing; the check cannot pass vacuously" >&2
        exit 1
    fi
    if x86_above_baseline_mnemonics <<<"$mnemonics" | grep -q .; then
        echo "$v1_target --opt=dev SIMD corpus emitted instructions above the x86-64 baseline:" >&2
        x86_above_baseline_mnemonics <<<"$mnemonics" >&2
        exit 1
    fi
    if ! x86_mnemonics "$tmp_dir/default" roc__ | x86_above_baseline_mnemonics | grep -q .; then
        echo "$default_target --opt=dev SIMD corpus emitted no above-baseline instruction; the check is not discriminating" >&2
        exit 1
    fi
else
    # PMULL comes from the AES extension rather than base NEON, so it is the
    # AArch64 instruction a baseline binary must not contain.
    v1_count="$(python3 ci/count_aarch64_pmull.py "$tmp_dir/v1")"
    default_count="$(python3 ci/count_aarch64_pmull.py "$tmp_dir/default")"
    if [[ "$v1_count" != "0" ]]; then
        echo "$v1_target --opt=dev SIMD corpus emitted $v1_count PMULL instructions; Armv8.0-A has none" >&2
        exit 1
    fi
    if [[ "$default_count" == "0" ]]; then
        echo "$default_target --opt=dev SIMD corpus emitted no PMULL; the check is not discriminating" >&2
        exit 1
    fi
fi

output="$("$tmp_dir/v1" </dev/null)"
if [[ "$output" != "PASS simd differential" ]]; then
    echo "$v1_target --opt=dev SIMD differential corpus did not pass:" >&2
    echo "$output" >&2
    exit 1
fi
echo "$v1_target --opt=dev SIMD differential corpus passed with baseline instructions only"
