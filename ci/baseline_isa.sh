# Shared by the scripts that prove `v1` binaries contain no instruction above
# their architecture's oldest revision. Source it; it defines functions only.

# Instructions above the x86-64 baseline. Anchored at both ends so each
# alternative names a whole mnemonic: an unanchored `pext` also matches
# `pextrw`, which is SSE2 and perfectly legal here.
#
# `pinsrw`, `pextrw`, `pminub`, `pmaxub`, `pminsw`, and `pmaxsw` are absent on
# purpose for the same reason: those forms are SSE2, and only their
# byte/dword/qword siblings arrived with SSE4.1.
x86_above_baseline='^(v[a-z].*|popcnt|lzcnt|tzcnt|pshufb|pclmul.*|pext|pdep|pmovsx.*|pmovzx.*|pminsb|pmaxsb|pminuw|pmaxuw|pminud|pmaxud|pminsd|pmaxsd|pcmpgtq|pcmpeqq|palignr|phadd.*|phsub.*|pmulld|pmuldq|pabs[bwd]|pmaddubsw|pmulhrsw|blend.*|ptest|movbe|crc32.*|round[sp][sd]|insertps|pinsr[bdq]|pextr[bdq]|packusdw)$'

# Binary -> distinct mnemonics in its function bodies. A plain `objdump -d` also
# decodes the padding, string literals, and stale object-writer leftovers that
# sit between functions in `.text`, and reports whatever mnemonics that data
# happens to decode to. See ci/x86_code_mnemonics.py.
#
# An optional second argument restricts the scan to functions whose names start
# with it.
x86_mnemonics() {
    python3 ci/x86_code_mnemonics.py "$@" | sort -u
}

x86_above_baseline_mnemonics() { # read mnemonics on stdin
    # VERR and VERW predate x86-64; their leading `v` does not denote AVX.
    grep -E "$x86_above_baseline" | grep -Ev '^(verr|verw)$'
}
