app [main!] { pf: platform "./platform/main.roc" }

import pf.Stdout

# Repro for https://github.com/roc-lang/roc/issues/10368
# Both maps must read the original parameter-derived padded list.
one_map : List(U8) -> Str
one_map = |p| {
    padded = List.concat(p, List.repeat(0, 64 - List.len(p)))
    mapped = List.map(padded, |b| U8.bitwise_xor(b, 0x36))
    first = Crypto.SHA256.hash(List.concat(mapped, Str.to_utf8("x"))).to_bytes()
    Crypto.SHA256.hash(List.concat(mapped, first)).to_hex()
}

two_maps : List(U8) -> Str
two_maps = |p| {
    padded = List.concat(p, List.repeat(0, 64 - List.len(p)))
    mapped1 = List.map(padded, |b| U8.bitwise_xor(b, 0x36))
    mapped2 = List.map(padded, |b| U8.bitwise_xor(b, 0x5c))
    first = Crypto.SHA256.hash(List.concat(mapped1, Str.to_utf8("x"))).to_bytes()
    Crypto.SHA256.hash(List.concat(mapped2, first)).to_hex()
}

main! = || {
    key = List.repeat(0x0b, 20)
    Stdout.line!("one map ${one_map(key)}")
    Stdout.line!("two maps ${two_maps(key)}")
}
