app [main] { pf: platform "./platform/main.roc" }

main : Str -> U64
main = |s|
    match s {
        "abcdefghijklmnop${mid}qrstuvwxyz012345${_}" => Str.count_utf8_bytes(mid) + 400
        _ => 0
    }
