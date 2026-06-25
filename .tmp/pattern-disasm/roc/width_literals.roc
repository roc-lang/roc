app [main] { pf: platform "./platform/main.roc" }

main : Str -> U64
main = |s|
    match s {
        "ab${x}cd" => Str.count_utf8_bytes(x) + 2
        "abcdefgh${x}ijklmnop" => Str.count_utf8_bytes(x) + 8
        "abcdefghijklmnop${x}qrstuvwxyz012345" => Str.count_utf8_bytes(x) + 16
        _ => 0
    }
