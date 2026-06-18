app [main] { pf: platform "./platform/main.roc" }

main : Str -> U64
main = |s|
    match s {
        "alpha:${a};beta:${b};end" => Str.count_utf8_bytes(a) + Str.count_utf8_bytes(b) + 100
        "${stem}.json" => Str.count_utf8_bytes(stem) + 200
        "log:${_}:level:${level}${_}" => Str.count_utf8_bytes(level) + 300
        _ => 0
    }
