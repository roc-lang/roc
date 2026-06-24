app [main] { pf: platform "./platform/main.roc" }

main : Str -> U64
main = |s|
    match s {
        "foo${bar}baz${qux}etc" => Str.count_utf8_bytes(bar) + Str.count_utf8_bytes(qux) * 3 + 11
        _ => 0
    }
