app [main] { pf: platform "./platform/main.roc" }

main : Str -> U64
main = |s|
    match s {
        "GET /${path}.txt" => Str.count_utf8_bytes(path) + 10
        _ => 0
    }
