app [main] { pf: platform "./platform/main.roc" }

main : Str -> U64
main = |s|
    match s {
        "GET /${_}.txt" => 1
        "${_}.json" => 2
        _ => 0
    }
