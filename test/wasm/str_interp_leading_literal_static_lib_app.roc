app [main!] { pf: platform "./static-lib-platform/main.roc" }

main! = |seed| {
    label = "abcdefghijkl${seed.to_str()} trailing text"

    if label == "abcdefghijkl0 trailing text" {
        "ok"
    } else {
        "bad"
    }
}
