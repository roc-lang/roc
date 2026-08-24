app [main!] { pf: platform "./static-lib-platform/main.roc" }

main! = |seed| {
    reserved = Str.reserve("abcdefghijkl", 16)
    suffix = if seed == 0 { " trailing text" } else { " bad" }
    concatenated = Str.concat(reserved, suffix)

    if concatenated == "abcdefghijkl trailing text" {
        "ok"
    } else {
        "bad"
    }
}
