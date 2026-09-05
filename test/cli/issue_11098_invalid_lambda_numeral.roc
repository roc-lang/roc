app [main!] { pf: platform "../fx-open/platform/main.roc" }

get_n = |{}| 300

main! = |_args| {
    _a : U8
    _a = get_n({})
    Ok({})
}
