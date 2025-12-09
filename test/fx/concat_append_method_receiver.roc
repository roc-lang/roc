app [main!] { pf: platform "./platform/main.roc" }

main! = || {
    _a = [].concat([1]).is_empty()
    _b = [1].append(1).is_empty()
}
