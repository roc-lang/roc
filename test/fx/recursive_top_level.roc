app [main!] { pf: platform "./platform/main.roc" }

countdown = |n| if True { 0 } else { countdown(n - 1) }

main! = || {
    _result = countdown(1)
}
