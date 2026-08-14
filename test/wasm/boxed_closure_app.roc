app [main!] { pf: platform "./platform/main.roc" }

main! = || Box.unbox(Box.box(|| "ok"))()
