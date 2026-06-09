app [main!] { pf: platform "./pf_i32/main.roc" }
main! : () => List(I32)
main! = || 0.I32.to(14).collect()
