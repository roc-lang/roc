app [main!] { pf: platform "./pf_i32/main.roc" }

main! : () => List(I32)
main! = || {
    lo : I32
    lo = 0
    hi : I32
    hi = 14
    lo.to(hi).to_list()
}
