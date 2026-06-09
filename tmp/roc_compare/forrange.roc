app [main!] { pf: platform "./pf_i32/main.roc" }
main! : () => List(I32)
main! = || {
    var $sum = 0
    for i in 0.I32.to(14) {
        $sum = $sum + i
    }
    [$sum]
}
