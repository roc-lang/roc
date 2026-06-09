app [main!] { pf: platform "./pf_i32/main.roc" }
main! : () => List(I32)
main! = || {
    var $list = []
    for i in 0.to(14) {
        $list = $list.append(i)
    }
    $list
}
