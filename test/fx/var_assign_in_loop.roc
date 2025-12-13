app [main!] { pf: platform "./platform/main.roc" }

import pf.Stdout

main! = || {
    calc!([8, 7, 0])
}

calc! = |line| {
    var $num = 0
    for num in List.drop_last(line, 1) {
        $num = num
        Stdout.line!($num.to_str())
    }
}
