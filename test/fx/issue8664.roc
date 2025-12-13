app [main!] { pf: platform "./platform/main.roc" }

import pf.Stdout

# Test for issue #8664: InvalidMethodReceiver crash
# The bug occurs when calling .to_str() on elements from a list parameter
# that doesn't have an explicit type annotation.

# No type annotation on calc - the parameter `line` has inferred type
calc = |line| {
    var $num = 0
    for num in List.drop_last(line, 1) {
        $num = num
        Stdout.line!("num: ${num.to_str()}")
    }
    $num
}

main! = || {
    line = [8, 7, 6]
    num = calc(line)
    Stdout.line!("result: ${num.to_str()}")
}
