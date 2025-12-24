app [main!] { pf: platform "./platform/main.roc" }

import pf.Stdout

get_pair = |n| {
    ("word", n + 1)
}

main! = || {
    var $index = 0
    while $index < 3 {
        (word, $index) = get_pair($index)
        Stdout.line!(word)
    }
}
