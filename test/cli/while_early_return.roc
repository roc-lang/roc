app [main!] { pf: platform "../fx/platform/main.roc" }

import pf.Stdout

findValue = |limit| {
    var i = 0
    while i < 100 {
        if i == limit {
            return i
        }
        i = i + 1
    }
    -1
}

main! = || {
    result = findValue 42

    if result == 42 {
        Stdout.line! "SUCCESS: early return found value 42"
    } else {
        Stdout.line! "FAILURE: early return did not work correctly"
    }
}
