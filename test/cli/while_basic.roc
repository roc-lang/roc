app [main!] { pf: platform "../fx/platform/main.roc" }

import pf.Stdout

main! = || {
    var count = 0
    while count < 5 {
        count = count + 1
    }

    if count == 5 {
        Stdout.line! "SUCCESS: count reached 5"
    } else {
        Stdout.line! "FAILURE: count did not reach 5"
    }
}
