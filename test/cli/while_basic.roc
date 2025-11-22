app [main!] { pf: platform "../basic-cli/main.roc" }

import pf.Stdout

main! = |_| {
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
