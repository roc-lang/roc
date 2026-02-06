app [main!] { pf: platform "../fx/platform/main.roc" }

import pf.Stdout

findPair = |targetSum, maxVal| {
    var outer = 0
    while outer < maxVal {
        var inner = 0
        while inner < maxVal {
            if outer + inner == targetSum {
                return (outer, inner)
            }
            inner = inner + 1
        }
        outer = outer + 1
    }
    (-1, -1)
}

main! = || {
    (a, b) = findPair 10 20

    if a + b == 10 && a >= 0 && b >= 0 {
        Stdout.line! "SUCCESS: nested while with early return found valid pair"
    } else {
        Stdout.line! "FAILURE: nested while with early return did not work"
    }
}
