app [main!] { pf: platform "./platform/main.roc" }

import pf.Stdin
import pf.Stdout

main! = || {
    # Read lines until empty string
    var $count = 0
    var $continue = True
    while $continue {
        line = Stdin.line!()
        Stdout.line!(line)
        $count = $count + 1
        if line == "" {
            $continue = False
        }
    }
}
