app [main!] { pf: platform "../../fx/platform/main.roc" }

import Static
import pf.Stdin
import pf.Stdout

main! = || {
    index = Str.count_utf8_bytes(Stdin.line!())
    Stdout.line!((Static.read(index) + Static.guarded(index)).to_str())
}
