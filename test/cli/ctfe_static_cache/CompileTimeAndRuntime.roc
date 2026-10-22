app [main!] { pf: platform "../../fx/platform/main.roc" }

import Static
import pf.Stdin
import pf.Stdout

seeded : I64
seeded = Static.read(3)

main! = || {
    index = Str.count_utf8_bytes(Stdin.line!())
    Stdout.line!((Static.read(index) + seeded).to_str())
}
