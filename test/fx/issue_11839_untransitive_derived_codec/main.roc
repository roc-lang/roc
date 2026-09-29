app [main!] { pf: platform "../platform/main.roc" }

import pf.Stdout
import Wrap

main! = || {
    decoded : Try(Wrap, [InvalidJson(Str)])
    decoded = Json.parse("\"Off\"")
    Stdout.line!("${Json.to_str(Wrap.W)} ${Str.inspect(decoded)}")
}
