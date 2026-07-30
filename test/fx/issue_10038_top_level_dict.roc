# Repro for https://github.com/roc-lang/roc/issues/10038
app [main!] { pf: platform "./platform/main.roc" }

import pf.Stdout
import pf.Stdin

top_dict = Dict.single("a", "b")

main! = || {
    key = Stdin.line!()

    match Dict.get(top_dict, key) {
        Ok(value) => Stdout.line!(value)
        Err(_) => Stdout.line!("missing")
    }
}
