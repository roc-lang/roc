app [main!] { pf: platform "./platform/main.roc" }

import pf.Stdout
import pf.Host

# Regression test: early return with live refcounted symbols.
# When early_return is taken, all live RC symbols in the enclosing scope
# must be cleaned up (decreffed). Catches bugs where incref'd symbols
# leak when the early return path is taken.

try_get : List(Str) -> Try(Str, [ListWasEmpty])
try_get = |list| {
    s1 = "hello"
    s2 = "world"
    first = List.first(list)?
    Ok("${first} ${s1} ${s2}")
}

main! = || {
    runtime = Host.get_greeting!(Host.new("early"))
    values = if Str.count_utf8_bytes(runtime) > 0 { [] } else { ["unused"] }

    match try_get(values) {
        Ok(val) => Stdout.line!(val)
        Err(ListWasEmpty) => Stdout.line!("empty")
    }
}
