app [main!] { pf: platform "./platform/main.roc" }

# Repro for https://github.com/roc-lang/roc/issues/12230
#
# `Enc.encode` is monomorphic in the record it encodes, so every module that
# calls it and matches its result must share one specialization of it and of
# `Json.to_str`.

import Site1
import Site2
import Site3

main! = |args| {
    arg = List.first(args) ?? ""
    form = { name: arg, email: arg, phone: arg }
    out =
        match List.len(args) {
            1 => Site1.run(form)
            2 => Site2.run(form)
            _ => Site3.run(form)
        }
    if Str.is_empty(out) Err(Exit(1)) else Ok({})
}
