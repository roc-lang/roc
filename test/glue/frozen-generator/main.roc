app [make_glue] { pf: platform glue }

import pf.Types
import pf.File

# All three files read completed constants at runtime, not just inline literals.
long_text : Str
long_text = "a frozen string longer than the inline Roc string representation"

shared : List(Str)
shared = [long_text, "end"]
nested : List({ values : List(Str) })
nested = [{ values: shared }, { values: shared }]
zeros : List(U8)
zeros = List.repeat(0, 65536)

capture = |text| |suffix| text.concat(suffix)
callbacks : List(Str -> Str)
callbacks = [capture(long_text), capture("other captured text longer than an inline string")]

make_glue : List(Types) -> Try(List(File), Str)
make_glue = |types| {
    index = if types.len() > 0 0 else 1
    row = match List.get(nested, index) { Ok(value) => value Err(_) => crash "missing row" }
    text = match List.get(row.values, 0) { Ok(value) => value Err(_) => crash "missing string" }
    callback = match List.get(callbacks, index) { Ok(value) => value Err(_) => crash "missing callback" }
    changed = row.values.append("new")
    zero = match List.get(zeros, index) { Ok(value) => value Err(_) => crash "missing zero" }
    if changed.len() != 3 or shared.len() != 2 or zero != 0 {
        crash "frozen backing changed"
    }
    Ok([
        File.{ name: "long.txt", content: long_text },
        File.{ name: "nested.txt", content: text },
        File.{ name: "callable.txt", content: callback("!") },
    ])
}
