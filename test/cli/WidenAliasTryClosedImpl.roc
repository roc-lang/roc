IoResult(a) : Try(a, [IoErr])

load : a -> Try(Str, [IoErr, Other]) where [a.fetch : a -> IoResult(Str)]
load = |x| {
    s = x.fetch()?
    Ok(s)
}

# `closed_value` is deliberately UNANNOTATED. An annotated value's implicitly
# opened row is quantified now (design.md "Polarity"), so an annotation can no
# longer produce a closed row at all. `closed` takes the row in an INPUT
# position, where it is generated as written, and returns it, so its result row
# is bound to `[]` by its own body: an input-position parameter is one of the
# closed sources design.md names. `closed_value` is therefore still a top-level
# constant whose row is closed, which is what this fixture needs.
closed : IoResult(Str) -> IoResult(Str)
closed = |v| v

closed_try = closed(Ok("hit"))

Src := [S].{
    fetch : Src -> IoResult(Str)
    fetch = |_| closed_try
}

expect match load(Src.S) { Ok(s) => s == "hit", Err(_) => False }
