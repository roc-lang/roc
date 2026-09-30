IoResult(a) : Try(a, [IoErr])

load : a -> Try(Str, [IoErr, Other]) where [a.fetch : a -> IoResult(Str)]
load = |x| {
    s = x.fetch()?
    Ok(s)
}

# `seal` forwards its closed input, which closes its output row, and so the
# row of every value built from it (design.md "Deferred: Row Subsumption").
# This depends on that known limitation (forwarding closes the row): once
# row subsumption lands, this fixture must close its impl row another way.
seal : IoResult(Str) -> IoResult(Str)
seal = |v| v

closed_try : IoResult(Str)
closed_try = seal(Ok("hit"))

Src := [S].{
    fetch : Src -> IoResult(Str)
    fetch = |_| closed_try
}

expect match load(Src.S) { Ok(s) => s == "hit", Err(_) => False }
