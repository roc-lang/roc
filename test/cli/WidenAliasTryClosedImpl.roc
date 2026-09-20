IoResult(a) : Try(a, [IoErr])

load : a -> Try(Str, [IoErr, Other]) where [a.fetch : a -> IoResult(Str)]
load = |x| {
    s = x.fetch()?
    Ok(s)
}

closed_try : IoResult(Str)
closed_try = Ok("hit")

Src := [S].{
    fetch : Src -> IoResult(Str)
    fetch = |_| closed_try
}

expect match load(Src.S) { Ok(s) => s == "hit", Err(_) => False }
