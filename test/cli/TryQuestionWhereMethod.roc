TryQuestionWhereMethod := {}

# `?` on a `where` method's result, with implementations that construct their
# result, forward a closed top-level value, and forward a payload.

load : a -> Try(Str, [NotFound, Other]) where [a.fetch : a -> Try(Str, [NotFound])]
load = |x| {
    s = x.fetch()?
    if s == "" Err(Other) else Ok(s)
}

closed_miss : Try(Str, [NotFound])
closed_miss = Err(NotFound)

Hit := [H].{
    fetch : Hit -> Try(Str, [NotFound])
    fetch = |_| Ok("hit")
}

Miss := [M].{
    fetch : Miss -> Try(Str, [NotFound])
    fetch = |_| closed_miss
}

Fwd := [F(Try(Str, [NotFound]))].{
    fetch : Fwd -> Try(Str, [NotFound])
    fetch = |f| match f { F(t) => t }
}

show : Try(Str, [NotFound, Other]) -> Str
show = |r| match r { Ok(s) => "Ok(${s})", Err(NotFound) => "NotFound", Err(Other) => "Other" }

expect show(load(Hit.H)) == "Ok(hit)"
expect show(load(Miss.M)) == "NotFound"
expect show(load(Fwd.F(Ok("")))) == "Other"
expect show(load(Fwd.F(Err(NotFound)))) == "NotFound"
