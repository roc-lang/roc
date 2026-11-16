# The widening dispatch sits inside a generalized local (`widen`), and the
# row it widens is the error row of a `Try`, whose tail the requirement's
# annotation leaves to each instantiation. `forward` and `repeat` hand their
# own evidence on at the same requirement, to another definition and around
# a recursive call; `describe` hands it to the local at a row two tags wider.
# `Aaa` sorts before `Bad`, so an `Err(Bad(_))` numbered by the
# implementation's row alone shows up as a wrong tag through `show`.
WidenClosedTryImplLocalScope := {}

Job := [Pending, Failed].{
    status : Job -> Try(Str, [Bad(Str)])
    status = |job| match job {
        Pending => Ok("cv")
        Failed => Err(Bad("broken"))
    }
}

show : Try(Str, [Bad(Str), Aaa, Zzz]) -> Str
show = |v| match v {
    Ok(s) => "Ok(${s})"
    Err(Bad(e)) => "Bad(${e})"
    Err(Aaa) => "Aaa"
    Err(Zzz) => "Zzz"
}

describe : a -> Str where [a.status : a -> Try(Str, [Bad(Str)])]
describe = |x| {
    widen = |y| y.status()
    show(widen(x))
}

forward : a -> Str where [a.status : a -> Try(Str, [Bad(Str)])]
forward = |x| describe(x)

repeat : a, U64 -> Str where [a.status : a -> Try(Str, [Bad(Str)])]
repeat = |x, remaining| if remaining == 0 forward(x) else repeat(x, remaining - 1)

expect describe(Job.Pending) == "Ok(cv)"
expect describe(Job.Failed) == "Bad(broken)"
expect repeat(Job.Pending, 3) == "Ok(cv)"
expect repeat(Job.Failed, 3) == "Bad(broken)"
