# A requirement's dictionary handed on from an alias's scope, where the row
# the consumer widens is the error row of a `Try`. Each alias of the chain
# states the requirement one tag wider than the one before it, so every step
# of the chain hands its dictionary to a consumer naming another storage.
# `Aaa` sorts before `Bad`, so an `Err(Bad(_))` numbered by the
# implementation's row alone shows up as a wrong tag through `show`.
WidenClosedTryImplAliasScope := {}

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

apply = |f, v| f(v)

widen : c -> Try(Str, [Bad(Str), Aaa, Zzz]) where [c.status : c -> Try(Str, [Bad(Str), Aaa, Zzz])]
widen = |y| y.status()

mid : b -> Try(Str, [Bad(Str), Aaa, Zzz]) where [b.status : b -> Try(Str, [Bad(Str), Aaa])]
mid = widen

narrow : b -> Try(Str, [Bad(Str), Aaa, Zzz]) where [b.status : b -> Try(Str, [Bad(Str)])]
narrow = mid

describe : a -> Str where [a.status : a -> Try(Str, [Bad(Str)])]
describe = |x| show(narrow(x))

as_value : a -> Str where [a.status : a -> Try(Str, [Bad(Str)])]
as_value = |x| show(apply(narrow, x))

repeat : a, U64 -> Str where [a.status : a -> Try(Str, [Bad(Str)])]
repeat = |x, remaining| if remaining == 0 describe(x) else repeat(x, remaining - 1)

# The same chain's last step as a local alias of a generalized local.
local_alias : a -> Str where [a.status : a -> Try(Str, [Bad(Str)])]
local_alias = |x| {
    local_widen = |y| y.status()
    local_narrow : b -> Try(Str, [Bad(Str), Aaa, Zzz]) where [b.status : b -> Try(Str, [Bad(Str)])]
    local_narrow = local_widen
    show(local_narrow(x))
}

local_alias_value : a -> Str where [a.status : a -> Try(Str, [Bad(Str)])]
local_alias_value = |x| {
    local_widen = |y| y.status()
    local_narrow : b -> Try(Str, [Bad(Str), Aaa, Zzz]) where [b.status : b -> Try(Str, [Bad(Str)])]
    local_narrow = local_widen
    show(apply(local_narrow, x))
}

# A recursion that returns to itself through an alias restating its own
# requirement, and leaves through the chain above.
loop : a, U64 -> Str where [a.status : a -> Try(Str, [Bad(Str)])]
loop = |x, remaining| if remaining == 0 show(narrow(x)) else again(x, remaining - 1)

again = loop

expect loop(Job.Pending, 5000) == "Ok(cv)"
expect loop(Job.Failed, 5000) == "Bad(broken)"
expect show(narrow(Job.Pending)) == "Ok(cv)"
expect show(narrow(Job.Failed)) == "Bad(broken)"
expect describe(Job.Pending) == "Ok(cv)"
expect describe(Job.Failed) == "Bad(broken)"
expect as_value(Job.Pending) == "Ok(cv)"
expect as_value(Job.Failed) == "Bad(broken)"
expect repeat(Job.Pending, 3) == "Ok(cv)"
expect repeat(Job.Failed, 3) == "Bad(broken)"
expect local_alias(Job.Pending) == "Ok(cv)"
expect local_alias(Job.Failed) == "Bad(broken)"
expect local_alias_value(Job.Pending) == "Ok(cv)"
expect local_alias_value(Job.Failed) == "Bad(broken)"
