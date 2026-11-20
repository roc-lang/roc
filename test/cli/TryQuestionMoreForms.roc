TryQuestionMoreForms := {}

# `?` through an alias of `Try` on a `where` method, `? Tag` on bounded value
# rows, in a lambda nested in an annotated function, on a record field, on a
# pattern-bound value, and after an optional field access.

Res(e) : Try(Str, e)

load : a -> Res([NotFound, Other]) where [a.fetch : a -> Res([NotFound])]
load = |x| {
    s = x.fetch()?
    if s == "" Err(Other) else Ok(s)
}

Src := [S(Str)].{
    fetch : Src -> Res([NotFound])
    fetch = |src| match src { S(s) => if s == "miss" Err(NotFound) else Ok(s) }
}

closed_top : Try(U64, [BadNum])
closed_top = Err(BadNum)

mapped : Bool -> Try(U64, [Wrapped([BadNum]), Empty])
mapped = |b| {
    local : Try(U64, [BadNum])
    local = if b Ok(0) else Err(BadNum)
    x = local ? Wrapped
    y = if b x else closed_top ? Wrapped
    if y == 0 Err(Empty) else Ok(y)
}

nested : List(Try(U64, [BadNum])) -> Try(List(U64), [BadNum, Empty])
nested = |items| {
    step = |t| {
        n = t?
        if n == 0 Err(Empty) else Ok(n)
    }
    first = step(List.first(items) ?? Ok(1))?
    Ok([first])
}

field : { f : Try(U64, [BadNum]) } -> Try(U64, [BadNum, Empty])
field = |r| {
    n = r.f?
    if n == 0 Err(Empty) else Ok(n)
}

pattern : [Wrap(Try(U64, [BadNum]))] -> Try(U64, [BadNum, Empty])
pattern = |w| match w {
    Wrap(t) => {
        n = t?
        if n == 0 Err(Empty) else Ok(n)
    }
}

Attrs : { count : U64, label ?: Str }

optional : Attrs -> Try(Str, [MissingField, Empty])
optional = |a| {
    l = a.?label?
    if l == "" Err(Empty) else Ok(l)
}

show : Res([NotFound, Other]) -> Str
show = |r| match r { Ok(s) => "Ok(${s})", Err(NotFound) => "NotFound", Err(Other) => "Other" }

showw : Try(U64, [Wrapped([BadNum]), Empty]) -> Str
showw = |r| match r { Ok(n) => "Ok(${n.to_str()})", Err(Wrapped(BadNum)) => "Wrapped", Err(Empty) => "Empty" }

shown : Try(U64, [BadNum, Empty]) -> Str
shown = |r| match r { Ok(n) => "Ok(${n.to_str()})", Err(BadNum) => "BadNum", Err(Empty) => "Empty" }

expect show(load(Src.S("miss"))) == "NotFound"
expect show(load(Src.S(""))) == "Other"
expect show(load(Src.S("a"))) == "Ok(a)"
expect showw(mapped(Bool.True)) == "Empty"
expect showw(mapped(Bool.False)) == "Wrapped"
expect match nested([Err(BadNum)]) { Err(BadNum) => Bool.True, _ => Bool.False }
expect match nested([Ok(0)]) { Err(Empty) => Bool.True, _ => Bool.False }
expect shown(field({ f: Err(BadNum) })) == "BadNum"
expect shown(field({ f: Ok(0) })) == "Empty"
expect shown(pattern(Wrap(Err(BadNum)))) == "BadNum"
expect shown(pattern(Wrap(Ok(3)))) == "Ok(3)"
expect match optional({ count: 1 }) { Err(MissingField) => Bool.True, _ => Bool.False }
expect match optional({ count: 1, label: "" }) { Err(Empty) => Bool.True, _ => Bool.False }
