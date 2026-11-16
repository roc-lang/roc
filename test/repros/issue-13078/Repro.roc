Repro := {}.{}

f : U64 -> List(U64)
f = |x| {
    { rest } = { rest: [] }
    var $rest = rest
    $rest = $rest.append(x)
    $rest = $rest.append(x)
    out = $rest
    out
}
