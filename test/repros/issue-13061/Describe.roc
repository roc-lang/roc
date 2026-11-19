# Repro for https://github.com/roc-lang/roc/issues/13061
# Reading the inline discriminant after a whole-record call must remain valid.
Describe := {}.{
    Kind : [Plain, Named(Str)]

    keep : { name : Str, kind : Kind } -> Str
    keep = |t| match t.kind {
        Named(n) => n
        Plain => t.name
    }

    describe : { name : Str, kind : Kind } -> Str
    describe = |t| {
        part = keep(t)
        head = match t.kind {
            Named(_) => "named"
            Plain => "plain"
        }
        "${head} ${t.name} ${part}"
    }
}

expect {
    t = { name: "n", kind: Named("k") }
    Describe.describe(t) == "named n k"
}
