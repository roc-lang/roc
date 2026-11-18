# Repro for https://github.com/roc-lang/roc/issues/13061
# Reading the inline discriminant after a field take must remain valid.
Describe := {}.{
    Kind : [Plain, Named(Str)]

    suffix : Kind -> Str
    suffix = |kind| match kind {
        Named(n) => n
        Plain => "plain"
    }

    describe : { name : Str, kind : Kind } -> Str
    describe = |t| {
        part = suffix(t.kind)
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
