# https://github.com/roc-lang/roc/issues/13217
app [main!] { pf: platform "./platform/main.roc" }
import pf.Stdin
import pf.Stdout

# The record has the same physical shape as the recursive Branch payload.
Tree := [Leaf(Str), Branch(U64, Tree)]

wrap : a -> Try({ next : U64, value : a }, [Invalid(U64)])
wrap = |value| Ok({ next: 1, value })

round_trip : a, U64 -> Try(a, [Invalid(U64)])
round_trip = |value, limit| {
    record = wrap(value)?
    if record.next == limit Ok(record.value) else Err(Invalid(record.next))
}

main! = || {
    input = Stdin.line!()
    parsed : Try(Tree, _)
    parsed = round_trip(Tree.Leaf(input), 1)
    match parsed {
        Ok(Leaf(s)) => Stdout.line!(s)
        _ => Stdout.line!("unexpected result")
    }
    rejected : Try(Tree, _)
    rejected = round_trip(Tree.Leaf(input), 2)
    Stdout.line!(if rejected.is_err() input else "unexpected success")
}
