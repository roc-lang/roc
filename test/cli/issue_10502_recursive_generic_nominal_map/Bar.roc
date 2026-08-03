# repro for https://github.com/roc-lang/roc/issues/10502
Bar(msg) := [
    Node(msg, List(Bar(msg))),
].{
    map : Bar(a), (a -> b) -> Bar(b)
    map = |bar, f|
        match bar {
            Node(m, children) =>
                Node(f(m), children.map(|child| Bar.map(child, f)))
        }
}

expect
    match Bar.Node("a", []).map(|s| s) {
        Node(_, _) => Bool.True
    }
