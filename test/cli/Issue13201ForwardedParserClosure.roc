# https://github.com/roc-lang/roc/issues/13201
Node := [Leaf(U8), Alt(List(Node)), And(List(Node))].{
    Out : { node : Node, i : U64 }

    node_of : List(Node), (List(Node) -> Node) -> Node
    node_of = |xs, wrap| if List.len(xs) == 1 { List.get(xs, 0) ?? Leaf(0) } else { wrap(xs) }

    parse_sep : List(U8), U64, List(Node), U8, (List(U8), U64 -> Node.Out), (List(Node) -> Node) -> Node.Out
    parse_sep = |toks, i, arms, sep, sub, wrap|
        if List.get(toks, i) == Ok(sep) {
            next = sub(toks, i + 1)
            Node.parse_sep(toks, next.i, List.append(arms, next.node), sep, sub, wrap)
        } else {
            { node: Node.node_of(arms, wrap), i }
        }

    parse_alt : List(U8), U64 -> Node.Out
    parse_alt = |toks, i| {
        first = Node.parse_and(toks, i)
        Node.parse_sep(toks, first.i, [first.node], '|', Node.parse_and, |xs| Alt(xs))
    }

    parse_and : List(U8), U64 -> Node.Out
    parse_and = |toks, i| {
        first = Node.parse_cat(toks, i)
        Node.parse_sep(toks, first.i, [first.node], '&', Node.parse_cat, |xs| And(xs))
    }

    parse_cat : List(U8), U64 -> Node.Out
    parse_cat = |toks, i| { node: Leaf(List.get(toks, i) ?? 0), i: i + 1 }

    show : Node -> Str
    show = |n|
        match n {
            Leaf(c) => Str.from_utf8_lossy([c])
            Alt(xs) => "Alt(${Str.join_with(List.map(xs, Node.show), ",")})"
            And(xs) => "And(${Str.join_with(List.map(xs, Node.show), ",")})"
        }
}

main! = |args| {
    # Keep the parse at runtime.
    suffix = if List.len(args) > 99 { "x" } else { "" }
    Ok(echo!(Node.show(Node.parse_alt(Str.to_utf8(Str.concat("a|s", suffix)), 0).node)))
}
