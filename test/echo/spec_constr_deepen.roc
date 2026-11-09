# A function that matches its tree argument and passes it on rebuilt one
# constructor deeper. Each recursive call has a new, strictly deeper call
# pattern, so call-pattern specialization must stop on its per-function worker
# budget rather than on the patterns running out.

Tree := [Leaf, Node(U64, Tree)]

deepen : Tree, U64 -> Tree
deepen = |t, n| match t {
    Leaf => if n == 0 Leaf else deepen(Node(n, Leaf), n - 1)
    Node(_, _) => if n == 0 t else deepen(Node(n, t), n - 1)
}

total : Tree -> U64
total = |t| match t {
    Leaf => 0
    Node(v, rest) => v + total(rest)
}

main! = |args| {
    # The depth depends on the argument count so the recursion is not
    # evaluated at compile time.
    depth = 9 + List.len(args)
    echo!(total(deepen(Leaf, depth)).to_str())
    Ok({})
}
