app [main!] { pf: platform "./platform/main.roc" }

import pf.Stdout

# Test that functions returning recursive nominal types work correctly.
# This tests the fix for issue #9044 which caused stack overflow when
# returning a recursive nominal type from a function.
main! = || {
    leaf = make_leaf({})
    node = make_node(leaf)
    nested = make_node(node)
    match nested {
        Leaf => Stdout.line!("Got Leaf (unexpected)")
        Node(inner) =>
            match inner {
                Leaf => Stdout.line!("Got Node(Leaf) (unexpected)")
                Node(_) => Stdout.line!("Got Node(Node(_)) (expected)")
            }
    }
}

Value := [
    Leaf,
    Node(Value),
]

make_leaf : {} -> Value
make_leaf = |{}| Leaf

make_node : Value -> Value
make_node = |child| Node(child)
