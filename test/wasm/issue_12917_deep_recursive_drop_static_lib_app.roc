app [main!] { pf: platform "./static-lib-platform/main.roc" }

# Values of recursive types built in a loop, far deeper than one native stack
# frame per level allows, and then dropped. The depth depends on `seed` so
# that the values are built at runtime.

V := [Leaf, Node(List(V))]

nested_lists : U64 -> V
nested_lists = |n| {
    var $v = V.Leaf
    var $i = 0
    while $i < n {
        $v = V.Node([$v])
        $i = $i + 1
    }
    $v
}

Tree := [Tip, Fork(Tree, Tree)]

spine : U64 -> Tree
spine = |n| {
    var $t = Tree.Tip
    var $i = 0
    while $i < n {
        $t = if $i % 2 == 0 { Tree.Fork($t, Tree.Tip) } else { Tree.Fork(Tree.Tip, $t) }
        $i = $i + 1
    }
    $t
}

Nested := { name : Str, children : List(Nested) }

nested_records : U64 -> Nested
nested_records = |n| {
    var $v = { name: "a name long enough to live on the heap", children: [] }
    var $i = 0
    while $i < n {
        $v = { name: "a name long enough to live on the heap", children: [$v, { name: "a sibling long enough to live on the heap", children: [] }] }
        $i = $i + 1
    }
    $v
}

main! = |seed| {
    n = 50_000 + seed
    v = nested_lists(n)
    t = spine(n)
    r = nested_records(n)
    lists =
        match v {
            Leaf => False
            Node(_) => True
        }
    trees =
        match t {
            Tip => False
            Fork(_, _) => True
        }
    if lists and trees and r.children.len() == 2 "ok" else "bad"
}
