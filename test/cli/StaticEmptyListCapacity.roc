# Each value below is a compile-time constant frozen into static data. An
# empty list evaluated with spare capacity keeps it, so it restores as
# `List.with_capacity(n)`; frozen into a record, it used to point at read-only
# memory with capacity 0, and the first prepend crashed `roc test` on the dev
# backend.
computed_empty = { first: "a", more: ["a"].drop_first(1) }

reserved_empty = { first: "a", more: List.with_capacity(4) }

literal_empty = { first: "a", more: [] }

computed_nonempty = { first: "a", more: ["a", "c"].drop_first(1) }

# The same list nested deeper: in a tuple, a tag's payload, a nominal record
# and a recursive nominal.
Bag := { name : Str, items : List(Str) }

Tree := [Node(Str, List(Tree))]

in_tuple = ("a", List.with_capacity(4))

maybe : [Some(List(Str)), None]
maybe = Some(List.with_capacity(4))

in_tag = { first: "a", inner: maybe }

in_nominal : Bag
in_nominal = { name: "bag", items: List.with_capacity(4) }

leaf : Tree
leaf = Node("leaf", List.with_capacity(3))

expect computed_empty.more.prepend("b") == ["b"]
expect reserved_empty.more.prepend("b") == ["b"]
expect literal_empty.more.prepend("b") == ["b"]
expect computed_nonempty.more.prepend("b") == ["b", "c"]
expect in_tuple.1.prepend("b") == ["b"]
expect match in_tag.inner {
	Some(l) => l.prepend("b") == ["b"]
	None => Bool.False
}
expect match in_nominal {
	{ items, .. } => items.prepend("b") == ["b"]
}
expect match leaf {
	Node(_, kids) => kids.prepend(leaf).len() == 1
}
