# A generic function over a type that recurses through a list. Under
# `--specialize=no` its one worker composes the descriptor of `Tree(a)` from
# the hidden descriptor of `a` at runtime, and the composition must break the
# cycle at the list element, which is a pointer, instead of expanding `Tree(a)`
# inside its own node.
Tree(a) := [Leaf(a), Node(List(Tree(a)))]

count : Tree(a) -> U64
count = |tree| match tree {
	Leaf(_) => 1
	Node(children) => List.fold(children, 0, |total, child| total + count(child))
}

depth : Tree(a) -> U64
depth = |tree| match tree {
	Leaf(_) => 1
	Node(children) => 1 + List.fold(children, 0, |deepest, child| U64.max(deepest, depth(child)))
}

main! = |_args| {
	tree : Tree(Str)
	tree = Node([Leaf("a"), Node([Leaf("b"), Leaf("c")]), Leaf("d")])
	single : Tree(U64)
	single = Leaf(42)
	echo!("${Str.inspect(count(tree))} ${Str.inspect(depth(tree))} ${Str.inspect(count(single))}")
	Ok({})
}
