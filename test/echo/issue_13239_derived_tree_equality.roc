# https://github.com/roc-lang/roc/issues/13239
# Derived equality of a recursive tag union whose variant holds the recursive
# child beside a scalar. The arm takes the child's box out of the union and
# releases it, then reads the scalar from the union's inline bytes.

Tree := [Leaf, Node(Tree, U64)].{
	is_eq : _
}

main! = |args| {
	count = args.len()
	left = Tree.Node(Tree.Node(Tree.Leaf, count), 1)
	right = Tree.Node(Tree.Node(Tree.Leaf, count + 1), 1)
	same = Tree.Node(Tree.Node(Tree.Leaf, count), 1)
	echo!("${Str.inspect(left == right)} ${Str.inspect(left == same)}\n")
	Ok({})
}
