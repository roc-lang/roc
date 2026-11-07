RecursionKeepsTypeVariables := {}

# Ordinary recursion in a generic function: the type variable is passed along
# unchanged.
countdown : U64, a -> a
countdown = |count, x| if count == 0 x else countdown(count - 1, x)

# Recursion at a closed type from a generic function.
closed_restart : U64, a -> U64
closed_restart = |count, _x| if count == 0 0 else 1 + closed_restart(count - 1, 5.U64)

# Mutual recursion between two generic functions whose variables line up.
Tree(a) := [Leaf(a), Node(List(Tree(a)))]

count_tree : Tree(a) -> U64
count_tree = |tree| match tree {
	Tree.Leaf(_) => 1
	Tree.Node(children) => count_children(children)
}

count_children : List(Tree(a)) -> U64
count_children = |children| {
	var $total = 0
	for child in children {
		$total = $total + count_tree(child)
	}
	$total
}

# Recursion through a function value.
apply : (a -> b), a -> b
apply = |f, x| f(x)

through_apply : U64, a -> a
through_apply = |count, x| if count == 0 x else apply(|y| through_apply(count - 1, y), x)

# A generic function that uses another generic function at a larger type,
# with no recursion.
wrap_len : a -> U64
wrap_len = |x| List.len([x])

# A method that calls itself through method syntax on the same type.
Rose(a) := [Rose(a, List(Rose(a)))].{
	size : Rose(a) -> U64
	size = |Rose.Rose(_, children)| {
		var $total = 1
		for child in children {
			$total = $total + child.size()
		}
		$total
	}
}

expect countdown(3, "x") == "x"
expect closed_restart(3, "x") == 3
expect count_tree(Tree.Node([Tree.Leaf(1.U8), Tree.Node([Tree.Leaf(2), Tree.Leaf(3)])])) == 3
expect through_apply(3, 7.U64) == 7
expect wrap_len("x") == 1
expect Rose.Rose(1.U8, [Rose.Rose(2, [])]).size() == 2
