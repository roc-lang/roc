Types := [].{
	Wrap(m) := { model : m, n : U64 }

	Pair(a, b) := { first : a, second : b }

	Opt(a) := [None, Some(a)]

	## Recursive, so telling two instantiations apart cannot compare them
	## field by field.
	Tree(a) := [Leaf(a), Node(Box(Tree(a)))]

	## Reaches `Wrap(a)` from inside another generic nominal's backing.
	Outer(a) := { inner : Wrap(a), opt : Opt(a) }
}
