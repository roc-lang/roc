Types := [].{
	Node := [Leaf, Branch(Value)]

	Value := { node : Node }

	Tree := [Leaf, Node({ left : Tree, right : Tree })]

	Chain := [Nil, Cons(Chain)]

	Wrapper(a) := a
}
