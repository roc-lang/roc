package [] {}

Tree := [Node(Tree)].{
	to_list : Tree -> List(U64)
	to_list = |tree| {
		match tree {
			Node(left) => left.to_list().append(1)
		}
	}
}
