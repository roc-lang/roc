Foo := [
	Node(List(Foo.Qux), List(Foo)),
].{
	Qux := [Wrap(Box(Str -> Str))]

	wrap : { items : List(Foo) } -> Foo
	wrap = |args|
		Node([], args.items)
}
