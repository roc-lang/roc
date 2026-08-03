import Foo exposing [Foo]

Bar := [].{
	build : List(Str) -> Foo
	build = |model|
		Foo.wrap({
			items: [
				Foo.Node(
					[Foo.Qux.Wrap(Box.box(|_v| "made"))],
					model.map(|_s| Foo.Node([], [])),
				),
			],
		})
}
