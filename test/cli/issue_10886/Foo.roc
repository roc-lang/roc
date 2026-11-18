Foo := { a : U64, b : U64 ?? 0 }.{
	is_eq : _
	to_hash : _
}

expect {
	set : Set(Foo)
	set = Set.from_list([{ a: 123 }, { a: 456, b: 789 }])
	set.len() == 2 and set.contains(Foo.{ a: 123, b: 0 }) and set.contains(Foo.{ a: 456, b: 789 })
}

expect {
	items : List(Foo)
	items = List.map(
		[True, False],
		|flag| {
			if flag {
				return { a: 123 }
			}
			{ a: 456, b: 789 }
		},
	)
	items == [Foo.{ a: 123, b: 0 }, Foo.{ a: 456, b: 789 }]
}
