DemandSpecialization := {}

# Builtins that only read a list's length share one specialization across
# element types; those that move elements share one per element layout.
# Every element type below must still behave like itself.

Point : { x : U64, y : Str }

expect {
	strs = ["a", "bb", "ccc"]
	nums = [1.U8, 2, 3, 4]
	points : List(Point)
	points = [{ x: 1, y: "one" }, { x: 2, y: "two" }]
	nested = [[1.I64], [], [2, 3]]
	List.len(strs) == 3 and List.len(nums) == 4 and List.len(points) == 2 and List.len(nested) == 3
}

expect {
	empty_strs : List(Str)
	empty_strs = []
	fns : List((U64 -> U64))
	fns = [|x| x + 1, |x| x * 2]
	empty_strs.is_empty() and !fns.is_empty() and List.len(fns) == 2
}

expect {
	points : List(Point)
	points = [{ x: 1, y: "one" }, { x: 2, y: "two" }]
	pairs = [(1.U64, "one"), (2, "two")]
	got_point = List.get(points, 1) == Ok({ x: 2, y: "two" })
	got_pair = List.get(pairs, 0) == Ok((1, "one"))
	missing = List.get(pairs, 5) == Err(OutOfBounds)
	got_point and got_pair and missing
}

expect {
	points : List(Point)
	points = [{ x: 1, y: "one" }]
	pairs = [(1.U64, "one")]
	more_points = points.append({ x: 3, y: "three" })
	more_pairs = pairs.append((3, "three"))
	more_points == [{ x: 1, y: "one" }, { x: 3, y: "three" }] and more_pairs == [(1, "one"), (3, "three")]
}

expect {
	words = ["a", "b"]
	set_words = List.set(words, 1, "z")
	set_words == Ok(["a", "z"])
}

expect {
	boxed_point = Box.box({ x: 7.U64, y: "seven" })
	boxed_pair = Box.box((7.U64, "seven"))
	Box.unbox(boxed_point).y == "seven" and Box.unbox(boxed_pair).1 == "seven"
}

expect {
	fns : List((U64 -> U64))
	fns = [|x| x + 1, |x| x * 2]
	applied = List.map(fns, |f| f(10))
	applied == [11, 20] and List.len(fns) == 2
}

# A list of zero-sized items has a representation of its own, so it never
# shares a specialization with lists of sized items.
expect {
	units : List({})
	units = [{}, {}, {}]
	strs = ["a", "b"]
	List.len(units) == 3 and List.capacity(units) == 0 and List.len(strs) == 2 and List.capacity(strs) >= 2
}

expect {
	units : List({})
	units = [{}, {}, {}]
	more = List.concat(units, [{}, {}])
	List.len(more) == 5 and List.capacity(more) == 0
}

# A parser's field-name table follows its shape's field names, not just
# its layout.
Item : U64

count_items : Str -> U64
count_items = |json| {
	parsed : Try({ items : List(Item) }, _)
	parsed = Json.parse(json)
	match parsed {
		Ok(record) => record.items.len()
		Err(_) => 0
	}
}

expect count_items("{\"items\":[1,2]}") == 2
