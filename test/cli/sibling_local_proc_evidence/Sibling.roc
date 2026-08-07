Sibling := [].{
	run : a -> a
	run = |value| {
		first = |{}| value
		second = |{}| first({})

		second({})
	}

	nested : a -> a
	nested = |value| {
		enclosing = |{}| {
			first = |{}| value
			second = |{}| first({})

			second({})
		}

		enclosing({})
	}
}

expect Sibling.run(42) == 42

expect Sibling.nested(43) == 43
