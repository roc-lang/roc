# Closed functions whose bodies fold to constants: records, lists of records,
# strings inside constants, and a lookup that indexes a constant table, so
# entries served from the object cache carry the constants they point at.
Tables :: [].{
	Entry : { name : Str, weight : U64, tags : List(Str) }

	entries : {} -> List(Entry)
	entries = |{}| [
		{ name: "alpha", weight: 10, tags: ["first", "greek"] },
		{ name: "beta", weight: 20, tags: ["second"] },
		{ name: "gamma", weight: 30, tags: [] },
	]

	squares : {} -> List(U64)
	squares = |{}| List.map([1, 2, 3, 4, 5, 6, 7, 8, 9, 10], |n| n * n)

	weight_of : U64 -> U64
	weight_of = |index| {
		match List.get(entries({}), index) {
			Ok(entry) => entry.weight
			Err(_) => 0
		}
	}

	banner : {} -> Str
	banner = |{}| Str.join_with(List.map(entries({}), |entry| entry.name), ",")
}
