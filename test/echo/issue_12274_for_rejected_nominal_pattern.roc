# A `for` loop whose nominal pattern checking rejected (the pattern omits
# `age`). The loop crashes once reached; the rejected pattern binds nothing.
Person := { name : Str, age : U64 }

main! = |_| {
	for Person.{ name } in [Person.{ name: "Ada", age: 36 }] {
		echo!(name)
	}
	Ok({})
}
