OpenMaker := [].{
	make = |key| { key, content: "m" }

	read = |{ key, content }| Str.concat(key.to_str(), content)

	listed = |key| [{ key, content: "l" }]
}
