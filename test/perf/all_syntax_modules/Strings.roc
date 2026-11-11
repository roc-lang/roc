import Leaf
import Shared

Strings := [].{
	multiline_str : U64 -> Str
	multiline_str = |number|
		\\Line 1
		\\Line 2
		\\Line ${number.to_str()}

	my_concat = Str.concat

	# Chaining static dispatch methods with a lambda
	format_names : List(Str) -> Str
	format_names = |names|
		names
			.map(|name| name.trim())
			|> Str.join_with(", ")
			|> (|joined| {
				if (joined.is_empty()) "No names provided" else Shared.label("Names", joined)
			})

	greeting : Str -> Str
	greeting = |name| "${Leaf.greet(name)} (${(Leaf.perf_edit + Shared.perf_edit).to_str()})"
}
