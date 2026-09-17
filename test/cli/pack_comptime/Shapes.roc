# Closed functions a compile-time root reaches, so a rebuild served from the
# object cache splices their entries into the compile-time evaluator's image:
# a list built by a closure, a lookup through a match, a string built from a
# record table, and a Dict, whose seed the evaluator fixes.
Shapes :: [].{
	Shape : { name : Str, sides : U64 }

	catalog : {} -> List(Shape)
	catalog = |{}| [
		{ name: "triangle", sides: 3 },
		{ name: "square", sides: 4 },
		{ name: "pentagon", sides: 5 },
		{ name: "hexagon", sides: 6 },
	]

	sides_of : Str -> U64
	sides_of = |name| {
		match List.find_first(catalog({}), |shape| shape.name == name) {
			Ok(shape) => shape.sides
			Err(_) => 0
		}
	}

	perimeters : U64 -> List(U64)
	perimeters = |side| List.map(catalog({}), |shape| shape.sides * side)

	roster : {} -> Str
	roster = |{}| Str.join_with(List.map(catalog({}), |shape| "${shape.name}:${U64.to_str(shape.sides)}"), " ")

	# Produces floats, so the evaluator lowers it itself: cached code keeps
	# the machine's NaN bits where the evaluator normalizes them.
	mean_sides : {} -> F64
	mean_sides = |{}| {
		total = List.sum(List.map(catalog({}), |shape| shape.sides))
		U64.to_f64(total) / U64.to_f64(List.len(catalog({})))
	}

	side_index : {} -> Dict(Str, U64)
	side_index = |{}| Dict.from_list(List.map(catalog({}), |shape| (shape.name, shape.sides)))
}
