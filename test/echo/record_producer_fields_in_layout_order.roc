# The same producer calls as record_producer_fields_out_of_layout_order.roc,
# written in layout order: they evaluate in that order and build the same
# records.
Model : { points : List(U64), cursor : U64 }

make_points : U64 -> List(U64)
make_points = |n| {
	dbg "points"
	List.repeat(n, n)
}

next_cursor : U64 -> U64
next_cursor = |n| {
	dbg "cursor"
	n + 1
}

grow : List(U64), U64 -> List(U64)
grow = |points, item| {
	dbg "grow"
	List.append(points, item)
}

init : U64 -> Model
init = |n| { cursor: next_cursor(n), points: make_points(n) }

step : Model -> Model
step = |model| { ..model, cursor: next_cursor(model.cursor), points: grow(model.points, model.cursor) }

main! = |args| {
	echo!(Str.inspect(step(init(List.len(args) + 2))))
	Ok({})
}
