# Producer calls written out of layout order (`cursor` precedes `points`) in a
# record literal and a record update still evaluate in source order, while
# optimized builds treat each producer exactly as a constructor field.
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
init = |n| { points: make_points(n), cursor: next_cursor(n) }

step : Model -> Model
step = |model| { ..model, points: grow(model.points, model.cursor), cursor: next_cursor(model.cursor) }

main! = |args| {
	echo!(Str.inspect(step(init(List.len(args) + 2))))
	Ok({})
}
