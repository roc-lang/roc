# Regression test for https://github.com/roc-lang/roc/issues/10527
# The optimized build must preserve every field supplied by the base record.

Scalar : F32

Size : { width : Scalar, height : Scalar }

Rect : { x : Scalar, y : Scalar, width : Scalar, height : Scalar }

Placement : { rect : Rect, clip_rect : Rect }

Layout(output) := {
	padding : Scalar,
	placers : List(Placement -> output),
	children : List(Layout(output)),
	dimensions : Size,
}

leaf : Size, (Placement -> output) -> Layout(output)
leaf = |intrinsic_size, place_leaf| Layout.(
	{
		padding: 0,
		placers: [place_leaf],
		children: [],
		dimensions: intrinsic_size,
	},
)

box : List(Layout(output)) -> Layout(output)
box = |children| Layout.(
	{
		padding: 0,
		placers: [],
		children,
		dimensions: { width: 0, height: 0 },
	},
)

dimensions_of : Layout(output) -> Size
dimensions_of = |Layout.(node)| node.dimensions

place_layout : Layout(output), Placement -> output
	where [output.default : output, output.plus : output, output -> output]
place_layout = |Layout.(node), placement| {
	Output : output
	var $output = Output.default()
	for placer in node.placers {
		$output = $output + placer(placement)
	}
	var $child_x = placement.rect.x + node.padding
	for child in node.children {
		child_size = dimensions_of(child)
		child_rect = { x: $child_x, y: placement.rect.y + node.padding, width: child_size.width, height: child_size.height }
		child_placement = { rect: child_rect, clip_rect: intersect_rect(child_rect, placement.clip_rect) }
		$output = $output + place_layout(child, child_placement)
		$child_x = $child_x + child_size.width
	}
	$output
}

intersect_rect : Rect, Rect -> Rect
intersect_rect = |left_rect, right_rect| {
	x = F32.max(left_rect.x, right_rect.x)
	y = F32.max(left_rect.y, right_rect.y)
	right_edge = F32.min(left_rect.x + left_rect.width, right_rect.x + right_rect.width)
	bottom_edge = F32.min(left_rect.y + left_rect.height, right_rect.y + right_rect.height)
	{ x, y, width: F32.max(0, right_edge - x), height: F32.max(0, bottom_edge - y) }
}

State : { drag_count : U64, committed_source : U64, committed_gap : U64 }

Handler := (State, U64 => [Handled(State), Declined]).{
	default : () -> Handler
	default = || Handler.(|_state, _event| Declined)

	plus : Handler, Handler -> Handler
	plus = |Handler.(earlier!), Handler.(later!)| {
		Handler.(
			|state, event| {
				match later!(state, event) {
					Handled(next) => Handled(next)
					Declined => earlier!(state, event)
				}
			},
		)
	}

	dispatch! : Handler, State, U64 => [Handled(State), Declined]
	dispatch! = |Handler.(handle!), state, event| handle!(state, event)
}

widget : U64 -> Layout(Handler)
widget = |amount| leaf(
	{ width: 10, height: 10 },
	|placement| Handler.(
		|state, event| if U64.to_f32(event) >= placement.clip_rect.x Handled({ ..state, drag_count: state.drag_count + amount }) else Declined,
	),
)

main! = |args| {
	seed = List.len(args)
	list = box([widget(seed), widget(seed + 1)])
	root_rect = { x: 0, y: 0, width: 100, height: 52 }
	handler : Handler
	handler = place_layout(list, { rect: root_rect, clip_rect: root_rect })
	initial : State
	initial = { drag_count: seed, committed_source: 99, committed_gap: 99 }
	match Handler.dispatch!(handler, initial, 5) {
		Handled(_) => Ok({})
		Declined => Ok({})
	}
}
