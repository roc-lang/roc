import NodeValue exposing [NodeValue]

## Pure graph descriptions. The Roc runtime owns evaluation; the host only sees
## stable event ids in rendered commands.
Graph := [].{
	EventNode := [
		Source(Str),
		MapEvent({ source : Box(EventNode), transform : Box((NodeValue -> NodeValue)) }),
		MapUnitI64Const({ source : Box(EventNode), value : I64 }),
		Merge({ left : Box(EventNode), right : Box(EventNode) }),
	].{
		make_source : Str -> EventNode
		make_source = |key| Source(key)

		make_map_event : EventNode, Box((NodeValue -> NodeValue)) -> EventNode
		make_map_event = |source, transform| MapEvent({ source: Box.box(source), transform })

		make_map_unit_i64_const : EventNode, I64 -> EventNode
		make_map_unit_i64_const = |source, value| MapUnitI64Const({ source: Box.box(source), value })

		make_merge : EventNode, EventNode -> EventNode
		make_merge = |left, right| Merge({ left: Box.box(left), right: Box.box(right) })
	}

	SignalNode := [
		ConstSignal(NodeValue),
		ConstI64(I64),
		ConstBool(Bool),
		ConstStr(Str),
		MapSignal({ source : Box(SignalNode), transform : Box((NodeValue -> NodeValue)) }),
		MapI64I64({ source : Box(SignalNode), transform : Box((I64 -> I64)) }),
		MapI64Str({ source : Box(SignalNode), transform : Box((I64 -> Str)) }),
		Map2Signal({ left : Box(SignalNode), right : Box(SignalNode), transform : Box(((NodeValue, NodeValue) -> NodeValue)) }),
		Map2I64I64({ left : Box(SignalNode), right : Box(SignalNode), transform : Box(((I64, I64) -> I64)) }),
		Map2I64I64Str({ left : Box(SignalNode), right : Box(SignalNode), transform : Box(((I64, I64) -> Str)) }),
		Hold({ key : Str, initial : NodeValue, event : Box(EventNode) }),
		Fold({ key : Str, initial : NodeValue, event : Box(EventNode), step : Box(((NodeValue, NodeValue) -> NodeValue)) }),
		FoldI64({ key : Str, initial : I64, event : Box(EventNode), step : Box(((I64, I64) -> I64)) }),
		FoldBoolToggle({ key : Str, initial : Bool, event : Box(EventNode) }),
	].{
		make_const : NodeValue -> SignalNode
		make_const = |nv| ConstSignal(nv)

		make_const_i64 : I64 -> SignalNode
		make_const_i64 = |value| ConstI64(value)

		make_const_bool : Bool -> SignalNode
		make_const_bool = |value| ConstBool(value)

		make_const_str : Str -> SignalNode
		make_const_str = |value| ConstStr(value)

		make_map_signal : SignalNode, Box((NodeValue -> NodeValue)) -> SignalNode
		make_map_signal = |source, transform| MapSignal({ source: Box.box(source), transform })

		make_map_i64_i64 : SignalNode, Box((I64 -> I64)) -> SignalNode
		make_map_i64_i64 = |source, transform| MapI64I64({ source: Box.box(source), transform })

		make_map_i64_str : SignalNode, Box((I64 -> Str)) -> SignalNode
		make_map_i64_str = |source, transform| MapI64Str({ source: Box.box(source), transform })

		make_map2_signal : SignalNode, SignalNode, Box(((NodeValue, NodeValue) -> NodeValue)) -> SignalNode
		make_map2_signal = |left, right, transform| Map2Signal({ left: Box.box(left), right: Box.box(right), transform })

		make_map2_i64_i64 : SignalNode, SignalNode, Box(((I64, I64) -> I64)) -> SignalNode
		make_map2_i64_i64 = |left, right, transform| Map2I64I64({ left: Box.box(left), right: Box.box(right), transform })

		make_map2_i64_i64_str : SignalNode, SignalNode, Box(((I64, I64) -> Str)) -> SignalNode
		make_map2_i64_i64_str = |left, right, transform| Map2I64I64Str({ left: Box.box(left), right: Box.box(right), transform })

		make_hold : Str, NodeValue, EventNode -> SignalNode
		make_hold = |key, initial, event| Hold({ key, initial, event: Box.box(event) })

		make_fold : Str, NodeValue, EventNode, Box(((NodeValue, NodeValue) -> NodeValue)) -> SignalNode
		make_fold = |key, initial, event, step| Fold({ key, initial, event: Box.box(event), step })

		make_fold_i64 : Str, I64, EventNode, Box(((I64, I64) -> I64)) -> SignalNode
		make_fold_i64 = |key, initial, event, step| FoldI64({ key, initial, event: Box.box(event), step })

		make_fold_bool_toggle : Str, Bool, EventNode -> SignalNode
		make_fold_bool_toggle = |key, initial, event| FoldBoolToggle({ key, initial, event: Box.box(event) })
	}
}
