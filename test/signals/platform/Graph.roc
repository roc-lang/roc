import NodeValue exposing [NodeValue]
import Host

## Internal type-erased graph nodes.
## SignalNode and EventNode live under one wrapper module so they can reference
## each other without creating Roc module import cycles.
Graph := [].{
	EventNode := [
		Source,
		Prebuilt(U64),
		MapEvent({ source : EventNode, transform : Box((NodeValue -> NodeValue)) }),
		MapUnitI64Const({ source : EventNode, value : I64 }),
		Filter({ source : EventNode, predicate : Box((NodeValue -> Bool)) }),
		Merge({ left : EventNode, right : EventNode }),
		WithLatest({ event : EventNode, signal : SignalNode, combine : Box(((NodeValue, NodeValue) -> NodeValue)) }),
	].{
		make_prebuilt : U64 -> EventNode
		make_prebuilt = |id| Prebuilt(id)

		make_map_event : EventNode, Box((NodeValue -> NodeValue)) -> EventNode
		make_map_event = |source, transform| MapEvent({ source, transform })

		make_map_unit_i64_const : EventNode, I64 -> EventNode
		make_map_unit_i64_const = |source, value| MapUnitI64Const({ source, value })

		make_filter : EventNode, Box((NodeValue -> Bool)) -> EventNode
		make_filter = |source, predicate| Filter({ source, predicate })

		make_merge : EventNode, EventNode -> EventNode
		make_merge = |left, right| Merge({ left, right })

		make_with_latest : EventNode, SignalNode, Box(((NodeValue, NodeValue) -> NodeValue)) -> EventNode
		make_with_latest = |event, signal, combine| WithLatest({ event, signal, combine })

		walk! : EventNode => U64
		walk! = |event| {
			match event {
				Source =>
					Host.create_event_source!()

				Prebuilt(id) =>
					id

				MapEvent({ source, transform }) => {
					source_id = EventNode.walk!(source)
					Host.create_event_map!(source_id, transform)
				}

				MapUnitI64Const({ source, value }) => {
					source_id = EventNode.walk!(source)
					Host.create_event_map_unit_i64_const!(source_id, value)
				}

				Filter({ source, predicate }) => {
					source_id = EventNode.walk!(source)
					Host.create_event_filter!(source_id, predicate)
				}

				Merge({ left, right }) => {
					left_id = EventNode.walk!(left)
					right_id = EventNode.walk!(right)
					Host.create_event_merge!(left_id, right_id)
				}

				WithLatest({ event: source_event, signal, combine }) => {
					event_id = EventNode.walk!(source_event)
					signal_id = SignalNode.walk!(signal)
					Host.create_event_with_latest!(event_id, signal_id, combine)
				}
			}
		}
	}

	SignalNode := [
		ConstSignal(NodeValue),
		Prebuilt(U64),
		MapSignal({ source : SignalNode, transform : Box((NodeValue -> NodeValue)) }),
		MapI64I64({ source : SignalNode, transform : Box((I64 -> I64)) }),
		MapI64Str({ source : SignalNode, transform : Box((I64 -> Str)) }),
		Map2Signal({ left : SignalNode, right : SignalNode, transform : Box(((NodeValue, NodeValue) -> NodeValue)) }),
		Map2I64I64({ left : SignalNode, right : SignalNode, transform : Box(((I64, I64) -> I64)) }),
		Map2I64I64Str({ left : SignalNode, right : SignalNode, transform : Box(((I64, I64) -> Str)) }),
		ConstI64(I64),
		ConstBool(Bool),
		ConstStr(Str),
	].{
		make_const : NodeValue -> SignalNode
		make_const = |nv| ConstSignal(nv)

		make_const_i64 : I64 -> SignalNode
		make_const_i64 = |value| ConstI64(value)

		make_const_bool : Bool -> SignalNode
		make_const_bool = |value| ConstBool(value)

		make_const_str : Str -> SignalNode
		make_const_str = |value| ConstStr(value)

		make_prebuilt_signal : U64 -> SignalNode
		make_prebuilt_signal = |id| Prebuilt(id)

		make_map_signal : SignalNode, Box((NodeValue -> NodeValue)) -> SignalNode
		make_map_signal = |source, transform| MapSignal({ source, transform })

		make_map_i64_i64 : SignalNode, Box((I64 -> I64)) -> SignalNode
		make_map_i64_i64 = |source, transform| MapI64I64({ source, transform })

		make_map_i64_str : SignalNode, Box((I64 -> Str)) -> SignalNode
		make_map_i64_str = |source, transform| MapI64Str({ source, transform })

		make_map2_signal : SignalNode, SignalNode, Box(((NodeValue, NodeValue) -> NodeValue)) -> SignalNode
		make_map2_signal = |left, right, transform| Map2Signal({ left, right, transform })

		make_map2_i64_i64 : SignalNode, SignalNode, Box(((I64, I64) -> I64)) -> SignalNode
		make_map2_i64_i64 = |left, right, transform| Map2I64I64({ left, right, transform })

		make_map2_i64_i64_str : SignalNode, SignalNode, Box(((I64, I64) -> Str)) -> SignalNode
		make_map2_i64_i64_str = |left, right, transform| Map2I64I64Str({ left, right, transform })

		walk! : SignalNode => U64
		walk! = |signal| {
			match signal {
				ConstSignal(nv) =>
					Host.create_signal_const!(nv)

				ConstI64(value) =>
					Host.create_signal_const_i64!(value)

				ConstBool(value) =>
					Host.create_signal_const_bool!(value)

				ConstStr(value) =>
					Host.create_signal_const_str!(value)

				Prebuilt(id) =>
					id

				MapSignal({ source, transform }) => {
					source_id = SignalNode.walk!(source)
					Host.create_signal_map!(source_id, transform)
				}

				MapI64I64({ source, transform }) => {
					source_id = SignalNode.walk!(source)
					Host.create_signal_map_i64_i64!(source_id, transform)
				}

				MapI64Str({ source, transform }) => {
					source_id = SignalNode.walk!(source)
					Host.create_signal_map_i64_str!(source_id, transform)
				}

				Map2Signal({ left, right, transform }) => {
					left_id = SignalNode.walk!(left)
					right_id = SignalNode.walk!(right)
					Host.create_signal_map2!(left_id, right_id, transform)
				}

				Map2I64I64({ left, right, transform }) => {
					left_id = SignalNode.walk!(left)
					right_id = SignalNode.walk!(right)
					Host.create_signal_map2_i64_i64!(left_id, right_id, transform)
				}

				Map2I64I64Str({ left, right, transform }) => {
					left_id = SignalNode.walk!(left)
					right_id = SignalNode.walk!(right)
					Host.create_signal_map2_i64_i64_str!(left_id, right_id, transform)
				}
			}
		}
	}
}
