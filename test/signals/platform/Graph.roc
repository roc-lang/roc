import NodeValue exposing [NodeValue]

## Pure graph descriptions. Registration turns this structure into explicit
## descriptor data for the host-owned signal topology.
Graph := [].{
	unit_payload_kind : U64
	unit_payload_kind = 1

	str_payload_kind : U64
	str_payload_kind = 2

	bool_payload_kind : U64
	bool_payload_kind = 3

	EventExpr := [
		Source({ key : Str, payload_kind : U64 }),
		MapEvent({ source : Box(EventNode), transform : Box((NodeValue -> NodeValue)) }),
		MapUnitI64Const({ source : Box(EventNode), value : I64 }),
		Merge({ left : Box(EventNode), right : Box(EventNode) }),
	]

	EventNode := {
		source_ids : List(U64),
		expr : EventExpr,
	}.{
		make_source : Str, U64 -> EventNode
		make_source = |key, payload_kind| { source_ids: [], expr: Source({ key, payload_kind }) }

		make_map_event : EventNode, Box((NodeValue -> NodeValue)) -> EventNode
		make_map_event = |source, transform| { source_ids: [], expr: MapEvent({ source: Box.box(source), transform }) }

		make_map_unit_i64_const : EventNode, I64 -> EventNode
		make_map_unit_i64_const = |source, value| { source_ids: [], expr: MapUnitI64Const({ source: Box.box(source), value }) }

		make_merge : EventNode, EventNode -> EventNode
		make_merge = |left, right| { source_ids: [], expr: Merge({ left: Box.box(left), right: Box.box(right) }) }
	}

	SignalCacheKey := [
		NoSignalCacheKey,
		SignalCacheKey(Str),
	]

	SignalIdentity := [
		UnregisteredSignal,
		RegisteredSignal(U64),
	]

	SignalExpr := [
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
	]

	SignalNode := {
		signal_id : SignalIdentity,
		cache_key : SignalCacheKey,
		dep_indexes : List(U64),
		expr : SignalExpr,
	}.{
		make_const : NodeValue -> SignalNode
		make_const = |nv| { signal_id: UnregisteredSignal, cache_key: NoSignalCacheKey, dep_indexes: [], expr: ConstSignal(nv) }

		make_const_i64 : I64 -> SignalNode
		make_const_i64 = |value| { signal_id: UnregisteredSignal, cache_key: NoSignalCacheKey, dep_indexes: [], expr: ConstI64(value) }

		make_const_bool : Bool -> SignalNode
		make_const_bool = |value| { signal_id: UnregisteredSignal, cache_key: NoSignalCacheKey, dep_indexes: [], expr: ConstBool(value) }

		make_const_str : Str -> SignalNode
		make_const_str = |value| { signal_id: UnregisteredSignal, cache_key: NoSignalCacheKey, dep_indexes: [], expr: ConstStr(value) }

		make_map_signal : SignalNode, Box((NodeValue -> NodeValue)) -> SignalNode
		make_map_signal = |source, transform| { signal_id: UnregisteredSignal, cache_key: NoSignalCacheKey, dep_indexes: [], expr: MapSignal({ source: Box.box(source), transform }) }

		make_keyed_map_signal : Str, SignalNode, Box((NodeValue -> NodeValue)) -> SignalNode
		make_keyed_map_signal = |key, source, transform| { signal_id: UnregisteredSignal, cache_key: SignalCacheKey(key), dep_indexes: [], expr: MapSignal({ source: Box.box(source), transform }) }

		make_map_i64_i64 : SignalNode, Box((I64 -> I64)) -> SignalNode
		make_map_i64_i64 = |source, transform| { signal_id: UnregisteredSignal, cache_key: NoSignalCacheKey, dep_indexes: [], expr: MapI64I64({ source: Box.box(source), transform }) }

		make_keyed_map_i64_i64 : Str, SignalNode, Box((I64 -> I64)) -> SignalNode
		make_keyed_map_i64_i64 = |key, source, transform| { signal_id: UnregisteredSignal, cache_key: SignalCacheKey(key), dep_indexes: [], expr: MapI64I64({ source: Box.box(source), transform }) }

		make_map_i64_str : SignalNode, Box((I64 -> Str)) -> SignalNode
		make_map_i64_str = |source, transform| { signal_id: UnregisteredSignal, cache_key: NoSignalCacheKey, dep_indexes: [], expr: MapI64Str({ source: Box.box(source), transform }) }

		make_keyed_map_i64_str : Str, SignalNode, Box((I64 -> Str)) -> SignalNode
		make_keyed_map_i64_str = |key, source, transform| { signal_id: UnregisteredSignal, cache_key: SignalCacheKey(key), dep_indexes: [], expr: MapI64Str({ source: Box.box(source), transform }) }

		make_map2_signal : SignalNode, SignalNode, Box(((NodeValue, NodeValue) -> NodeValue)) -> SignalNode
		make_map2_signal = |left, right, transform| { signal_id: UnregisteredSignal, cache_key: NoSignalCacheKey, dep_indexes: [], expr: Map2Signal({ left: Box.box(left), right: Box.box(right), transform }) }

		make_keyed_map2_signal : Str, SignalNode, SignalNode, Box(((NodeValue, NodeValue) -> NodeValue)) -> SignalNode
		make_keyed_map2_signal = |key, left, right, transform| { signal_id: UnregisteredSignal, cache_key: SignalCacheKey(key), dep_indexes: [], expr: Map2Signal({ left: Box.box(left), right: Box.box(right), transform }) }

		make_map2_i64_i64 : SignalNode, SignalNode, Box(((I64, I64) -> I64)) -> SignalNode
		make_map2_i64_i64 = |left, right, transform| { signal_id: UnregisteredSignal, cache_key: NoSignalCacheKey, dep_indexes: [], expr: Map2I64I64({ left: Box.box(left), right: Box.box(right), transform }) }

		make_keyed_map2_i64_i64 : Str, SignalNode, SignalNode, Box(((I64, I64) -> I64)) -> SignalNode
		make_keyed_map2_i64_i64 = |key, left, right, transform| { signal_id: UnregisteredSignal, cache_key: SignalCacheKey(key), dep_indexes: [], expr: Map2I64I64({ left: Box.box(left), right: Box.box(right), transform }) }

		make_map2_i64_i64_str : SignalNode, SignalNode, Box(((I64, I64) -> Str)) -> SignalNode
		make_map2_i64_i64_str = |left, right, transform| { signal_id: UnregisteredSignal, cache_key: NoSignalCacheKey, dep_indexes: [], expr: Map2I64I64Str({ left: Box.box(left), right: Box.box(right), transform }) }

		make_keyed_map2_i64_i64_str : Str, SignalNode, SignalNode, Box(((I64, I64) -> Str)) -> SignalNode
		make_keyed_map2_i64_i64_str = |key, left, right, transform| { signal_id: UnregisteredSignal, cache_key: SignalCacheKey(key), dep_indexes: [], expr: Map2I64I64Str({ left: Box.box(left), right: Box.box(right), transform }) }

		make_hold : Str, NodeValue, EventNode -> SignalNode
		make_hold = |key, initial, event| { signal_id: UnregisteredSignal, cache_key: SignalCacheKey(key), dep_indexes: [], expr: Hold({ key, initial, event: Box.box(event) }) }

		make_fold : Str, NodeValue, EventNode, Box(((NodeValue, NodeValue) -> NodeValue)) -> SignalNode
		make_fold = |key, initial, event, step| { signal_id: UnregisteredSignal, cache_key: SignalCacheKey(key), dep_indexes: [], expr: Fold({ key, initial, event: Box.box(event), step }) }

		make_fold_i64 : Str, I64, EventNode, Box(((I64, I64) -> I64)) -> SignalNode
		make_fold_i64 = |key, initial, event, step| { signal_id: UnregisteredSignal, cache_key: SignalCacheKey(key), dep_indexes: [], expr: FoldI64({ key, initial, event: Box.box(event), step }) }

		make_fold_bool_toggle : Str, Bool, EventNode -> SignalNode
		make_fold_bool_toggle = |key, initial, event| { signal_id: UnregisteredSignal, cache_key: SignalCacheKey(key), dep_indexes: [], expr: FoldBoolToggle({ key, initial, event: Box.box(event) }) }
	}
}
