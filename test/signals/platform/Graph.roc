import NodeValue exposing [NodeValue]

## Pure graph descriptions. Constructors attach explicit dependency metadata so
## the Roc runtime can make dirty-propagation decisions from graph data.
Graph := [].{
	EventExpr := [
		Source(Str),
		MapEvent({ source : Box(EventNode), transform : Box((NodeValue -> NodeValue)) }),
		MapUnitI64Const({ source : Box(EventNode), value : I64 }),
		Merge({ left : Box(EventNode), right : Box(EventNode) }),
	]

	str_list_contains : List(Str), Str -> Bool
	str_list_contains = |items, item| {
		var $index = 0
		var $found = False

		while $found == False and $index < List.len(items) {
			match List.get(items, $index) {
				Ok(existing) =>
					if existing == item {
						$found = True
					} else {
						$index = $index + 1
					}
				Err(_) => {
					$index = List.len(items)
				}
			}
		}

		$found
	}

	merge_deps : List(Str), List(Str) -> List(Str)
	merge_deps = |left, right| {
		var $deps = left

		for dep in right {
			if str_list_contains($deps, dep) {
				{}
			} else {
				$deps = List.append($deps, dep)
			}
		}

		$deps
	}

	EventNode := {
		sources : List(Str),
		expr : EventExpr,
	}.{
		make_source : Str -> EventNode
		make_source = |key| { sources: [key], expr: Source(key) }

		make_map_event : EventNode, Box((NodeValue -> NodeValue)) -> EventNode
		make_map_event = |source, transform| { sources: source.sources, expr: MapEvent({ source: Box.box(source), transform }) }

		make_map_unit_i64_const : EventNode, I64 -> EventNode
		make_map_unit_i64_const = |source, value| { sources: source.sources, expr: MapUnitI64Const({ source: Box.box(source), value }) }

		make_merge : EventNode, EventNode -> EventNode
		make_merge = |left, right| { sources: merge_deps(left.sources, right.sources), expr: Merge({ left: Box.box(left), right: Box.box(right) }) }
	}

	SignalCacheKey := [
		NoSignalCacheKey,
		SignalCacheKey(Str),
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
		cache_key : SignalCacheKey,
		deps : List(Str),
		expr : SignalExpr,
	}.{
		make_const : NodeValue -> SignalNode
		make_const = |nv| { cache_key: NoSignalCacheKey, deps: [], expr: ConstSignal(nv) }

		make_const_i64 : I64 -> SignalNode
		make_const_i64 = |value| { cache_key: NoSignalCacheKey, deps: [], expr: ConstI64(value) }

		make_const_bool : Bool -> SignalNode
		make_const_bool = |value| { cache_key: NoSignalCacheKey, deps: [], expr: ConstBool(value) }

		make_const_str : Str -> SignalNode
		make_const_str = |value| { cache_key: NoSignalCacheKey, deps: [], expr: ConstStr(value) }

		make_map_signal : SignalNode, Box((NodeValue -> NodeValue)) -> SignalNode
		make_map_signal = |source, transform| { cache_key: NoSignalCacheKey, deps: source.deps, expr: MapSignal({ source: Box.box(source), transform }) }

		make_keyed_map_signal : Str, SignalNode, Box((NodeValue -> NodeValue)) -> SignalNode
		make_keyed_map_signal = |key, source, transform| { cache_key: SignalCacheKey(key), deps: source.deps, expr: MapSignal({ source: Box.box(source), transform }) }

		make_map_i64_i64 : SignalNode, Box((I64 -> I64)) -> SignalNode
		make_map_i64_i64 = |source, transform| { cache_key: NoSignalCacheKey, deps: source.deps, expr: MapI64I64({ source: Box.box(source), transform }) }

		make_keyed_map_i64_i64 : Str, SignalNode, Box((I64 -> I64)) -> SignalNode
		make_keyed_map_i64_i64 = |key, source, transform| { cache_key: SignalCacheKey(key), deps: source.deps, expr: MapI64I64({ source: Box.box(source), transform }) }

		make_map_i64_str : SignalNode, Box((I64 -> Str)) -> SignalNode
		make_map_i64_str = |source, transform| { cache_key: NoSignalCacheKey, deps: source.deps, expr: MapI64Str({ source: Box.box(source), transform }) }

		make_keyed_map_i64_str : Str, SignalNode, Box((I64 -> Str)) -> SignalNode
		make_keyed_map_i64_str = |key, source, transform| { cache_key: SignalCacheKey(key), deps: source.deps, expr: MapI64Str({ source: Box.box(source), transform }) }

		make_map2_signal : SignalNode, SignalNode, Box(((NodeValue, NodeValue) -> NodeValue)) -> SignalNode
		make_map2_signal = |left, right, transform| { cache_key: NoSignalCacheKey, deps: merge_deps(left.deps, right.deps), expr: Map2Signal({ left: Box.box(left), right: Box.box(right), transform }) }

		make_keyed_map2_signal : Str, SignalNode, SignalNode, Box(((NodeValue, NodeValue) -> NodeValue)) -> SignalNode
		make_keyed_map2_signal = |key, left, right, transform| { cache_key: SignalCacheKey(key), deps: merge_deps(left.deps, right.deps), expr: Map2Signal({ left: Box.box(left), right: Box.box(right), transform }) }

		make_map2_i64_i64 : SignalNode, SignalNode, Box(((I64, I64) -> I64)) -> SignalNode
		make_map2_i64_i64 = |left, right, transform| { cache_key: NoSignalCacheKey, deps: merge_deps(left.deps, right.deps), expr: Map2I64I64({ left: Box.box(left), right: Box.box(right), transform }) }

		make_keyed_map2_i64_i64 : Str, SignalNode, SignalNode, Box(((I64, I64) -> I64)) -> SignalNode
		make_keyed_map2_i64_i64 = |key, left, right, transform| { cache_key: SignalCacheKey(key), deps: merge_deps(left.deps, right.deps), expr: Map2I64I64({ left: Box.box(left), right: Box.box(right), transform }) }

		make_map2_i64_i64_str : SignalNode, SignalNode, Box(((I64, I64) -> Str)) -> SignalNode
		make_map2_i64_i64_str = |left, right, transform| { cache_key: NoSignalCacheKey, deps: merge_deps(left.deps, right.deps), expr: Map2I64I64Str({ left: Box.box(left), right: Box.box(right), transform }) }

		make_keyed_map2_i64_i64_str : Str, SignalNode, SignalNode, Box(((I64, I64) -> Str)) -> SignalNode
		make_keyed_map2_i64_i64_str = |key, left, right, transform| { cache_key: SignalCacheKey(key), deps: merge_deps(left.deps, right.deps), expr: Map2I64I64Str({ left: Box.box(left), right: Box.box(right), transform }) }

		make_hold : Str, NodeValue, EventNode -> SignalNode
		make_hold = |key, initial, event| { cache_key: SignalCacheKey(key), deps: [key], expr: Hold({ key, initial, event: Box.box(event) }) }

		make_fold : Str, NodeValue, EventNode, Box(((NodeValue, NodeValue) -> NodeValue)) -> SignalNode
		make_fold = |key, initial, event, step| { cache_key: SignalCacheKey(key), deps: [key], expr: Fold({ key, initial, event: Box.box(event), step }) }

		make_fold_i64 : Str, I64, EventNode, Box(((I64, I64) -> I64)) -> SignalNode
		make_fold_i64 = |key, initial, event, step| { cache_key: SignalCacheKey(key), deps: [key], expr: FoldI64({ key, initial, event: Box.box(event), step }) }

		make_fold_bool_toggle : Str, Bool, EventNode -> SignalNode
		make_fold_bool_toggle = |key, initial, event| { cache_key: SignalCacheKey(key), deps: [key], expr: FoldBoolToggle({ key, initial, event: Box.box(event) }) }
	}
}
