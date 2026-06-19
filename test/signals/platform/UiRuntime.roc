import Elem exposing [Elem]
import Graph
import NodeValue exposing [NodeValue]

UiRuntime := [].{
	RuntimeMetrics : {
		events_processed : U64,
		nodes_recomputed : U64,
		propagation_prunes : U64,
		derived_calls_into_roc : U64,
		recompute_batches : U64,
		patches_emitted : U64,
		scopes_created : U64,
		scopes_disposed : U64,
		rows_reused : U64,
		rows_created : U64,
		rows_removed : U64,
		closure_retains : U64,
		closure_releases : U64,
		retained_alloc_delta : I64,
	}

	EventId : { key : Str, id : U64, payload_kind : U64 }

	StateSlot : { key : Str, initial : NodeValue }

	StateDesc : { state_id : U64, value : NodeValue }

	StateValueDesc : { state_id : U64, value : NodeValue }

	SignalDesc : { signal_id : U64, kind : U64, source_state_ids : List(U64), source_event_ids : List(U64), input_signal_ids : List(U64) }

	SignalValueDesc : { signal_id : U64, value : NodeValue }

	SignalRegistryEntry : { key : Str, signal_id : U64, signal : Graph.SignalNode, kind : U64, source_state_ids : List(U64), source_event_ids : List(U64), input_signal_ids : List(U64) }

	EventDesc : { event_id : U64, payload_kind : U64 }

	RenderElementDesc : { elem_id : U64, parent_id : U64, tag : Str }

	RenderTextDesc : { elem_id : U64, field : U64, value : Str }

	RenderSignalTextDesc : { elem_id : U64, field : U64, signal_id : U64, value : Str }

	RenderBoolDesc : { elem_id : U64, field : U64, value : Bool }

	RenderSignalBoolDesc : { elem_id : U64, field : U64, signal_id : U64, value : Bool }

	RenderEventDesc : { elem_id : U64, event_kind : U64, event_id : U64 }

	RenderStructuralDesc : { elem_id : U64, signal_id : U64 }

	Runtime : {
		root : Elem,
		states : List(StateSlot),
		event_ids : List(EventId),
		next_event_id : U64,
		next_signal_id : U64,
		signal_registry : List(SignalRegistryEntry),
		metrics : RuntimeMetrics,
	}

	DispatchResult : {
		runtime : Box(Runtime),
		render_elements : List(RenderElementDesc),
		render_texts : List(RenderTextDesc),
		render_signal_texts : List(RenderSignalTextDesc),
		render_bools : List(RenderBoolDesc),
		render_signal_bools : List(RenderSignalBoolDesc),
		render_events : List(RenderEventDesc),
		render_structures : List(RenderStructuralDesc),
		event_descriptors : List(EventDesc),
		signal_descriptors : List(SignalDesc),
		state_descriptors : List(StateDesc),
		state_changes : List(StateValueDesc),
		signal_changes : List(SignalValueDesc),
		metrics : RuntimeMetrics,
	}

	RecomputeResult : {
		runtime : Box(Runtime),
		event_descriptors : List(EventDesc),
		signal_descriptors : List(SignalDesc),
		state_descriptors : List(StateDesc),
		state_changes : List(StateValueDesc),
		signal_changes : List(SignalValueDesc),
		metrics : RuntimeMetrics,
	}

	RenderInput : {
		cached_signals : List(SignalValueDesc),
		cached_states : List(StateValueDesc),
	}

	ActiveEvent := [
		NoEvent,
		Occurrence({ id : U64, value : NodeValue }),
	]

	RecomputeInput : {
		active_event : ActiveEvent,
		dirty_signal_ids : List(U64),
		cached_signals : List(SignalValueDesc),
		cached_states : List(StateValueDesc),
	}

	EvalState : {
		runtime : Runtime,
		active_event : ActiveEvent,
		dirty_signal_ids : List(U64),
		cached_signals : List(SignalValueDesc),
		cached_states : List(StateValueDesc),
		signal_changes : List(SignalValueDesc),
		state_changes : List(StateValueDesc),
		updated_state_indexes : List(U64),
		changed_state_indexes : List(U64),
	}

	EvalResult : { state : EvalState, value : NodeValue }

	EventResult : { state : EvalState, values : List(NodeValue) }

	RenderState : {
		state : EvalState,
		render_elements : List(RenderElementDesc),
		render_texts : List(RenderTextDesc),
		render_signal_texts : List(RenderSignalTextDesc),
		render_bools : List(RenderBoolDesc),
		render_signal_bools : List(RenderSignalBoolDesc),
		render_events : List(RenderEventDesc),
		render_structures : List(RenderStructuralDesc),
		next_elem_id : U64,
	}

	RenderResult : {
		runtime : Runtime,
		render_elements : List(RenderElementDesc),
		render_texts : List(RenderTextDesc),
		render_signal_texts : List(RenderSignalTextDesc),
		render_bools : List(RenderBoolDesc),
		render_signal_bools : List(RenderSignalBoolDesc),
		render_events : List(RenderEventDesc),
		render_structures : List(RenderStructuralDesc),
		changed_state_indexes : List(U64),
		state_changes : List(StateValueDesc),
		signal_changes : List(SignalValueDesc),
	}

	EventLookup : { runtime : Runtime, id : U64 }

	StateSlotLookup := [
		StateSlotFound({ index : U64, slot : StateSlot }),
		StateSlotMissing,
	]

	EventIdLookup := [
		EventIdFound(EventId),
		EventIdMissing,
	]

	DirtySignals : { ids : List(U64) }

	SignalRegistryLookup := [
		SignalRegistryFound(SignalRegistryEntry),
		SignalRegistryMissing,
	]

	CacheLookup := [
		CacheHit(NodeValue),
		CacheMiss,
	]

	StateChangeLookup := [
		StateChangeFound(NodeValue),
		StateChangeMissing,
	]

	StateValue : { runtime : Runtime, value : NodeValue, index : U64 }

	RenderSignalLookup := [
		RenderSignalFound(U64),
		RenderSignalMissing,
	]

	RegisteredElem : { runtime : Runtime, elem : Elem }

	RegisteredChildren : { runtime : Runtime, children : List(Elem) }

	RegisteredSignal : { runtime : Runtime, signal : Graph.SignalNode }

	RegisteredEvent : { runtime : Runtime, event : Graph.EventNode }

	RegisteredState : { runtime : Runtime, index : U64 }

	signal_kind_source : U64
	signal_kind_source = 1

	signal_kind_map : U64
	signal_kind_map = 2

	signal_kind_map2 : U64
	signal_kind_map2 = 3

	render_text_field_text : U64
	render_text_field_text = 1

	render_text_field_role : U64
	render_text_field_role = 2

	render_text_field_label : U64
	render_text_field_label = 3

	render_text_field_test_id : U64
	render_text_field_test_id = 4

	render_text_field_value : U64
	render_text_field_value = 5

	render_bool_field_checked : U64
	render_bool_field_checked = 1

	render_bool_field_disabled : U64
	render_bool_field_disabled = 2

	render_event_kind_click : U64
	render_event_kind_click = 1

	render_event_kind_input : U64
	render_event_kind_input = 2

	render_event_kind_check : U64
	render_event_kind_check = 3

	zero_metrics : RuntimeMetrics
	zero_metrics = {
		events_processed: 0,
		nodes_recomputed: 0,
		propagation_prunes: 0,
		derived_calls_into_roc: 0,
		recompute_batches: 0,
		patches_emitted: 0,
		scopes_created: 0,
		scopes_disposed: 0,
		rows_reused: 0,
		rows_created: 0,
		rows_removed: 0,
		closure_retains: 0,
		closure_releases: 0,
		retained_alloc_delta: 0,
	}

	init : Elem -> DispatchResult
	init = |root| {
		runtime0 = {
			root,
			states: [],
			event_ids: [],
			next_event_id: 1,
			next_signal_id: 0,
			signal_registry: [],
			metrics: zero_metrics,
		}
		registered = register_elem(runtime0, root)
		runtime = { ..registered.runtime, root: registered.elem }
		rendered = render_runtime(runtime, NoEvent, { ids: [] }, [], [])
		runtime_box = Box.box(rendered.runtime)
		result = {
			runtime: runtime_box,
			render_elements: rendered.render_elements,
			render_texts: rendered.render_texts,
			render_signal_texts: rendered.render_signal_texts,
			render_bools: rendered.render_bools,
			render_signal_bools: rendered.render_signal_bools,
			render_events: rendered.render_events,
			render_structures: rendered.render_structures,
			event_descriptors: event_descriptors_for_runtime(rendered.runtime),
			signal_descriptors: signal_descriptors_for_runtime(rendered.runtime),
			state_descriptors: state_descriptors_for_runtime(rendered.runtime),
			state_changes: rendered.state_changes,
			signal_changes: rendered.signal_changes,
			metrics: rendered.runtime.metrics,
		}
		result
	}

	recompute : Box(Runtime), RecomputeInput -> RecomputeResult
	recompute = |boxed_runtime, input| {
		runtime0 = Box.unbox(boxed_runtime)
		state0 = eval_state_for_runtime(runtime0, input.active_event, { ids: input.dirty_signal_ids }, input.cached_signals, input.cached_states)
		state1 = eval_signal_plan(state0, input.dirty_signal_ids)
		{
			runtime: Box.box(state1.runtime),
			event_descriptors: event_descriptors_for_runtime(state1.runtime),
			signal_descriptors: signal_descriptors_for_runtime(state1.runtime),
			state_descriptors: state_descriptors_for_runtime(state1.runtime),
			state_changes: state1.state_changes,
			signal_changes: state1.signal_changes,
			metrics: state1.runtime.metrics,
		}
	}

	render_only : Box(Runtime), RenderInput -> DispatchResult
	render_only = |boxed_runtime, input| {
		runtime0 = Box.unbox(boxed_runtime)
		rendered = render_runtime(runtime0, NoEvent, { ids: [] }, input.cached_signals, input.cached_states)
		{
			runtime: Box.box(rendered.runtime),
			render_elements: rendered.render_elements,
			render_texts: rendered.render_texts,
			render_signal_texts: rendered.render_signal_texts,
			render_bools: rendered.render_bools,
			render_signal_bools: rendered.render_signal_bools,
			render_events: rendered.render_events,
			render_structures: rendered.render_structures,
			event_descriptors: event_descriptors_for_runtime(rendered.runtime),
			signal_descriptors: signal_descriptors_for_runtime(rendered.runtime),
			state_descriptors: state_descriptors_for_runtime(rendered.runtime),
			state_changes: rendered.state_changes,
			signal_changes: rendered.signal_changes,
			metrics: rendered.runtime.metrics,
		}
	}

	register_elem : Runtime, Elem -> RegisteredElem
	register_elem = |runtime, elem| {
		match elem {
			Div(children) => {
				registered = register_children(runtime, children)
				{ runtime: registered.runtime, elem: Div(registered.children) }
			}

			Button({ on_click, label }) => {
				registered_event = register_event(runtime, on_click)
				registered_label = register_signal(registered_event.runtime, label)
				{
					runtime: registered_label.runtime,
					elem: Button({ on_click: registered_event.event, label: registered_label.signal }),
				}
			}

			ActionButton({ on_click, label, disabled }) => {
				registered_event = register_event(runtime, on_click)
				registered_label = register_signal(registered_event.runtime, label)
				registered_disabled = register_signal(registered_label.runtime, disabled)
				{
					runtime: registered_disabled.runtime,
					elem: ActionButton({ on_click: registered_event.event, label: registered_label.signal, disabled: registered_disabled.signal }),
				}
			}

			Label({ signal }) => {
				registered_signal = register_signal(runtime, signal)
				{ runtime: registered_signal.runtime, elem: Label({ signal: registered_signal.signal }) }
			}

			Text(_) => { runtime, elem }

			Heading(_) => { runtime, elem }

			Paragraph(_) => { runtime, elem }

			Section({ label, children }) => {
				registered = register_children(runtime, children)
				{ runtime: registered.runtime, elem: Section({ label, children: registered.children }) }
			}

			TextInput({ label, value, on_input, disabled }) => {
				registered_event = register_event(runtime, on_input)
				registered_value = register_signal(registered_event.runtime, value)
				registered_disabled = register_signal(registered_value.runtime, disabled)
				{
					runtime: registered_disabled.runtime,
					elem: TextInput({ label, value: registered_value.signal, on_input: registered_event.event, disabled: registered_disabled.signal }),
				}
			}

			Checkbox({ label, checked, on_check, disabled }) => {
				registered_event = register_event(runtime, on_check)
				registered_checked = register_signal(registered_event.runtime, checked)
				registered_disabled = register_signal(registered_checked.runtime, disabled)
				{
					runtime: registered_disabled.runtime,
					elem: Checkbox({ label, checked: registered_checked.signal, on_check: registered_event.event, disabled: registered_disabled.signal }),
				}
			}

			Dynamic({ signal, render }) => {
				registered_signal = register_signal(runtime, signal)
				{ runtime: registered_signal.runtime, elem: Dynamic({ signal: registered_signal.signal, render }) }
			}

			DynamicKeyed({ signal, key, render }) => {
				registered_signal = register_signal(runtime, signal)
				{ runtime: registered_signal.runtime, elem: DynamicKeyed({ signal: registered_signal.signal, key, render }) }
			}

			Each({ signal, key, render }) => {
				registered_signal = register_signal(runtime, signal)
				{ runtime: registered_signal.runtime, elem: Each({ signal: registered_signal.signal, key, render }) }
			}
		}
	}

	register_children : Runtime, List(Elem) -> RegisteredChildren
	register_children = |runtime, children| {
		List.fold(
			children,
			{ runtime, children: [] },
			|acc, child| {
				registered = register_elem(acc.runtime, child)
				{ runtime: registered.runtime, children: List.append(acc.children, registered.elem) }
			},
		)
	}

	register_signal : Runtime, Graph.SignalNode -> RegisteredSignal
	register_signal = |runtime, signal| {
		match signal.expr {
			Graph.SignalExpr.ConstSignal(_) => { runtime, signal: { ..signal, dep_indexes: [] } }
			Graph.SignalExpr.ConstI64(_) => { runtime, signal: { ..signal, dep_indexes: [] } }
			Graph.SignalExpr.ConstBool(_) => { runtime, signal: { ..signal, dep_indexes: [] } }
			Graph.SignalExpr.ConstStr(_) => { runtime, signal: { ..signal, dep_indexes: [] } }

			Graph.SignalExpr.MapSignal({ source, transform }) => {
				registered_source = register_signal(runtime, Box.unbox(source))
				registered_signal0 = { ..signal,
					dep_indexes: registered_source.signal.dep_indexes,
					expr: Graph.SignalExpr.MapSignal({ source: Box.box(registered_source.signal), transform }),
				}
				register_signal_descriptor(registered_source.runtime, registered_signal0, signal_kind_map, descriptor_input_ids_for_single(registered_signal0, registered_source.signal), [])
			}

			Graph.SignalExpr.MapI64I64({ source, transform }) => {
				registered_source = register_signal(runtime, Box.unbox(source))
				registered_signal0 = { ..signal,
					dep_indexes: registered_source.signal.dep_indexes,
					expr: Graph.SignalExpr.MapI64I64({ source: Box.box(registered_source.signal), transform }),
				}
				register_signal_descriptor(registered_source.runtime, registered_signal0, signal_kind_map, descriptor_input_ids_for_single(registered_signal0, registered_source.signal), [])
			}

			Graph.SignalExpr.MapI64Str({ source, transform }) => {
				registered_source = register_signal(runtime, Box.unbox(source))
				registered_signal0 = { ..signal,
					dep_indexes: registered_source.signal.dep_indexes,
					expr: Graph.SignalExpr.MapI64Str({ source: Box.box(registered_source.signal), transform }),
				}
				register_signal_descriptor(registered_source.runtime, registered_signal0, signal_kind_map, descriptor_input_ids_for_single(registered_signal0, registered_source.signal), [])
			}

			Graph.SignalExpr.Map2Signal({ left, right, transform }) => {
				registered_left = register_signal(runtime, Box.unbox(left))
				registered_right = register_signal(registered_left.runtime, Box.unbox(right))
				registered_signal0 = { ..signal,
					dep_indexes: merge_u64_deps(registered_left.signal.dep_indexes, registered_right.signal.dep_indexes),
					expr: Graph.SignalExpr.Map2Signal({ left: Box.box(registered_left.signal), right: Box.box(registered_right.signal), transform }),
				}
				register_signal_descriptor(registered_right.runtime, registered_signal0, signal_kind_map2, descriptor_input_ids_for_pair(registered_signal0, registered_left.signal, registered_right.signal), [])
			}

			Graph.SignalExpr.Map2I64I64({ left, right, transform }) => {
				registered_left = register_signal(runtime, Box.unbox(left))
				registered_right = register_signal(registered_left.runtime, Box.unbox(right))
				registered_signal0 = { ..signal,
					dep_indexes: merge_u64_deps(registered_left.signal.dep_indexes, registered_right.signal.dep_indexes),
					expr: Graph.SignalExpr.Map2I64I64({ left: Box.box(registered_left.signal), right: Box.box(registered_right.signal), transform }),
				}
				register_signal_descriptor(registered_right.runtime, registered_signal0, signal_kind_map2, descriptor_input_ids_for_pair(registered_signal0, registered_left.signal, registered_right.signal), [])
			}

			Graph.SignalExpr.Map2I64I64Str({ left, right, transform }) => {
				registered_left = register_signal(runtime, Box.unbox(left))
				registered_right = register_signal(registered_left.runtime, Box.unbox(right))
				registered_signal0 = { ..signal,
					dep_indexes: merge_u64_deps(registered_left.signal.dep_indexes, registered_right.signal.dep_indexes),
					expr: Graph.SignalExpr.Map2I64I64Str({ left: Box.box(registered_left.signal), right: Box.box(registered_right.signal), transform }),
				}
				register_signal_descriptor(registered_right.runtime, registered_signal0, signal_kind_map2, descriptor_input_ids_for_pair(registered_signal0, registered_left.signal, registered_right.signal), [])
			}

			Graph.SignalExpr.Hold({ key, initial, event }) => {
				registered_event = register_event(runtime, Box.unbox(event))
				registered_state = register_state_key(registered_event.runtime, key, initial)
				registered_signal0 = { ..signal,
					dep_indexes: [registered_state.index],
					expr: Graph.SignalExpr.Hold({ key, initial, event: Box.box(registered_event.event) }),
				}
				register_signal_descriptor(registered_state.runtime, registered_signal0, signal_kind_source, [], registered_event.event.source_ids)
			}

			Graph.SignalExpr.Fold({ key, initial, event, step }) => {
				registered_event = register_event(runtime, Box.unbox(event))
				registered_state = register_state_key(registered_event.runtime, key, initial)
				registered_signal0 = { ..signal,
					dep_indexes: [registered_state.index],
					expr: Graph.SignalExpr.Fold({ key, initial, event: Box.box(registered_event.event), step }),
				}
				register_signal_descriptor(registered_state.runtime, registered_signal0, signal_kind_source, [], registered_event.event.source_ids)
			}

			Graph.SignalExpr.FoldI64({ key, initial, event, step }) => {
				registered_event = register_event(runtime, Box.unbox(event))
				registered_state = register_state_key(registered_event.runtime, key, NodeValue.from_i64(initial))
				registered_signal0 = { ..signal,
					dep_indexes: [registered_state.index],
					expr: Graph.SignalExpr.FoldI64({ key, initial, event: Box.box(registered_event.event), step }),
				}
				register_signal_descriptor(registered_state.runtime, registered_signal0, signal_kind_source, [], registered_event.event.source_ids)
			}

			Graph.SignalExpr.FoldBoolToggle({ key, initial, event }) => {
				registered_event = register_event(runtime, Box.unbox(event))
				registered_state = register_state_key(registered_event.runtime, key, NodeValue.from_bool(initial))
				registered_signal0 = { ..signal,
					dep_indexes: [registered_state.index],
					expr: Graph.SignalExpr.FoldBoolToggle({ key, initial, event: Box.box(registered_event.event) }),
				}
				register_signal_descriptor(registered_state.runtime, registered_signal0, signal_kind_source, [], registered_event.event.source_ids)
			}
		}
	}

	descriptor_input_ids_for_single : Graph.SignalNode, Graph.SignalNode -> List(U64)
	descriptor_input_ids_for_single = |signal, source| {
		match signal.cache_key {
			Graph.SignalCacheKey.NoSignalCacheKey => []
			Graph.SignalCacheKey.SignalCacheKey(_) => [registered_signal_id(source)]
		}
	}

	descriptor_input_ids_for_pair : Graph.SignalNode, Graph.SignalNode, Graph.SignalNode -> List(U64)
	descriptor_input_ids_for_pair = |signal, left, right| {
		match signal.cache_key {
			Graph.SignalCacheKey.NoSignalCacheKey => []
			Graph.SignalCacheKey.SignalCacheKey(_) => [registered_signal_id(left), registered_signal_id(right)]
		}
	}

	register_signal_descriptor : Runtime, Graph.SignalNode, U64, List(U64), List(U64) -> RegisteredSignal
	register_signal_descriptor = |runtime, signal, kind, input_signal_ids, source_event_ids| {
		match signal.cache_key {
			Graph.SignalCacheKey.NoSignalCacheKey => { runtime, signal }
			Graph.SignalCacheKey.SignalCacheKey(key) => {
				source_state_ids = descriptor_source_state_ids(kind, signal)
				match signal_registry_lookup(runtime.signal_registry, key, kind, source_state_ids, source_event_ids, input_signal_ids) {
					SignalRegistryFound(entry) => {
						{
							runtime,
							signal: entry.signal,
						}
					}

					SignalRegistryMissing => {
						signal_id = runtime.next_signal_id
						registered_signal = { ..signal, signal_id: Graph.SignalIdentity.RegisteredSignal(signal_id) }
						entry = { key, signal_id, signal: registered_signal, kind, source_state_ids, source_event_ids, input_signal_ids }
						{
							runtime: { ..runtime,
								next_signal_id: signal_id + 1,
								signal_registry: List.append(runtime.signal_registry, entry),
							},
							signal: registered_signal,
						}
					}
				}
			}
		}
	}

	descriptor_source_state_ids : U64, Graph.SignalNode -> List(U64)
	descriptor_source_state_ids = |kind, signal| {
		if kind == signal_kind_source {
			signal.dep_indexes
		} else {
			[]
		}
	}

	register_event : Runtime, Graph.EventNode -> RegisteredEvent
	register_event = |runtime, event| {
		match event.expr {
			Graph.EventExpr.Source({ key, payload_kind }) => {
				registered_event = register_event_key(runtime, key, payload_kind)
				{
					runtime: registered_event.runtime,
					event: { ..event, source_ids: [registered_event.id] },
				}
			}

			Graph.EventExpr.MapEvent({ source, transform }) => {
				registered_source = register_event(runtime, Box.unbox(source))
				registered_event = { ..event,
					source_ids: registered_source.event.source_ids,
					expr: Graph.EventExpr.MapEvent({ source: Box.box(registered_source.event), transform }),
				}
				{ runtime: registered_source.runtime, event: registered_event }
			}

			Graph.EventExpr.MapUnitI64Const({ source, value }) => {
				registered_source = register_event(runtime, Box.unbox(source))
				registered_event = { ..event,
					source_ids: registered_source.event.source_ids,
					expr: Graph.EventExpr.MapUnitI64Const({ source: Box.box(registered_source.event), value }),
				}
				{ runtime: registered_source.runtime, event: registered_event }
			}

			Graph.EventExpr.Merge({ left, right }) => {
				registered_left = register_event(runtime, Box.unbox(left))
				registered_right = register_event(registered_left.runtime, Box.unbox(right))
				registered_event = { ..event,
					source_ids: merge_u64_deps(registered_left.event.source_ids, registered_right.event.source_ids),
					expr: Graph.EventExpr.Merge({ left: Box.box(registered_left.event), right: Box.box(registered_right.event) }),
				}
				{ runtime: registered_right.runtime, event: registered_event }
			}
		}
	}

	register_state_key : Runtime, Str, NodeValue -> RegisteredState
	register_state_key = |runtime, key, initial| {
		match state_slot_lookup(runtime.states, key) {
			StateSlotFound(found) => { runtime, index: found.index }
			StateSlotMissing => {
				index = List.len(runtime.states)
				{
					runtime: { ..runtime, states: List.append(runtime.states, { key, initial }) },
					index,
				}
			}
		}
	}

	register_event_key : Runtime, Str, U64 -> EventLookup
	register_event_key = |runtime, key, payload_kind| {
		match event_id_lookup(runtime.event_ids, key) {
			EventIdFound(existing) =>
				if existing.payload_kind == payload_kind {
					{ runtime, id: existing.id }
				} else {
					crash "Signals runtime invariant violated: event key was registered with conflicting payload kind"
				}
			EventIdMissing => {
				id = runtime.next_event_id
				{
					runtime: { ..runtime,
						event_ids: List.append(runtime.event_ids, { key, id, payload_kind }),
						next_event_id: id + 1,
					},
					id,
				}
			}
		}
	}

	drop : Box(Runtime) -> {}
	drop = |boxed_runtime| {
		_ = Box.unbox(boxed_runtime)
		{}
	}

	render_runtime : Runtime, ActiveEvent, DirtySignals, List(SignalValueDesc), List(StateValueDesc) -> RenderResult
	render_runtime = |runtime, active_event, dirty_signals, cached_signals, cached_states| {
		state = eval_state_for_runtime(runtime, active_event, dirty_signals, cached_signals, cached_states)
		scheduled_state = eval_signal_plan(state, dirty_signals.ids)
		render_state = {
			state: scheduled_state,
			render_elements: [],
			render_texts: [],
			render_signal_texts: [],
			render_bools: [],
			render_signal_bools: [],
			render_events: [],
			render_structures: [],
			next_elem_id: 1,
		}
		rendered = render_elem(render_state, runtime.root, 0)
		{
			runtime: rendered.state.runtime,
			render_elements: rendered.render_elements,
			render_texts: rendered.render_texts,
			render_signal_texts: rendered.render_signal_texts,
			render_bools: rendered.render_bools,
			render_signal_bools: rendered.render_signal_bools,
			render_events: rendered.render_events,
			render_structures: rendered.render_structures,
			changed_state_indexes: rendered.state.changed_state_indexes,
			state_changes: rendered.state.state_changes,
			signal_changes: rendered.state.signal_changes,
		}
	}

	eval_state_for_runtime : Runtime, ActiveEvent, DirtySignals, List(SignalValueDesc), List(StateValueDesc) -> EvalState
	eval_state_for_runtime = |runtime, active_event, dirty_signals, cached_signals, cached_states| {
		{
			runtime,
			active_event,
			dirty_signal_ids: dirty_signals.ids,
			cached_signals,
			cached_states,
			signal_changes: [],
			state_changes: [],
			updated_state_indexes: [],
			changed_state_indexes: [],
		}
	}

	eval_signal_plan : EvalState, List(U64) -> EvalState
	eval_signal_plan = |state, signal_ids| {
		List.fold(
			signal_ids,
			state,
			|acc, signal_id| {
				signal = signal_node_for_registered_id(acc.runtime, signal_id)
				result = eval_signal(acc, signal)
				result.state
			},
		)
	}

	add_render_element : RenderState, U64, U64, Str -> RenderState
	add_render_element = |render_state, elem_id, parent_id, tag| {
		{ ..render_state, render_elements: List.append(render_state.render_elements, { elem_id, parent_id, tag }) }
	}

	add_static_text_desc : RenderState, U64, U64, Str -> RenderState
	add_static_text_desc = |render_state, elem_id, field, value| {
		{ ..render_state, render_texts: List.append(render_state.render_texts, { elem_id, field, value }) }
	}

	add_signal_text_desc : RenderState, U64, U64, U64, Str -> RenderState
	add_signal_text_desc = |render_state, elem_id, field, signal_id, value| {
		{ ..render_state, render_signal_texts: List.append(render_state.render_signal_texts, { elem_id, field, signal_id, value }) }
	}

	add_static_bool_desc : RenderState, U64, U64, Bool -> RenderState
	add_static_bool_desc = |render_state, elem_id, field, value| {
		{ ..render_state, render_bools: List.append(render_state.render_bools, { elem_id, field, value }) }
	}

	add_signal_bool_desc : RenderState, U64, U64, U64, Bool -> RenderState
	add_signal_bool_desc = |render_state, elem_id, field, signal_id, value| {
		{ ..render_state, render_signal_bools: List.append(render_state.render_signal_bools, { elem_id, field, signal_id, value }) }
	}

	add_event_desc : RenderState, U64, U64, U64 -> RenderState
	add_event_desc = |render_state, elem_id, event_kind, event_id| {
		{ ..render_state, render_events: List.append(render_state.render_events, { elem_id, event_kind, event_id }) }
	}

	add_structural_desc : RenderState, U64, U64 -> RenderState
	add_structural_desc = |render_state, elem_id, signal_id| {
		{ ..render_state, render_structures: List.append(render_state.render_structures, { elem_id, signal_id }) }
	}

	render_signal_id : Graph.SignalNode -> RenderSignalLookup
	render_signal_id = |signal| {
		match signal.cache_key {
			Graph.SignalCacheKey.NoSignalCacheKey => RenderSignalMissing
			Graph.SignalCacheKey.SignalCacheKey(_) => RenderSignalFound(registered_signal_id(signal))
		}
	}

	add_text_sink_desc : RenderState, U64, U64, Graph.SignalNode, Str -> RenderState
	add_text_sink_desc = |render_state, elem_id, field, signal, value| {
		match render_signal_id(signal) {
			RenderSignalFound(signal_id) => add_signal_text_desc(render_state, elem_id, field, signal_id, value)
			RenderSignalMissing => add_static_text_desc(render_state, elem_id, field, value)
		}
	}

	add_bool_sink_desc : RenderState, U64, U64, Graph.SignalNode, Bool -> RenderState
	add_bool_sink_desc = |render_state, elem_id, field, signal, value| {
		match render_signal_id(signal) {
			RenderSignalFound(signal_id) => add_signal_bool_desc(render_state, elem_id, field, signal_id, value)
			RenderSignalMissing => add_static_bool_desc(render_state, elem_id, field, value)
		}
	}

	add_structural_signal_desc : RenderState, U64, Graph.SignalNode -> RenderState
	add_structural_signal_desc = |render_state, elem_id, signal| {
		match render_signal_id(signal) {
			RenderSignalFound(signal_id) => add_structural_desc(render_state, elem_id, signal_id)
			RenderSignalMissing => render_state
		}
	}

	create_child : RenderState, U64, Str -> { render_state : RenderState, id : U64 }
	create_child = |render_state, parent_id, tag| {
		id = render_state.next_elem_id
		next_state = add_render_element({ ..render_state, next_elem_id: id + 1 }, id, parent_id, tag)
		{ render_state: next_state, id }
	}

	render_children : RenderState, List(Elem), U64 -> RenderState
	render_children = |render_state, children, parent_id| {
		List.fold(children, render_state, |acc, child| render_elem(acc, child, parent_id))
	}

	render_elem : RenderState, Elem, U64 -> RenderState
	render_elem = |render_state, elem, parent_id| {
		match elem {
			Div(children) => {
				created = create_child(render_state, parent_id, "div")
				render_children(created.render_state, children, created.id)
			}

			Button({ on_click, label }) => {
				created = create_child(render_state, parent_id, "button")
				label_result = eval_signal(created.render_state.state, label)
				event_lookup = event_id_for_node(label_result.state.runtime, on_click)
				state1 = { ..label_result.state, runtime: event_lookup.runtime }
				with_state = { ..created.render_state, state: state1 }
				label_text = node_text(label_result.value)
				with_text_desc = add_text_sink_desc(with_state, created.id, render_text_field_text, label, label_text)
				add_event_desc(with_text_desc, created.id, render_event_kind_click, event_lookup.id)
			}

			ActionButton({ on_click, label, disabled }) => {
				created = create_child(render_state, parent_id, "button")
				label_result = eval_signal(created.render_state.state, label)
				disabled_result = eval_signal(label_result.state, disabled)
				event_lookup = event_id_for_node(disabled_result.state.runtime, on_click)
				state1 = { ..disabled_result.state, runtime: event_lookup.runtime }
				with_state = { ..created.render_state, state: state1 }
				label_text = node_text(label_result.value)
				disabled_value = node_bool(disabled_result.value)
				with_text_desc = add_text_sink_desc(with_state, created.id, render_text_field_text, label, label_text)
				with_bool_desc = add_bool_sink_desc(with_text_desc, created.id, render_bool_field_disabled, disabled, disabled_value)
				add_event_desc(with_bool_desc, created.id, render_event_kind_click, event_lookup.id)
			}

			Label({ signal }) => {
				created = create_child(render_state, parent_id, "span")
				result = eval_signal(created.render_state.state, signal)
				with_state = { ..created.render_state, state: result.state }
				text = node_text(result.value)
				add_text_sink_desc(with_state, created.id, render_text_field_text, signal, text)
			}

			Text(text) => {
				created = create_child(render_state, parent_id, "span")
				add_static_text_desc(created.render_state, created.id, render_text_field_text, text)
			}

			Heading(text) => {
				created = create_child(render_state, parent_id, "h2")
				with_role_desc = add_static_text_desc(created.render_state, created.id, render_text_field_role, "heading")
				add_static_text_desc(with_role_desc, created.id, render_text_field_text, text)
			}

			Paragraph(text) => {
				created = create_child(render_state, parent_id, "p")
				add_static_text_desc(created.render_state, created.id, render_text_field_text, text)
			}

			Section({ label, children }) => {
				created = create_child(render_state, parent_id, "section")
				with_role_desc = add_static_text_desc(created.render_state, created.id, render_text_field_role, "region")
				with_label_desc = add_static_text_desc(with_role_desc, created.id, render_text_field_label, label)
				render_children(with_label_desc, children, created.id)
			}

			TextInput({ label, value, on_input, disabled }) => {
				created = create_child(render_state, parent_id, "input")
				value_result = eval_signal(created.render_state.state, value)
				disabled_result = eval_signal(value_result.state, disabled)
				event_lookup = event_id_for_node(disabled_result.state.runtime, on_input)
				state1 = { ..disabled_result.state, runtime: event_lookup.runtime }
				with_state = { ..created.render_state, state: state1 }
				value_text = node_text(value_result.value)
				disabled_value = node_bool(disabled_result.value)
				with_role_desc = add_static_text_desc(with_state, created.id, render_text_field_role, "textbox")
				with_label_desc = add_static_text_desc(with_role_desc, created.id, render_text_field_label, label)
				with_value_desc = add_text_sink_desc(with_label_desc, created.id, render_text_field_value, value, value_text)
				with_bool_desc = add_bool_sink_desc(with_value_desc, created.id, render_bool_field_disabled, disabled, disabled_value)
				add_event_desc(with_bool_desc, created.id, render_event_kind_input, event_lookup.id)
			}

			Checkbox({ label, checked, on_check, disabled }) => {
				created = create_child(render_state, parent_id, "input")
				checked_result = eval_signal(created.render_state.state, checked)
				disabled_result = eval_signal(checked_result.state, disabled)
				event_lookup = event_id_for_node(disabled_result.state.runtime, on_check)
				state1 = { ..disabled_result.state, runtime: event_lookup.runtime }
				with_state = { ..created.render_state, state: state1 }
				checked_value = node_bool(checked_result.value)
				disabled_value = node_bool(disabled_result.value)
				with_role_desc = add_static_text_desc(with_state, created.id, render_text_field_role, "checkbox")
				with_label_desc = add_static_text_desc(with_role_desc, created.id, render_text_field_label, label)
				with_checked_desc = add_bool_sink_desc(with_label_desc, created.id, render_bool_field_checked, checked, checked_value)
				with_disabled_desc = add_bool_sink_desc(with_checked_desc, created.id, render_bool_field_disabled, disabled, disabled_value)
				add_event_desc(with_disabled_desc, created.id, render_event_kind_check, event_lookup.id)
			}

			Dynamic({ signal, render }) => {
				created = create_child(render_state, parent_id, "div")
				result = eval_signal(created.render_state.state, signal)
				with_signal_state = { ..created.render_state, state: result.state }
				with_structure_desc = add_structural_signal_desc(with_signal_state, created.id, signal)
				fn = Box.unbox(render)
				child = fn(result.value)
				registered_child = register_elem(result.state.runtime, child)
				state1 = { ..result.state, runtime: registered_child.runtime }
				render_child = { ..with_structure_desc, state: state1 }
				render_elem(render_child, registered_child.elem, created.id)
			}

			DynamicKeyed({ signal, key, render }) => {
				created = create_child(render_state, parent_id, "div")
				result = eval_signal(created.render_state.state, signal)
				with_signal_state = { ..created.render_state, state: result.state }
				with_structure_desc = add_structural_signal_desc(with_signal_state, created.id, signal)
				key_fn = Box.unbox(key)
				_ = key_fn(result.value)
				fn = Box.unbox(render)
				child = fn(result.value)
				registered_child = register_elem(result.state.runtime, child)
				metrics = registered_child.runtime.metrics
				state1 = { ..result.state, runtime: { ..registered_child.runtime, metrics: { ..metrics, rows_created: metrics.rows_created + 1 }
					}
				}
				render_child = { ..with_structure_desc, state: state1 }
				render_elem(render_child, registered_child.elem, created.id)
			}

			Each({ signal, key, render }) => {
				created = create_child(render_state, parent_id, "div")
				result = eval_signal(created.render_state.state, signal)
				with_signal_state = { ..created.render_state, state: result.state }
				with_structure_desc = add_structural_signal_desc(with_signal_state, created.id, signal)
				items = NodeValue.to_list(result.value)
				key_fn = Box.unbox(key)
				render_fn = Box.unbox(render)
				metrics = result.state.runtime.metrics
				state1 = { ..result.state, runtime: { ..result.state.runtime, metrics: { ..metrics, rows_created: metrics.rows_created + List.len(items) }
					}
				}
				start = { ..with_structure_desc, state: state1 }
				List.fold(
					items,
					start,
					|acc, item| {
						_ = key_fn(item)
						child = render_fn(item)
						registered_child = register_elem(acc.state.runtime, child)
						acc_with_registered = { ..acc, state: { ..acc.state, runtime: registered_child.runtime } }
						render_elem(acc_with_registered, registered_child.elem, created.id)
					},
				)
			}
		}
	}

	event_id_for_node : Runtime, Graph.EventNode -> EventLookup
	event_id_for_node = |runtime, event| {
		match event.expr {
			Graph.EventExpr.Source(_) =>
				match List.get(event.source_ids, 0) {
					Ok(id) => { runtime, id }
					Err(_) => {
						crash "Signals runtime invariant violated: event source id was not registered"
					}
				}
			_ => {
				crash "Signals runtime invariant violated: UI binding must use an event source"
			}
		}
	}

	eval_event : EvalState, Graph.EventNode -> EventResult
	eval_event = |state, event| {
		match event.expr {
			Graph.EventExpr.Source(_) => {
				lookup = event_id_for_node(state.runtime, event)
				state1 = { ..state, runtime: lookup.runtime }
				match state.active_event {
					Occurrence({ id, value }) =>
						if id == lookup.id {
							{ state: state1, values: [value] }
						} else {
							{ state: state1, values: [] }
						}

					NoEvent => { state: state1, values: [] }
				}
			}

			Graph.EventExpr.MapEvent({ source, transform }) => {
				source_result = eval_event(state, Box.unbox(source))
				fn = Box.unbox(transform)
				metrics = source_result.state.runtime.metrics
				state1 = { ..source_result.state, runtime: { ..source_result.state.runtime, metrics: { ..metrics, derived_calls_into_roc: metrics.derived_calls_into_roc + List.len(source_result.values) }
					}
				}
				values = List.map(source_result.values, |value| fn(value))
				{ state: state1, values }
			}

			Graph.EventExpr.MapUnitI64Const({ source, value }) => {
				source_result = eval_event(state, Box.unbox(source))
				values = List.map(source_result.values, |_| NodeValue.from_i64(value))
				{ state: source_result.state, values }
			}

			Graph.EventExpr.Merge({ left, right }) => {
				left_result = eval_event(state, Box.unbox(left))
				right_result = eval_event(left_result.state, Box.unbox(right))
				{ state: right_result.state, values: List.concat(left_result.values, right_result.values) }
			}
		}
	}

	eval_signal : EvalState, Graph.SignalNode -> EvalResult
	eval_signal = |state, signal| {
		match signal.cache_key {
			Graph.SignalCacheKey.NoSignalCacheKey => eval_signal_uncached(state, signal)
			Graph.SignalCacheKey.SignalCacheKey(_) =>
				if signal_is_dirty(signal, state.dirty_signal_ids) {
					signal_id = registered_signal_id(signal)
					match signal_cache_lookup(state.signal_changes, signal_id) {
						CacheHit(value) => { state, value }
						CacheMiss => {
							eval_signal_uncached(state, signal)
						}
					}
				} else {
					match signal_cache_lookup(state.cached_signals, registered_signal_id(signal)) {
						CacheHit(value) => {
							{ state, value }
						}

						CacheMiss => {
							eval_signal_uncached(state, signal)
						}
					}
				}
		}
	}

	eval_signal_uncached : EvalState, Graph.SignalNode -> EvalResult
	eval_signal_uncached = |state, signal| {
		metrics0 = state.runtime.metrics
		state_with_count = { ..state, runtime: { ..state.runtime, metrics: { ..metrics0, nodes_recomputed: metrics0.nodes_recomputed + 1 }
			}
		}
		result =
			match signal.expr {
				Graph.SignalExpr.ConstSignal(value) => { state: state_with_count, value }
				Graph.SignalExpr.ConstI64(value) => { state: state_with_count, value: NodeValue.from_i64(value) }
				Graph.SignalExpr.ConstBool(value) => { state: state_with_count, value: NodeValue.from_bool(value) }
				Graph.SignalExpr.ConstStr(value) => { state: state_with_count, value: NodeValue.from_str(value) }

				Graph.SignalExpr.MapSignal({ source, transform }) => {
					source_result = eval_signal(state_with_count, Box.unbox(source))
					fn = Box.unbox(transform)
					metrics = source_result.state.runtime.metrics
					state1 = { ..source_result.state, runtime: { ..source_result.state.runtime, metrics: { ..metrics, derived_calls_into_roc: metrics.derived_calls_into_roc + 1 }
						}
					}
					{ state: state1, value: fn(source_result.value) }
				}

				Graph.SignalExpr.MapI64I64({ source, transform }) => {
					source_result = eval_signal(state_with_count, Box.unbox(source))
					fn = Box.unbox(transform)
					metrics = source_result.state.runtime.metrics
					state1 = { ..source_result.state, runtime: { ..source_result.state.runtime, metrics: { ..metrics, derived_calls_into_roc: metrics.derived_calls_into_roc + 1 }
						}
					}
					{ state: state1, value: NodeValue.from_i64(fn(NodeValue.to_i64(source_result.value))) }
				}

				Graph.SignalExpr.MapI64Str({ source, transform }) => {
					source_result = eval_signal(state_with_count, Box.unbox(source))
					fn = Box.unbox(transform)
					metrics = source_result.state.runtime.metrics
					state1 = { ..source_result.state, runtime: { ..source_result.state.runtime, metrics: { ..metrics, derived_calls_into_roc: metrics.derived_calls_into_roc + 1 }
						}
					}
					{ state: state1, value: NodeValue.from_str(fn(NodeValue.to_i64(source_result.value))) }
				}

				Graph.SignalExpr.Map2Signal({ left, right, transform }) => {
					left_result = eval_signal(state_with_count, Box.unbox(left))
					right_result = eval_signal(left_result.state, Box.unbox(right))
					fn = Box.unbox(transform)
					metrics = right_result.state.runtime.metrics
					state1 = { ..right_result.state, runtime: { ..right_result.state.runtime, metrics: { ..metrics, derived_calls_into_roc: metrics.derived_calls_into_roc + 1 }
						}
					}
					{ state: state1, value: fn((left_result.value, right_result.value)) }
				}

				Graph.SignalExpr.Map2I64I64({ left, right, transform }) => {
					left_result = eval_signal(state_with_count, Box.unbox(left))
					right_result = eval_signal(left_result.state, Box.unbox(right))
					fn = Box.unbox(transform)
					metrics = right_result.state.runtime.metrics
					state1 = { ..right_result.state, runtime: { ..right_result.state.runtime, metrics: { ..metrics, derived_calls_into_roc: metrics.derived_calls_into_roc + 1 }
						}
					}
					{ state: state1, value: NodeValue.from_i64(fn((NodeValue.to_i64(left_result.value), NodeValue.to_i64(right_result.value)))) }
				}

				Graph.SignalExpr.Map2I64I64Str({ left, right, transform }) => {
					left_result = eval_signal(state_with_count, Box.unbox(left))
					right_result = eval_signal(left_result.state, Box.unbox(right))
					fn = Box.unbox(transform)
					metrics = right_result.state.runtime.metrics
					state1 = { ..right_result.state, runtime: { ..right_result.state.runtime, metrics: { ..metrics, derived_calls_into_roc: metrics.derived_calls_into_roc + 1 }
						}
					}
					{ state: state1, value: NodeValue.from_str(fn((NodeValue.to_i64(left_result.value), NodeValue.to_i64(right_result.value)))) }
				}

				Graph.SignalExpr.Hold({ key: _, initial: _, event }) => {
					event_node = Box.unbox(event)
					state_index = state_index_for_signal(signal)
					current = state_value_by_index(state_with_count, state_index)
					state1 = { ..state_with_count, runtime: current.runtime }
					if u64_list_contains(state1.updated_state_indexes, current.index) {
						{ state: state1, value: current.value }
					} else {
						event_result = eval_event(state1, event_node)
						next_value = List.fold(event_result.values, current.value, |_acc, value| value)
						finish_state_update(event_result.state, current.index, current.value, next_value, !List.is_empty(event_result.values))
					}
				}

				Graph.SignalExpr.Fold({ key: _, initial: _, event, step }) => {
					event_node = Box.unbox(event)
					state_index = state_index_for_signal(signal)
					current = state_value_by_index(state_with_count, state_index)
					state1 = { ..state_with_count, runtime: current.runtime }
					if u64_list_contains(state1.updated_state_indexes, current.index) {
						{ state: state1, value: current.value }
					} else {
						event_result = eval_event(state1, event_node)
						fn = Box.unbox(step)
						metrics = event_result.state.runtime.metrics
						state2 = { ..event_result.state, runtime: { ..event_result.state.runtime, metrics: { ..metrics, derived_calls_into_roc: metrics.derived_calls_into_roc + List.len(event_result.values) }
							}
						}
						next_value = List.fold(event_result.values, current.value, |acc, evt| fn((acc, evt)))
						finish_state_update(state2, current.index, current.value, next_value, !List.is_empty(event_result.values))
					}
				}

				Graph.SignalExpr.FoldI64({ key: _, initial: _, event, step }) => {
					event_node = Box.unbox(event)
					state_index = state_index_for_signal(signal)
					current = state_value_by_index(state_with_count, state_index)
					state1 = { ..state_with_count, runtime: current.runtime }
					if u64_list_contains(state1.updated_state_indexes, current.index) {
						{ state: state1, value: current.value }
					} else {
						event_result = eval_event(state1, event_node)
						fn = Box.unbox(step)
						metrics = event_result.state.runtime.metrics
						state2 = { ..event_result.state, runtime: { ..event_result.state.runtime, metrics: { ..metrics, derived_calls_into_roc: metrics.derived_calls_into_roc + List.len(event_result.values) }
							}
						}
						next_i64 = List.fold(event_result.values, NodeValue.to_i64(current.value), |acc, evt| fn((acc, NodeValue.to_i64(evt))))
						finish_state_update(state2, current.index, current.value, NodeValue.from_i64(next_i64), !List.is_empty(event_result.values))
					}
				}

				Graph.SignalExpr.FoldBoolToggle({ key: _, initial: _, event }) => {
					event_node = Box.unbox(event)
					state_index = state_index_for_signal(signal)
					current = state_value_by_index(state_with_count, state_index)
					state1 = { ..state_with_count, runtime: current.runtime }
					if u64_list_contains(state1.updated_state_indexes, current.index) {
						{ state: state1, value: current.value }
					} else {
						event_result = eval_event(state1, event_node)
						next_bool = List.fold(event_result.values, node_bool(current.value), |acc, _evt| !acc)
						finish_state_update(event_result.state, current.index, current.value, NodeValue.from_bool(next_bool), !List.is_empty(event_result.values))
					}
				}
			}
		cache_signal_result(signal, result)
	}

	cache_signal_result : Graph.SignalNode, EvalResult -> EvalResult
	cache_signal_result = |signal, result| {
		match signal.cache_key {
			Graph.SignalCacheKey.NoSignalCacheKey => result
			Graph.SignalCacheKey.SignalCacheKey(_) => {
				signal_id = registered_signal_id(signal)
				next_changes = replace_signal_change(result.state.signal_changes, signal_id, result.value)
				{ state: { ..result.state, signal_changes: next_changes }, value: result.value }
			}
		}
	}

	signal_is_dirty : Graph.SignalNode, List(U64) -> Bool
	signal_is_dirty = |signal, dirty_signal_ids| {
		u64_list_contains(dirty_signal_ids, registered_signal_id(signal))
	}

	registered_signal_id : Graph.SignalNode -> U64
	registered_signal_id = |signal| {
		match signal.signal_id {
			Graph.SignalIdentity.RegisteredSignal(id) => id
			Graph.SignalIdentity.UnregisteredSignal => {
				crash "Signals runtime invariant violated: keyed signal was not registered"
			}
		}
	}

	signal_node_for_registered_id : Runtime, U64 -> Graph.SignalNode
	signal_node_for_registered_id = |runtime, signal_id| {
		match List.get(runtime.signal_registry, signal_id) {
			Ok(entry) =>
				if entry.signal_id == signal_id {
					entry.signal
				} else {
					crash "Signals runtime invariant violated: signal registry is not indexed by signal id"
				}
			Err(_) => {
				crash "Signals runtime invariant violated: host recompute plan referenced an unknown signal id"
			}
		}
	}

	event_id_lookup : List(EventId), Str -> EventIdLookup
	event_id_lookup = |event_ids, key| {
		var $index = 0
		var $result = EventIdMissing
		var $done = False

		while $done == False and $index < List.len(event_ids) {
			match List.get(event_ids, $index) {
				Ok(entry) =>
					if entry.key == key {
						$result = EventIdFound(entry)
						$done = True
					} else {
						$index = $index + 1
					}
				Err(_) => {
					$done = True
				}
			}
		}

		$result
	}

	signal_registry_lookup : List(SignalRegistryEntry), Str, U64, List(U64), List(U64), List(U64) -> SignalRegistryLookup
	signal_registry_lookup = |entries, key, kind, source_state_ids, source_event_ids, input_signal_ids| {
		var $index = 0
		var $result = SignalRegistryMissing
		var $done = False

		while $done == False and $index < List.len(entries) {
			match List.get(entries, $index) {
				Ok(entry) =>
					if entry.key == key and entry.kind == kind and u64_list_equal(entry.source_state_ids, source_state_ids) and u64_list_equal(entry.source_event_ids, source_event_ids) and u64_list_equal(entry.input_signal_ids, input_signal_ids) {
						$result = SignalRegistryFound(entry)
						$done = True
					} else {
						$index = $index + 1
					}
				Err(_) => {
					$done = True
				}
			}
		}

		$result
	}

	state_change_lookup : List(StateValueDesc), U64 -> StateChangeLookup
	state_change_lookup = |entries, state_id| {
		var $index = 0
		var $result = StateChangeMissing
		var $done = False

		while $done == False and $index < List.len(entries) {
			match List.get(entries, $index) {
				Ok(entry) =>
					if entry.state_id == state_id {
						$result = StateChangeFound(entry.value)
						$done = True
					} else {
						$index = $index + 1
					}
				Err(_) => {
					$done = True
				}
			}
		}

		$result
	}

	signal_cache_lookup : List(SignalValueDesc), U64 -> CacheLookup
	signal_cache_lookup = |entries, signal_id| {
		var $index = 0
		var $result = CacheMiss
		var $done = False

		while $done == False and $index < List.len(entries) {
			match List.get(entries, $index) {
				Ok(entry) =>
					if entry.signal_id == signal_id {
						$result = CacheHit(entry.value)
						$done = True
					} else {
						$index = $index + 1
					}
				Err(_) => {
					$done = True
				}
			}
		}

		$result
	}

	state_slot_lookup : List(StateSlot), Str -> StateSlotLookup
	state_slot_lookup = |slots, key| {
		var $index = 0
		var $result = StateSlotMissing
		var $done = False

		while $done == False and $index < List.len(slots) {
			match List.get(slots, $index) {
				Ok(slot) =>
					if slot.key == key {
						$result = StateSlotFound({ index: $index, slot })
						$done = True
					} else {
						$index = $index + 1
					}
				Err(_) => {
					$done = True
				}
			}
		}

		$result
	}

	state_descriptors_for_runtime : Runtime -> List(StateDesc)
	state_descriptors_for_runtime = |runtime| {
		var $state_id = 0
		var $descriptors = List.with_capacity(List.len(runtime.states))

		for slot in runtime.states {
			$descriptors = List.append($descriptors, { state_id: $state_id, value: slot.initial })
			$state_id = $state_id + 1
		}

		$descriptors
	}

	event_descriptors_for_runtime : Runtime -> List(EventDesc)
	event_descriptors_for_runtime = |runtime| {
		List.map(
			runtime.event_ids,
			|event| {
				{ event_id: event.id, payload_kind: event.payload_kind }
			},
		)
	}

	signal_descriptors_for_runtime : Runtime -> List(SignalDesc)
	signal_descriptors_for_runtime = |runtime| {
		List.map(
			runtime.signal_registry,
			|entry| {
				{ signal_id: entry.signal_id, kind: entry.kind, source_state_ids: entry.source_state_ids, source_event_ids: entry.source_event_ids, input_signal_ids: entry.input_signal_ids }
			},
		)
	}

	replace_state_change : List(StateValueDesc), U64, NodeValue -> List(StateValueDesc)
	replace_state_change = |entries, state_id, value| {
		var $found = False
		var $next = List.with_capacity(List.len(entries) + 1)

		for entry in entries {
			if entry.state_id == state_id {
				$next = List.append($next, { state_id, value })
				$found = True
			} else {
				$next = List.append($next, entry)
			}
		}

		if $found {
			$next
		} else {
			List.append($next, { state_id, value })
		}
	}

	replace_signal_change : List(SignalValueDesc), U64, NodeValue -> List(SignalValueDesc)
	replace_signal_change = |entries, signal_id, value| {
		var $found = False
		var $next = List.with_capacity(List.len(entries) + 1)

		for entry in entries {
			if entry.signal_id == signal_id {
				$next = List.append($next, { signal_id, value })
				$found = True
			} else {
				$next = List.append($next, entry)
			}
		}

		if $found {
			$next
		} else {
			List.append($next, { signal_id, value })
		}
	}

	merge_u64_deps : List(U64), List(U64) -> List(U64)
	merge_u64_deps = |left, right| {
		var $deps = left

		for dep in right {
			if u64_list_contains($deps, dep) {
				{}
			} else {
				$deps = List.append($deps, dep)
			}
		}

		$deps
	}

	u64_list_equal : List(U64), List(U64) -> Bool
	u64_list_equal = |left, right| {
		if List.len(left) != List.len(right) {
			False
		} else {
			var $index = 0
			var $same = True

			while $same and $index < List.len(left) {
				match (List.get(left, $index), List.get(right, $index)) {
					(Ok(left_item), Ok(right_item)) =>
						if left_item == right_item {
							$index = $index + 1
						} else {
							$same = False
						}
					_ => {
						$same = False
					}
				}
			}

			$same
		}
	}

	u64_list_contains : List(U64), U64 -> Bool
	u64_list_contains = |items, item| {
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

	state_index_for_signal : Graph.SignalNode -> U64
	state_index_for_signal = |signal| {
		match List.get(signal.dep_indexes, 0) {
			Ok(index) => index
			Err(_) => {
				crash "Signals runtime invariant violated: signal state index was not registered"
			}
		}
	}

	state_value_by_index : EvalState, U64 -> StateValue
	state_value_by_index = |state, state_index| {
		match state_change_lookup(state.state_changes, state_index) {
			StateChangeFound(value) => { runtime: state.runtime, value, index: state_index }
			StateChangeMissing =>
				if state_index < List.len(state.cached_states) {
					match List.get(state.cached_states, state_index) {
						Ok(entry) =>
							if entry.state_id == state_index {
								{ runtime: state.runtime, value: entry.value, index: state_index }
							} else {
								crash "Signals runtime invariant violated: host state values must be dense and ordered by state id"
							}
						Err(_) => {
							crash "Signals runtime invariant violated: cached state index was not present"
						}
					}
				} else {
					match List.get(state.runtime.states, state_index) {
						Ok(slot) => { runtime: state.runtime, value: slot.initial, index: state_index }
						Err(_) => {
							crash "Signals runtime invariant violated: state index was not registered"
						}
					}
				}
		}
	}

	finish_state_update : EvalState, U64, NodeValue, NodeValue, Bool -> EvalResult
	finish_state_update = |state, state_index, previous, next, had_event| {
		if had_event {
			metrics0 = state.runtime.metrics
			changed = !node_value_equal(previous, next)
			metrics1 =
				if changed {
					metrics0
				} else {
					{ ..metrics0,
						propagation_prunes: metrics0.propagation_prunes + 1,
					}
				}
			next_state_changes =
				if changed {
					replace_state_change(state.state_changes, state_index, next)
				} else {
					state.state_changes
				}
			next_changed_state_indexes =
				if changed {
					List.append(state.changed_state_indexes, state_index)
				} else {
					state.changed_state_indexes
				}
			state1 = {
				..state,
				runtime: { ..state.runtime, metrics: metrics1 },
				updated_state_indexes: List.append(state.updated_state_indexes, state_index),
				changed_state_indexes: next_changed_state_indexes,
				state_changes: next_state_changes,
			}
			{ state: state1, value: next }
		} else {
			{ state, value: previous }
		}
	}

	node_text : NodeValue -> Str
	node_text = |value| {
		match value {
			NvStr(str) => str
			NvI64(int) => int.to_str()
			NvBool(bool) =>
				if bool {
					"true"
				} else {
					"false"
				}
			NvUnit => ""
			NvF64(_) => ""
			NvList(_) => ""
		}
	}

	node_bool : NodeValue -> Bool
	node_bool = |value| {
		match value {
			NvBool(bool) => bool
			_ => {
				crash "Signals runtime invariant violated: expected Bool NodeValue"
			}
		}
	}

	node_value_equal : NodeValue, NodeValue -> Bool
	node_value_equal = |left, right| {
		match (left, right) {
			(NvI64(a), NvI64(b)) => a == b
			(NvBool(a), NvBool(b)) => a == b
			(NvStr(a), NvStr(b)) => a == b
			(NvUnit, NvUnit) => True
			(NvList(a), NvList(b)) => list_node_value_equal(a, b)
			_ => False
		}
	}

	list_node_value_equal : List(NodeValue), List(NodeValue) -> Bool
	list_node_value_equal = |left, right| {
		if List.len(left) != List.len(right) {
			False
		} else {
			var $index = 0
			var $same = True

			while $same and $index < List.len(left) {
				match (List.get(left, $index), List.get(right, $index)) {
					(Ok(a), Ok(b)) =>
						if node_value_equal(a, b) {
							$index = $index + 1
						} else {
							$same = False
						}
					_ => {
						$same = False
					}
				}
			}

			$same
		}
	}
}
