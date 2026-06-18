import Elem exposing [Elem]
import Graph
import NodeValue exposing [NodeValue]

UiRuntime := [].{
	RuntimeMetrics : {
		events_processed : U64,
		evaluated_nodes : U64,
		signal_writes : U64,
		signal_changes : U64,
		signal_suppressed : U64,
		callbacks : U64,
		dynamic_renders : U64,
		keyed_creates : U64,
		keyed_reuses : U64,
		keyed_removes : U64,
		node_value_equality_checks : U64,
		graph_nodes : U64,
		commands_emitted : U64,
		full_render_batches : U64,
		incremental_batches : U64,
		structural_resets : U64,
		state_lookups : U64,
		event_lookups : U64,
		retained_graph_dispatches : U64,
		signal_cache_hits : U64,
		signal_cache_misses : U64,
		stale_signal_cache_misses : U64,
		clean_signal_skips : U64,
		state_version_bumps : U64,
	}

	EventId : { key : Str, id : U64, payload_kind : U64 }

	StateSlot : { key : Str, value : NodeValue, version : U64, event_ids : List(U64) }

	StateDesc : { state_id : U64, value : NodeValue, event_ids : List(U64) }

	StateChangeDesc : { state_id : U64, value : NodeValue }

	SignalDesc : { signal_id : U64, kind : U64, state_deps : List(U64) }

	SignalRegistryEntry : { key : Str, signal_id : U64, kind : U64, state_deps : List(U64) }

	StateVersionEntry : { state_index : U64, version : U64 }

	SignalCacheEntry : { key : Str, value : NodeValue, deps : List(StateVersionEntry) }

	EventDesc : { event_id : U64, payload_kind : U64 }

	HostEvent := [
		Click({ event : U64, dirty_signal_ids : List(U64) }),
		Input({ event : U64, dirty_signal_ids : List(U64), value : Str }),
		Check({ event : U64, dirty_signal_ids : List(U64), checked : Bool }),
	]

	Command := [
		ResetDom,
		CreateElement({ id : U64, tag : Str }),
		AppendChild({ parent : U64, child : U64 }),
		SetText({ id : U64, value : Str }),
		SetRole({ id : U64, value : Str }),
		SetLabel({ id : U64, value : Str }),
		SetTestId({ id : U64, value : Str }),
		SetValue({ id : U64, value : Str }),
		SetChecked({ id : U64, value : Bool }),
		SetDisabled({ id : U64, value : Bool }),
		BindClick({ id : U64, event : U64 }),
		BindInput({ id : U64, event : U64 }),
		BindCheck({ id : U64, event : U64 }),
	]

	Runtime : {
		root : Elem,
		states : List(StateSlot),
		event_ids : List(EventId),
		next_event_id : U64,
		next_signal_id : U64,
		signal_registry : List(SignalRegistryEntry),
		signal_cache : List(SignalCacheEntry),
		metrics : RuntimeMetrics,
	}

	DispatchResult : {
		runtime : Box(Runtime),
		commands : List(Command),
		event_descriptors : List(EventDesc),
		signal_descriptors : List(SignalDesc),
		state_descriptors : List(StateDesc),
		state_changes : List(StateChangeDesc),
		metrics : RuntimeMetrics,
	}

	ActiveEvent := [
		NoEvent,
		Occurrence({ id : U64, value : NodeValue }),
	]

	EvalState : {
		runtime : Runtime,
		active_event : ActiveEvent,
		dirty_signal_ids : List(U64),
		updated_state_indexes : List(U64),
		changed_state_indexes : List(U64),
	}

	EvalResult : { state : EvalState, value : NodeValue }

	EventResult : { state : EvalState, values : List(NodeValue) }

	RenderState : {
		state : EvalState,
		commands : List(Command),
		next_elem_id : U64,
	}

	RenderResult : {
		runtime : Runtime,
		emit_commands : List(Command),
		changed_state_indexes : List(U64),
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

	SignalCacheEntryLookup := [
		SignalCacheEntryFound({ index : U64, entry : SignalCacheEntry }),
		SignalCacheEntryMissing,
	]

	CacheLookup := [
		CacheHit(NodeValue),
		CacheMiss,
		StaleCacheMiss,
	]

	VersionLookup := [
		VersionFound(U64),
		VersionMissing,
	]

	StateValue : { runtime : Runtime, value : NodeValue, index : U64 }

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

	zero_metrics : RuntimeMetrics
	zero_metrics = {
		events_processed: 0,
		evaluated_nodes: 0,
		signal_writes: 0,
		signal_changes: 0,
		signal_suppressed: 0,
		callbacks: 0,
		dynamic_renders: 0,
		keyed_creates: 0,
		keyed_reuses: 0,
		keyed_removes: 0,
		node_value_equality_checks: 0,
		graph_nodes: 0,
		commands_emitted: 0,
		full_render_batches: 0,
		incremental_batches: 0,
		structural_resets: 0,
		state_lookups: 0,
		event_lookups: 0,
		retained_graph_dispatches: 0,
		signal_cache_hits: 0,
		signal_cache_misses: 0,
		stale_signal_cache_misses: 0,
		clean_signal_skips: 0,
		state_version_bumps: 0,
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
			signal_cache: [],
			metrics: zero_metrics,
		}
		registered = register_elem(runtime0, root)
		runtime = { ..registered.runtime, root: registered.elem }
		rendered = render_runtime(runtime, NoEvent, { ids: [] })
		runtime_box = Box.box(rendered.runtime)
		result = {
			runtime: runtime_box,
			commands: rendered.emit_commands,
			event_descriptors: event_descriptors_for_runtime(rendered.runtime),
			signal_descriptors: signal_descriptors_for_runtime(rendered.runtime),
			state_descriptors: state_descriptors_for_runtime(rendered.runtime),
			state_changes: state_changes_for_indexes(rendered.runtime, rendered.changed_state_indexes),
			metrics: rendered.runtime.metrics,
		}
		result
	}

	dispatch : Box(Runtime), HostEvent -> DispatchResult
	dispatch = |boxed_runtime, host_event| {
		runtime0 = Box.unbox(boxed_runtime)
		active_event = host_event_to_active(host_event)
		dirty_signals = { ids: host_event_dirty_signal_ids(host_event) }
		rendered = render_runtime(runtime0, active_event, dirty_signals)
		{
			runtime: Box.box(rendered.runtime),
			commands: rendered.emit_commands,
			event_descriptors: event_descriptors_for_runtime(rendered.runtime),
			signal_descriptors: signal_descriptors_for_runtime(rendered.runtime),
			state_descriptors: state_descriptors_for_runtime(rendered.runtime),
			state_changes: state_changes_for_indexes(rendered.runtime, rendered.changed_state_indexes),
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
				register_signal_descriptor(registered_source.runtime, registered_signal0, signal_kind_map)
			}

			Graph.SignalExpr.MapI64I64({ source, transform }) => {
				registered_source = register_signal(runtime, Box.unbox(source))
				registered_signal0 = { ..signal,
					dep_indexes: registered_source.signal.dep_indexes,
					expr: Graph.SignalExpr.MapI64I64({ source: Box.box(registered_source.signal), transform }),
				}
				register_signal_descriptor(registered_source.runtime, registered_signal0, signal_kind_map)
			}

			Graph.SignalExpr.MapI64Str({ source, transform }) => {
				registered_source = register_signal(runtime, Box.unbox(source))
				registered_signal0 = { ..signal,
					dep_indexes: registered_source.signal.dep_indexes,
					expr: Graph.SignalExpr.MapI64Str({ source: Box.box(registered_source.signal), transform }),
				}
				register_signal_descriptor(registered_source.runtime, registered_signal0, signal_kind_map)
			}

			Graph.SignalExpr.Map2Signal({ left, right, transform }) => {
				registered_left = register_signal(runtime, Box.unbox(left))
				registered_right = register_signal(registered_left.runtime, Box.unbox(right))
				registered_signal0 = { ..signal,
					dep_indexes: merge_u64_deps(registered_left.signal.dep_indexes, registered_right.signal.dep_indexes),
					expr: Graph.SignalExpr.Map2Signal({ left: Box.box(registered_left.signal), right: Box.box(registered_right.signal), transform }),
				}
				register_signal_descriptor(registered_right.runtime, registered_signal0, signal_kind_map2)
			}

			Graph.SignalExpr.Map2I64I64({ left, right, transform }) => {
				registered_left = register_signal(runtime, Box.unbox(left))
				registered_right = register_signal(registered_left.runtime, Box.unbox(right))
				registered_signal0 = { ..signal,
					dep_indexes: merge_u64_deps(registered_left.signal.dep_indexes, registered_right.signal.dep_indexes),
					expr: Graph.SignalExpr.Map2I64I64({ left: Box.box(registered_left.signal), right: Box.box(registered_right.signal), transform }),
				}
				register_signal_descriptor(registered_right.runtime, registered_signal0, signal_kind_map2)
			}

			Graph.SignalExpr.Map2I64I64Str({ left, right, transform }) => {
				registered_left = register_signal(runtime, Box.unbox(left))
				registered_right = register_signal(registered_left.runtime, Box.unbox(right))
				registered_signal0 = { ..signal,
					dep_indexes: merge_u64_deps(registered_left.signal.dep_indexes, registered_right.signal.dep_indexes),
					expr: Graph.SignalExpr.Map2I64I64Str({ left: Box.box(registered_left.signal), right: Box.box(registered_right.signal), transform }),
				}
				register_signal_descriptor(registered_right.runtime, registered_signal0, signal_kind_map2)
			}

			Graph.SignalExpr.Hold({ key, initial, event }) => {
				registered_event = register_event(runtime, Box.unbox(event))
				registered_state = register_state_key(registered_event.runtime, key, initial)
				runtime1 = register_state_event_indexes(registered_state.runtime, registered_state.index, registered_event.event.source_ids)
				registered_signal0 = { ..signal,
					dep_indexes: [registered_state.index],
					expr: Graph.SignalExpr.Hold({ key, initial, event: Box.box(registered_event.event) }),
				}
				register_signal_descriptor(runtime1, registered_signal0, signal_kind_source)
			}

			Graph.SignalExpr.Fold({ key, initial, event, step }) => {
				registered_event = register_event(runtime, Box.unbox(event))
				registered_state = register_state_key(registered_event.runtime, key, initial)
				runtime1 = register_state_event_indexes(registered_state.runtime, registered_state.index, registered_event.event.source_ids)
				registered_signal0 = { ..signal,
					dep_indexes: [registered_state.index],
					expr: Graph.SignalExpr.Fold({ key, initial, event: Box.box(registered_event.event), step }),
				}
				register_signal_descriptor(runtime1, registered_signal0, signal_kind_source)
			}

			Graph.SignalExpr.FoldI64({ key, initial, event, step }) => {
				registered_event = register_event(runtime, Box.unbox(event))
				registered_state = register_state_key(registered_event.runtime, key, NodeValue.from_i64(initial))
				runtime1 = register_state_event_indexes(registered_state.runtime, registered_state.index, registered_event.event.source_ids)
				registered_signal0 = { ..signal,
					dep_indexes: [registered_state.index],
					expr: Graph.SignalExpr.FoldI64({ key, initial, event: Box.box(registered_event.event), step }),
				}
				register_signal_descriptor(runtime1, registered_signal0, signal_kind_source)
			}

			Graph.SignalExpr.FoldBoolToggle({ key, initial, event }) => {
				registered_event = register_event(runtime, Box.unbox(event))
				registered_state = register_state_key(registered_event.runtime, key, NodeValue.from_bool(initial))
				runtime1 = register_state_event_indexes(registered_state.runtime, registered_state.index, registered_event.event.source_ids)
				registered_signal0 = { ..signal,
					dep_indexes: [registered_state.index],
					expr: Graph.SignalExpr.FoldBoolToggle({ key, initial, event: Box.box(registered_event.event) }),
				}
				register_signal_descriptor(runtime1, registered_signal0, signal_kind_source)
			}
		}
	}

	register_signal_descriptor : Runtime, Graph.SignalNode, U64 -> RegisteredSignal
	register_signal_descriptor = |runtime, signal, kind| {
		match signal.cache_key {
			Graph.SignalCacheKey.NoSignalCacheKey => { runtime, signal }
			Graph.SignalCacheKey.SignalCacheKey(key) =>
				match signal_registry_lookup(runtime.signal_registry, key, kind, signal.dep_indexes) {
					SignalRegistryFound(entry) => {
						{
							runtime,
							signal: { ..signal, signal_id: Graph.SignalIdentity.RegisteredSignal(entry.signal_id) },
						}
					}

					SignalRegistryMissing => {
						signal_id = runtime.next_signal_id
						entry = { key, signal_id, kind, state_deps: signal.dep_indexes }
						{
							runtime: { ..runtime,
								next_signal_id: signal_id + 1,
								signal_registry: List.append(runtime.signal_registry, entry),
							},
							signal: { ..signal, signal_id: Graph.SignalIdentity.RegisteredSignal(signal_id) },
						}
					}
				}
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
					runtime: { ..runtime, states: List.append(runtime.states, { key, value: initial, version: 0, event_ids: [] }) },
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

	register_state_event_indexes : Runtime, U64, List(U64) -> Runtime
	register_state_event_indexes = |runtime, state_index, event_ids| {
		List.fold(
			event_ids,
			runtime,
			|acc, event_id| register_state_event_index(acc, event_id, state_index),
		)
	}

	register_state_event_index : Runtime, U64, U64 -> Runtime
	register_state_event_index = |runtime, event_id, state_index| {
		match List.get(runtime.states, state_index) {
			Ok(slot) =>
				if u64_list_contains(slot.event_ids, event_id) {
					runtime
				} else {
					next = { ..slot, event_ids: List.append(slot.event_ids, event_id) }
					{ ..runtime, states: replace_state_slot(runtime.states, state_index, next) }
				}

			Err(_) => {
				crash "Signals runtime invariant violated: state dependency slot missing"
			}
		}
	}

	drop : Box(Runtime) -> {}
	drop = |boxed_runtime| {
		_ = Box.unbox(boxed_runtime)
		{}
	}

	host_event_to_active : HostEvent -> ActiveEvent
	host_event_to_active = |host_event| {
		match host_event {
			Click({ event }) => Occurrence({ id: event, value: NodeValue.unit })
			Input({ event, value }) => Occurrence({ id: event, value: NodeValue.from_str(value) })
			Check({ event, checked }) => Occurrence({ id: event, value: NodeValue.from_bool(checked) })
		}
	}

	host_event_dirty_signal_ids : HostEvent -> List(U64)
	host_event_dirty_signal_ids = |host_event| {
		match host_event {
			Click({ dirty_signal_ids }) => dirty_signal_ids
			Input({ dirty_signal_ids }) => dirty_signal_ids
			Check({ dirty_signal_ids }) => dirty_signal_ids
		}
	}

	render_runtime : Runtime, ActiveEvent, DirtySignals -> RenderResult
	render_runtime = |runtime, active_event, dirty_signals| {
		state = {
			runtime,
			active_event,
			dirty_signal_ids: dirty_signals.ids,
			updated_state_indexes: [],
			changed_state_indexes: [],
		}
		render_state = {
			state,
			commands: [ResetDom],
			next_elem_id: 1,
		}
		rendered = render_elem(render_state, runtime.root, 0)
		{
			runtime: rendered.state.runtime,
			emit_commands: rendered.commands,
			changed_state_indexes: rendered.state.changed_state_indexes,
		}
	}

	add_command : RenderState, Command -> RenderState
	add_command = |render_state, command| {
		{ ..render_state, commands: List.append(render_state.commands, command) }
	}

	create_child : RenderState, U64, Str -> { render_state : RenderState, id : U64 }
	create_child = |render_state, parent_id, tag| {
		id = render_state.next_elem_id
		next_state = { ..render_state, next_elem_id: id + 1 }
		with_create = add_command(next_state, CreateElement({ id, tag }))
		with_append = add_command(with_create, AppendChild({ parent: parent_id, child: id }))
		{ render_state: with_append, id }
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
				with_text = add_command(with_state, SetText({ id: created.id, value: node_text(label_result.value) }))
				add_command(with_text, BindClick({ id: created.id, event: event_lookup.id }))
			}

			ActionButton({ on_click, label, disabled }) => {
				created = create_child(render_state, parent_id, "button")
				label_result = eval_signal(created.render_state.state, label)
				disabled_result = eval_signal(label_result.state, disabled)
				event_lookup = event_id_for_node(disabled_result.state.runtime, on_click)
				state1 = { ..disabled_result.state, runtime: event_lookup.runtime }
				with_state = { ..created.render_state, state: state1 }
				with_text = add_command(with_state, SetText({ id: created.id, value: node_text(label_result.value) }))
				with_disabled = add_command(with_text, SetDisabled({ id: created.id, value: node_bool(disabled_result.value) }))
				add_command(with_disabled, BindClick({ id: created.id, event: event_lookup.id }))
			}

			Label({ signal }) => {
				created = create_child(render_state, parent_id, "span")
				result = eval_signal(created.render_state.state, signal)
				with_state = { ..created.render_state, state: result.state }
				add_command(with_state, SetText({ id: created.id, value: node_text(result.value) }))
			}

			Text(text) => {
				created = create_child(render_state, parent_id, "span")
				add_command(created.render_state, SetText({ id: created.id, value: text }))
			}

			Heading(text) => {
				created = create_child(render_state, parent_id, "h2")
				with_role = add_command(created.render_state, SetRole({ id: created.id, value: "heading" }))
				add_command(with_role, SetText({ id: created.id, value: text }))
			}

			Paragraph(text) => {
				created = create_child(render_state, parent_id, "p")
				add_command(created.render_state, SetText({ id: created.id, value: text }))
			}

			Section({ label, children }) => {
				created = create_child(render_state, parent_id, "section")
				with_role = add_command(created.render_state, SetRole({ id: created.id, value: "region" }))
				with_label = add_command(with_role, SetLabel({ id: created.id, value: label }))
				render_children(with_label, children, created.id)
			}

			TextInput({ label, value, on_input, disabled }) => {
				created = create_child(render_state, parent_id, "input")
				value_result = eval_signal(created.render_state.state, value)
				disabled_result = eval_signal(value_result.state, disabled)
				event_lookup = event_id_for_node(disabled_result.state.runtime, on_input)
				state1 = { ..disabled_result.state, runtime: event_lookup.runtime }
				with_state = { ..created.render_state, state: state1 }
				with_role = add_command(with_state, SetRole({ id: created.id, value: "textbox" }))
				with_label = add_command(with_role, SetLabel({ id: created.id, value: label }))
				with_value = add_command(with_label, SetValue({ id: created.id, value: node_text(value_result.value) }))
				with_disabled = add_command(with_value, SetDisabled({ id: created.id, value: node_bool(disabled_result.value) }))
				add_command(with_disabled, BindInput({ id: created.id, event: event_lookup.id }))
			}

			Checkbox({ label, checked, on_check, disabled }) => {
				created = create_child(render_state, parent_id, "input")
				checked_result = eval_signal(created.render_state.state, checked)
				disabled_result = eval_signal(checked_result.state, disabled)
				event_lookup = event_id_for_node(disabled_result.state.runtime, on_check)
				state1 = { ..disabled_result.state, runtime: event_lookup.runtime }
				with_state = { ..created.render_state, state: state1 }
				with_role = add_command(with_state, SetRole({ id: created.id, value: "checkbox" }))
				with_label = add_command(with_role, SetLabel({ id: created.id, value: label }))
				with_checked = add_command(with_label, SetChecked({ id: created.id, value: node_bool(checked_result.value) }))
				with_disabled = add_command(with_checked, SetDisabled({ id: created.id, value: node_bool(disabled_result.value) }))
				add_command(with_disabled, BindCheck({ id: created.id, event: event_lookup.id }))
			}

			Dynamic({ signal, render }) => {
				created = create_child(render_state, parent_id, "div")
				result = eval_signal(created.render_state.state, signal)
				fn = Box.unbox(render)
				child = fn(result.value)
				registered_child = register_elem(result.state.runtime, child)
				metrics = registered_child.runtime.metrics
				state1 = { ..result.state, runtime: { ..registered_child.runtime, metrics: { ..metrics, dynamic_renders: metrics.dynamic_renders + 1 }
					}
				}
				render_child = { ..created.render_state, state: state1 }
				render_elem(render_child, registered_child.elem, created.id)
			}

			DynamicKeyed({ signal, key, render }) => {
				created = create_child(render_state, parent_id, "div")
				result = eval_signal(created.render_state.state, signal)
				key_fn = Box.unbox(key)
				_ = key_fn(result.value)
				fn = Box.unbox(render)
				child = fn(result.value)
				registered_child = register_elem(result.state.runtime, child)
				metrics = registered_child.runtime.metrics
				state1 = { ..result.state, runtime: { ..registered_child.runtime, metrics: { ..metrics, dynamic_renders: metrics.dynamic_renders + 1, keyed_creates: metrics.keyed_creates + 1 }
					}
				}
				render_child = { ..created.render_state, state: state1 }
				render_elem(render_child, registered_child.elem, created.id)
			}

			Each({ signal, key, render }) => {
				created = create_child(render_state, parent_id, "div")
				result = eval_signal(created.render_state.state, signal)
				items = NodeValue.to_list(result.value)
				key_fn = Box.unbox(key)
				render_fn = Box.unbox(render)
				metrics = result.state.runtime.metrics
				state1 = { ..result.state, runtime: { ..result.state.runtime, metrics: { ..metrics, dynamic_renders: metrics.dynamic_renders + 1, keyed_creates: metrics.keyed_creates + List.len(items) }
					}
				}
				start = { ..created.render_state, state: state1 }
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
		metrics0 = runtime.metrics
		runtime_with_count = { ..runtime, metrics: { ..metrics0, event_lookups: metrics0.event_lookups + 1 } }
		match event.expr {
			Graph.EventExpr.Source(_) =>
				match List.get(event.source_ids, 0) {
					Ok(id) => { runtime: runtime_with_count, id }
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
				state1 = { ..source_result.state, runtime: { ..source_result.state.runtime, metrics: { ..metrics, callbacks: metrics.callbacks + List.len(source_result.values) }
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
			Graph.SignalCacheKey.SignalCacheKey(key) =>
				if signal_is_dirty(signal, state.dirty_signal_ids) {
					metrics0 = state.runtime.metrics
					state1 = { ..state, runtime: { ..state.runtime, metrics: { ..metrics0, signal_cache_misses: metrics0.signal_cache_misses + 1 } } }
					eval_signal_uncached(state1, signal)
				} else {
					match signal_cache_lookup(state.runtime, signal.dep_indexes, key) {
						CacheHit(value) => {
							metrics0 = state.runtime.metrics
							metrics1 = { ..metrics0,
								signal_cache_hits: metrics0.signal_cache_hits + 1,
								clean_signal_skips: metrics0.clean_signal_skips + 1,
							}
							{ state: { ..state, runtime: { ..state.runtime, metrics: metrics1 } }, value }
						}

						CacheMiss => {
							metrics0 = state.runtime.metrics
							state1 = { ..state, runtime: { ..state.runtime, metrics: { ..metrics0, signal_cache_misses: metrics0.signal_cache_misses + 1 } } }
							eval_signal_uncached(state1, signal)
						}

						StaleCacheMiss => {
							metrics0 = state.runtime.metrics
							metrics1 = { ..metrics0,
								signal_cache_misses: metrics0.signal_cache_misses + 1,
								stale_signal_cache_misses: metrics0.stale_signal_cache_misses + 1,
							}
							state1 = { ..state, runtime: { ..state.runtime, metrics: metrics1 } }
							eval_signal_uncached(state1, signal)
						}
					}
				}
		}
	}

	eval_signal_uncached : EvalState, Graph.SignalNode -> EvalResult
	eval_signal_uncached = |state, signal| {
		metrics0 = state.runtime.metrics
		state_with_count = { ..state, runtime: { ..state.runtime, metrics: { ..metrics0, evaluated_nodes: metrics0.evaluated_nodes + 1, graph_nodes: metrics0.graph_nodes + 1 }
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
					state1 = { ..source_result.state, runtime: { ..source_result.state.runtime, metrics: { ..metrics, callbacks: metrics.callbacks + 1 }
						}
					}
					{ state: state1, value: fn(source_result.value) }
				}

				Graph.SignalExpr.MapI64I64({ source, transform }) => {
					source_result = eval_signal(state_with_count, Box.unbox(source))
					fn = Box.unbox(transform)
					metrics = source_result.state.runtime.metrics
					state1 = { ..source_result.state, runtime: { ..source_result.state.runtime, metrics: { ..metrics, callbacks: metrics.callbacks + 1 }
						}
					}
					{ state: state1, value: NodeValue.from_i64(fn(NodeValue.to_i64(source_result.value))) }
				}

				Graph.SignalExpr.MapI64Str({ source, transform }) => {
					source_result = eval_signal(state_with_count, Box.unbox(source))
					fn = Box.unbox(transform)
					metrics = source_result.state.runtime.metrics
					state1 = { ..source_result.state, runtime: { ..source_result.state.runtime, metrics: { ..metrics, callbacks: metrics.callbacks + 1 }
						}
					}
					{ state: state1, value: NodeValue.from_str(fn(NodeValue.to_i64(source_result.value))) }
				}

				Graph.SignalExpr.Map2Signal({ left, right, transform }) => {
					left_result = eval_signal(state_with_count, Box.unbox(left))
					right_result = eval_signal(left_result.state, Box.unbox(right))
					fn = Box.unbox(transform)
					metrics = right_result.state.runtime.metrics
					state1 = { ..right_result.state, runtime: { ..right_result.state.runtime, metrics: { ..metrics, callbacks: metrics.callbacks + 1 }
						}
					}
					{ state: state1, value: fn((left_result.value, right_result.value)) }
				}

				Graph.SignalExpr.Map2I64I64({ left, right, transform }) => {
					left_result = eval_signal(state_with_count, Box.unbox(left))
					right_result = eval_signal(left_result.state, Box.unbox(right))
					fn = Box.unbox(transform)
					metrics = right_result.state.runtime.metrics
					state1 = { ..right_result.state, runtime: { ..right_result.state.runtime, metrics: { ..metrics, callbacks: metrics.callbacks + 1 }
						}
					}
					{ state: state1, value: NodeValue.from_i64(fn((NodeValue.to_i64(left_result.value), NodeValue.to_i64(right_result.value)))) }
				}

				Graph.SignalExpr.Map2I64I64Str({ left, right, transform }) => {
					left_result = eval_signal(state_with_count, Box.unbox(left))
					right_result = eval_signal(left_result.state, Box.unbox(right))
					fn = Box.unbox(transform)
					metrics = right_result.state.runtime.metrics
					state1 = { ..right_result.state, runtime: { ..right_result.state.runtime, metrics: { ..metrics, callbacks: metrics.callbacks + 1 }
						}
					}
					{ state: state1, value: NodeValue.from_str(fn((NodeValue.to_i64(left_result.value), NodeValue.to_i64(right_result.value)))) }
				}

				Graph.SignalExpr.Hold({ event }) => {
					event_node = Box.unbox(event)
					state_index = state_index_for_signal(signal)
					current = state_value_by_index(state_with_count.runtime, state_index)
					state1 = { ..state_with_count, runtime: current.runtime }
					if u64_list_contains(state1.updated_state_indexes, current.index) {
						{ state: state1, value: current.value }
					} else {
						event_result = eval_event(state1, event_node)
						next_value = List.fold(event_result.values, current.value, |_acc, value| value)
						finish_state_update(event_result.state, current.index, current.value, next_value, !List.is_empty(event_result.values))
					}
				}

				Graph.SignalExpr.Fold({ event, step }) => {
					event_node = Box.unbox(event)
					state_index = state_index_for_signal(signal)
					current = state_value_by_index(state_with_count.runtime, state_index)
					state1 = { ..state_with_count, runtime: current.runtime }
					if u64_list_contains(state1.updated_state_indexes, current.index) {
						{ state: state1, value: current.value }
					} else {
						event_result = eval_event(state1, event_node)
						fn = Box.unbox(step)
						metrics = event_result.state.runtime.metrics
						state2 = { ..event_result.state, runtime: { ..event_result.state.runtime, metrics: { ..metrics, callbacks: metrics.callbacks + List.len(event_result.values) }
							}
						}
						next_value = List.fold(event_result.values, current.value, |acc, evt| fn((acc, evt)))
						finish_state_update(state2, current.index, current.value, next_value, !List.is_empty(event_result.values))
					}
				}

				Graph.SignalExpr.FoldI64({ event, step }) => {
					event_node = Box.unbox(event)
					state_index = state_index_for_signal(signal)
					current = state_value_by_index(state_with_count.runtime, state_index)
					state1 = { ..state_with_count, runtime: current.runtime }
					if u64_list_contains(state1.updated_state_indexes, current.index) {
						{ state: state1, value: current.value }
					} else {
						event_result = eval_event(state1, event_node)
						fn = Box.unbox(step)
						metrics = event_result.state.runtime.metrics
						state2 = { ..event_result.state, runtime: { ..event_result.state.runtime, metrics: { ..metrics, callbacks: metrics.callbacks + List.len(event_result.values) }
							}
						}
						next_i64 = List.fold(event_result.values, NodeValue.to_i64(current.value), |acc, evt| fn((acc, NodeValue.to_i64(evt))))
						finish_state_update(state2, current.index, current.value, NodeValue.from_i64(next_i64), !List.is_empty(event_result.values))
					}
				}

				Graph.SignalExpr.FoldBoolToggle({ event }) => {
					event_node = Box.unbox(event)
					state_index = state_index_for_signal(signal)
					current = state_value_by_index(state_with_count.runtime, state_index)
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
		cache_signal_result(signal.cache_key, signal.dep_indexes, result)
	}

	cache_signal_result : Graph.SignalCacheKey, List(U64), EvalResult -> EvalResult
	cache_signal_result = |cache_key, deps, result| {
		match cache_key {
			Graph.SignalCacheKey.NoSignalCacheKey => result
			Graph.SignalCacheKey.SignalCacheKey(key) => {
				runtime1 = set_signal_cache(result.state.runtime, key, deps, result.value)
				{ state: { ..result.state, runtime: runtime1 }, value: result.value }
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

	signal_registry_lookup : List(SignalRegistryEntry), Str, U64, List(U64) -> SignalRegistryLookup
	signal_registry_lookup = |entries, key, kind, state_deps| {
		var $index = 0
		var $result = SignalRegistryMissing
		var $done = False

		while $done == False and $index < List.len(entries) {
			match List.get(entries, $index) {
				Ok(entry) =>
					if entry.key == key and entry.kind == kind and u64_list_equal(entry.state_deps, state_deps) {
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

	signal_cache_entry_lookup : List(SignalCacheEntry), Str -> SignalCacheEntryLookup
	signal_cache_entry_lookup = |entries, key| {
		var $index = 0
		var $result = SignalCacheEntryMissing
		var $done = False

		while $done == False and $index < List.len(entries) {
			match List.get(entries, $index) {
				Ok(entry) =>
					if entry.key == key {
						$result = SignalCacheEntryFound({ index: $index, entry })
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

	signal_cache_lookup : Runtime, List(U64), Str -> CacheLookup
	signal_cache_lookup = |runtime, signal_deps, key| {
		match signal_cache_entry_lookup(runtime.signal_cache, key) {
			SignalCacheEntryFound(found) =>
				if signal_cache_entry_valid(runtime, signal_deps, found.entry.deps) {
					CacheHit(found.entry.value)
				} else {
					StaleCacheMiss
				}
			SignalCacheEntryMissing => CacheMiss
		}
	}

	signal_cache_entry_valid : Runtime, List(U64), List(StateVersionEntry) -> Bool
	signal_cache_entry_valid = |runtime, signal_deps, cached_deps| {
		current_deps_present = List.all(
			signal_deps,
			|state_index|
				match state_version_lookup(cached_deps, state_index) {
					VersionFound(version) => current_state_version_by_index(runtime, state_index) == version
					VersionMissing => False
				},
		)
		no_stale_cached_deps = List.all(
			cached_deps,
			|dep| u64_list_contains(signal_deps, dep.state_index) and current_state_version_by_index(runtime, dep.state_index) == dep.version,
		)
		current_deps_present and no_stale_cached_deps
	}

	state_versions_for_deps : Runtime, List(U64) -> List(StateVersionEntry)
	state_versions_for_deps = |runtime, deps| {
		List.map(
			deps,
			|state_index| {
				{ state_index, version: current_state_version_by_index(runtime, state_index) }
			},
		)
	}

	state_descriptors_for_runtime : Runtime -> List(StateDesc)
	state_descriptors_for_runtime = |runtime| {
		var $state_id = 0
		var $descriptors = List.with_capacity(List.len(runtime.states))

		for slot in runtime.states {
			$descriptors = List.append($descriptors, { state_id: $state_id, value: slot.value, event_ids: slot.event_ids })
			$state_id = $state_id + 1
		}

		$descriptors
	}

	state_changes_for_indexes : Runtime, List(U64) -> List(StateChangeDesc)
	state_changes_for_indexes = |runtime, state_indexes| {
		List.map(
			state_indexes,
			|state_id| {
				match List.get(runtime.states, state_id) {
					Ok(slot) => { state_id, value: slot.value }
					Err(_) => {
						crash "Signals runtime invariant violated: changed state index was not registered"
					}
				}
			},
		)
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
				{ signal_id: entry.signal_id, kind: entry.kind, state_deps: entry.state_deps }
			},
		)
	}

	current_state_version_by_index : Runtime, U64 -> U64
	current_state_version_by_index = |runtime, state_index| {
		match List.get(runtime.states, state_index) {
			Ok(slot) => slot.version
			Err(_) => {
				crash "Signals runtime invariant violated: state version index was not registered"
			}
		}
	}

	state_version_lookup : List(StateVersionEntry), U64 -> VersionLookup
	state_version_lookup = |entries, state_index| {
		var $index = 0
		var $result = VersionMissing
		var $done = False

		while $done == False and $index < List.len(entries) {
			match List.get(entries, $index) {
				Ok(entry) =>
					if entry.state_index == state_index {
						$result = VersionFound(entry.version)
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

	replace_signal_cache_entry : List(SignalCacheEntry), U64, SignalCacheEntry -> List(SignalCacheEntry)
	replace_signal_cache_entry = |entries, target_index, replacement| {
		var $index = 0
		var $next = List.with_capacity(List.len(entries))

		for entry in entries {
			if $index == target_index {
				$next = List.append($next, replacement)
			} else {
				$next = List.append($next, entry)
			}
			$index = $index + 1
		}

		$next
	}

	replace_state_slot : List(StateSlot), U64, StateSlot -> List(StateSlot)
	replace_state_slot = |entries, target_index, replacement| {
		var $index = 0
		var $next = List.with_capacity(List.len(entries))

		for entry in entries {
			if $index == target_index {
				$next = List.append($next, replacement)
			} else {
				$next = List.append($next, entry)
			}
			$index = $index + 1
		}

		$next
	}

	set_signal_cache : Runtime, Str, List(U64), NodeValue -> Runtime
	set_signal_cache = |runtime, key, signal_deps, value| {
		deps = state_versions_for_deps(runtime, signal_deps)
		entry = { key, value, deps }
		match signal_cache_entry_lookup(runtime.signal_cache, key) {
			SignalCacheEntryFound(found) => { ..runtime, signal_cache: replace_signal_cache_entry(runtime.signal_cache, found.index, entry) }

			SignalCacheEntryMissing => { ..runtime, signal_cache: List.append(runtime.signal_cache, entry) }
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

	state_value_by_index : Runtime, U64 -> StateValue
	state_value_by_index = |runtime, state_index| {
		metrics0 = runtime.metrics
		runtime_with_count = { ..runtime, metrics: { ..metrics0, state_lookups: metrics0.state_lookups + 1 } }
		match List.get(runtime_with_count.states, state_index) {
			Ok(slot) => { runtime: runtime_with_count, value: slot.value, index: state_index }
			Err(_) => {
				crash "Signals runtime invariant violated: state index was not registered"
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
					{ ..metrics0, signal_writes: metrics0.signal_writes + 1,
						signal_changes: metrics0.signal_changes + 1,
						node_value_equality_checks: metrics0.node_value_equality_checks + 1,
					}
				} else {
					{ ..metrics0, signal_writes: metrics0.signal_writes + 1,
						signal_suppressed: metrics0.signal_suppressed + 1,
						node_value_equality_checks: metrics0.node_value_equality_checks + 1,
					}
				}
			runtime2 = set_state_by_index({ ..state.runtime, metrics: metrics1 }, state_index, next, changed)
			next_changed_state_indexes =
				if changed {
					List.append(state.changed_state_indexes, state_index)
				} else {
					state.changed_state_indexes
				}
			state1 = {
				..state,
				runtime: runtime2,
				updated_state_indexes: List.append(state.updated_state_indexes, state_index),
				changed_state_indexes: next_changed_state_indexes,
			}
			{ state: state1, value: next }
		} else {
			{ state, value: previous }
		}
	}

	set_state_by_index : Runtime, U64, NodeValue, Bool -> Runtime
	set_state_by_index = |runtime, state_index, value, bump_version| {
		match List.get(runtime.states, state_index) {
			Ok(slot) => {
				next_version =
					if bump_version {
						slot.version + 1
					} else {
						slot.version
					}
				next_slot = { key: slot.key, value, version: next_version, event_ids: slot.event_ids }
				{ ..runtime, states: replace_state_slot(runtime.states, state_index, next_slot) }
			}
			Err(_) => {
				crash "Signals runtime invariant violated: state update index was not registered"
			}
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
