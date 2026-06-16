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
	}

	EventId : { key : Str, id : U64 }

	StateEntry : { key : Str, value : NodeValue }

	Runtime : {
		states : List(StateEntry),
		event_ids : List(EventId),
		next_event_id : U64,
		metrics : RuntimeMetrics,
	}

	HostEvent := [
		Click(U64),
		Input({ event : U64, value : Str }),
		Check({ event : U64, checked : Bool }),
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

	DispatchResult : {
		runtime : Box(Runtime),
		commands : List(Command),
		metrics : RuntimeMetrics,
	}

	ActiveEvent := [
		NoEvent,
		Occurrence({ id : U64, value : NodeValue }),
	]

	EvalState : {
		runtime : Runtime,
		active_event : ActiveEvent,
		updated_keys : List(Str),
	}

	EvalResult : { state : EvalState, value : NodeValue }

	EventResult : { state : EvalState, values : List(NodeValue) }

	RenderState : {
		state : EvalState,
		commands : List(Command),
		next_elem_id : U64,
	}

	StateLookup : { found : Bool, value : NodeValue }

	EventLookup : { runtime : Runtime, id : U64 }

	UpsertState : { entries : List(StateEntry), found : Bool }

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
	}

	init : Elem -> DispatchResult
	init = |root| {
		runtime = {
			states: [],
			event_ids: [],
			next_event_id: 1,
			metrics: zero_metrics,
		}
		rendered = render_runtime(runtime, root, NoEvent)
		runtime_box = Box.box(rendered.runtime)
		result = {
			runtime: runtime_box,
			commands: rendered.commands,
			metrics: rendered.runtime.metrics,
		}
		result
	}

	dispatch : Box(Runtime), Elem, HostEvent -> DispatchResult
	dispatch = |boxed_runtime, root, host_event| {
		runtime0 = Box.unbox(boxed_runtime)
		active_event = host_event_to_active(host_event)
		metrics = runtime0.metrics
		runtime1 = { ..runtime0, metrics: { ..metrics, events_processed: metrics.events_processed + 1 }
		}
		rendered = render_runtime(runtime1, root, active_event)
		{
			runtime: Box.box(rendered.runtime),
			commands: rendered.commands,
			metrics: rendered.runtime.metrics,
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
			Click(event_id) => Occurrence({ id: event_id, value: NodeValue.unit })
			Input({ event, value }) => Occurrence({ id: event, value: NodeValue.from_str(value) })
			Check({ event, checked }) => Occurrence({ id: event, value: NodeValue.from_bool(checked) })
		}
	}

	render_runtime : Runtime, Elem, ActiveEvent -> { runtime : Runtime, commands : List(Command) }
	render_runtime = |runtime, root, active_event| {
		state = { runtime, active_event, updated_keys: [] }
		render_state = {
			state,
			commands: [ResetDom],
			next_elem_id: 1,
		}
		rendered = render_elem(render_state, root, 0)
		{
			runtime: rendered.state.runtime,
			commands: rendered.commands,
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
				metrics = result.state.runtime.metrics
				state1 = { ..result.state, runtime: { ..result.state.runtime, metrics: { ..metrics, dynamic_renders: metrics.dynamic_renders + 1 }
					}
				}
				render_child = { ..created.render_state, state: state1 }
				render_elem(render_child, child, created.id)
			}

			DynamicKeyed({ signal, key, render }) => {
				created = create_child(render_state, parent_id, "div")
				result = eval_signal(created.render_state.state, signal)
				key_fn = Box.unbox(key)
				_ = key_fn(result.value)
				fn = Box.unbox(render)
				child = fn(result.value)
				metrics = result.state.runtime.metrics
				state1 = { ..result.state, runtime: { ..result.state.runtime, metrics: { ..metrics, dynamic_renders: metrics.dynamic_renders + 1, keyed_creates: metrics.keyed_creates + 1 }
					}
				}
				render_child = { ..created.render_state, state: state1 }
				render_elem(render_child, child, created.id)
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
						render_elem(acc, render_fn(item), created.id)
					},
				)
			}
		}
	}

	event_id_for_node : Runtime, Graph.EventNode -> EventLookup
	event_id_for_node = |runtime, event| {
		match event {
			Graph.EventNode.Source(key) => event_id_for_key(runtime, key)
			_ => ...
		}
	}

	event_id_for_key : Runtime, Str -> EventLookup
	event_id_for_key = |runtime, key| {
		found = List.fold(
			runtime.event_ids,
			{ found: False, id: 0 },
			|acc, entry| if acc.found {
				acc
			} else if entry.key == key {
				{ found: True, id: entry.id }
			} else {
				acc
			},
		)
		if found.found {
			{ runtime, id: found.id }
		} else {
			id = runtime.next_event_id
			entry = { key, id }
			{
				runtime: { ..runtime, event_ids: List.append(runtime.event_ids, entry), next_event_id: id + 1
				},
				id,
			}
		}
	}

	eval_event : EvalState, Graph.EventNode -> EventResult
	eval_event = |state, event| {
		match event {
			Graph.EventNode.Source(key) => {
				lookup = event_id_for_key(state.runtime, key)
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

			Graph.EventNode.MapEvent({ source, transform }) => {
				source_result = eval_event(state, Box.unbox(source))
				fn = Box.unbox(transform)
				metrics = source_result.state.runtime.metrics
				state1 = { ..source_result.state, runtime: { ..source_result.state.runtime, metrics: { ..metrics, callbacks: metrics.callbacks + List.len(source_result.values) }
					}
				}
				values = List.map(source_result.values, |value| fn(value))
				{ state: state1, values }
			}

			Graph.EventNode.MapUnitI64Const({ source, value }) => {
				source_result = eval_event(state, Box.unbox(source))
				values = List.map(source_result.values, |_| NodeValue.from_i64(value))
				{ state: source_result.state, values }
			}

			Graph.EventNode.Merge({ left, right }) => {
				left_result = eval_event(state, Box.unbox(left))
				right_result = eval_event(left_result.state, Box.unbox(right))
				{ state: right_result.state, values: List.concat(left_result.values, right_result.values) }
			}
		}
	}

	eval_signal : EvalState, Graph.SignalNode -> EvalResult
	eval_signal = |state, signal| {
		metrics0 = state.runtime.metrics
		state_with_count = { ..state, runtime: { ..state.runtime, metrics: { ..metrics0, evaluated_nodes: metrics0.evaluated_nodes + 1, graph_nodes: metrics0.graph_nodes + 1 }
			}
		}
		match signal {
			Graph.SignalNode.ConstSignal(value) => { state: state_with_count, value }
			Graph.SignalNode.ConstI64(value) => { state: state_with_count, value: NodeValue.from_i64(value) }
			Graph.SignalNode.ConstBool(value) => { state: state_with_count, value: NodeValue.from_bool(value) }
			Graph.SignalNode.ConstStr(value) => { state: state_with_count, value: NodeValue.from_str(value) }

			Graph.SignalNode.MapSignal({ source, transform }) => {
				source_result = eval_signal(state_with_count, Box.unbox(source))
				fn = Box.unbox(transform)
				metrics = source_result.state.runtime.metrics
				state1 = { ..source_result.state, runtime: { ..source_result.state.runtime, metrics: { ..metrics, callbacks: metrics.callbacks + 1 }
					}
				}
				{ state: state1, value: fn(source_result.value) }
			}

			Graph.SignalNode.MapI64I64({ source, transform }) => {
				source_result = eval_signal(state_with_count, Box.unbox(source))
				fn = Box.unbox(transform)
				metrics = source_result.state.runtime.metrics
				state1 = { ..source_result.state, runtime: { ..source_result.state.runtime, metrics: { ..metrics, callbacks: metrics.callbacks + 1 }
					}
				}
				{ state: state1, value: NodeValue.from_i64(fn(NodeValue.to_i64(source_result.value))) }
			}

			Graph.SignalNode.MapI64Str({ source, transform }) => {
				source_result = eval_signal(state_with_count, Box.unbox(source))
				fn = Box.unbox(transform)
				metrics = source_result.state.runtime.metrics
				state1 = { ..source_result.state, runtime: { ..source_result.state.runtime, metrics: { ..metrics, callbacks: metrics.callbacks + 1 }
					}
				}
				{ state: state1, value: NodeValue.from_str(fn(NodeValue.to_i64(source_result.value))) }
			}

			Graph.SignalNode.Map2Signal({ left, right, transform }) => {
				left_result = eval_signal(state_with_count, Box.unbox(left))
				right_result = eval_signal(left_result.state, Box.unbox(right))
				fn = Box.unbox(transform)
				metrics = right_result.state.runtime.metrics
				state1 = { ..right_result.state, runtime: { ..right_result.state.runtime, metrics: { ..metrics, callbacks: metrics.callbacks + 1 }
					}
				}
				{ state: state1, value: fn((left_result.value, right_result.value)) }
			}

			Graph.SignalNode.Map2I64I64({ left, right, transform }) => {
				left_result = eval_signal(state_with_count, Box.unbox(left))
				right_result = eval_signal(left_result.state, Box.unbox(right))
				fn = Box.unbox(transform)
				metrics = right_result.state.runtime.metrics
				state1 = { ..right_result.state, runtime: { ..right_result.state.runtime, metrics: { ..metrics, callbacks: metrics.callbacks + 1 }
					}
				}
				{ state: state1, value: NodeValue.from_i64(fn((NodeValue.to_i64(left_result.value), NodeValue.to_i64(right_result.value)))) }
			}

			Graph.SignalNode.Map2I64I64Str({ left, right, transform }) => {
				left_result = eval_signal(state_with_count, Box.unbox(left))
				right_result = eval_signal(left_result.state, Box.unbox(right))
				fn = Box.unbox(transform)
				metrics = right_result.state.runtime.metrics
				state1 = { ..right_result.state, runtime: { ..right_result.state.runtime, metrics: { ..metrics, callbacks: metrics.callbacks + 1 }
					}
				}
				{ state: state1, value: NodeValue.from_str(fn((NodeValue.to_i64(left_result.value), NodeValue.to_i64(right_result.value)))) }
			}

			Graph.SignalNode.Hold({ key, initial, event }) => {
				current = state_value(state_with_count.runtime, key, initial)
				state1 = { ..state_with_count, runtime: current.runtime }
				if List.contains(state1.updated_keys, key) {
					{ state: state1, value: current.value }
				} else {
					event_result = eval_event(state1, Box.unbox(event))
					next_value = List.fold(event_result.values, current.value, |_acc, value| value)
					finish_state_update(event_result.state, key, current.value, next_value, !List.is_empty(event_result.values))
				}
			}

			Graph.SignalNode.Fold({ key, initial, event, step }) => {
				current = state_value(state_with_count.runtime, key, initial)
				state1 = { ..state_with_count, runtime: current.runtime }
				if List.contains(state1.updated_keys, key) {
					{ state: state1, value: current.value }
				} else {
					event_result = eval_event(state1, Box.unbox(event))
					fn = Box.unbox(step)
					metrics = event_result.state.runtime.metrics
					state2 = { ..event_result.state, runtime: { ..event_result.state.runtime, metrics: { ..metrics, callbacks: metrics.callbacks + List.len(event_result.values) }
						}
					}
					next_value = List.fold(event_result.values, current.value, |acc, evt| fn((acc, evt)))
					finish_state_update(state2, key, current.value, next_value, !List.is_empty(event_result.values))
				}
			}

			Graph.SignalNode.FoldI64({ key, initial, event, step }) => {
				initial_nv = NodeValue.from_i64(initial)
				current = state_value(state_with_count.runtime, key, initial_nv)
				state1 = { ..state_with_count, runtime: current.runtime }
				if List.contains(state1.updated_keys, key) {
					{ state: state1, value: current.value }
				} else {
					event_result = eval_event(state1, Box.unbox(event))
					fn = Box.unbox(step)
					metrics = event_result.state.runtime.metrics
					state2 = { ..event_result.state, runtime: { ..event_result.state.runtime, metrics: { ..metrics, callbacks: metrics.callbacks + List.len(event_result.values) }
						}
					}
					next_i64 = List.fold(event_result.values, NodeValue.to_i64(current.value), |acc, evt| fn((acc, NodeValue.to_i64(evt))))
					finish_state_update(state2, key, current.value, NodeValue.from_i64(next_i64), !List.is_empty(event_result.values))
				}
			}

			Graph.SignalNode.FoldBoolToggle({ key, initial, event }) => {
				initial_nv = NodeValue.from_bool(initial)
				current = state_value(state_with_count.runtime, key, initial_nv)
				state1 = { ..state_with_count, runtime: current.runtime }
				if List.contains(state1.updated_keys, key) {
					{ state: state1, value: current.value }
				} else {
					event_result = eval_event(state1, Box.unbox(event))
					next_bool = List.fold(event_result.values, node_bool(current.value), |acc, _evt| !acc)
					finish_state_update(event_result.state, key, current.value, NodeValue.from_bool(next_bool), !List.is_empty(event_result.values))
				}
			}
		}
	}

	state_value : Runtime, Str, NodeValue -> { runtime : Runtime, value : NodeValue }
	state_value = |runtime, key, initial| {
		lookup = List.fold(
			runtime.states,
			{ found: False, value: initial },
			|acc, entry| if acc.found {
				acc
			} else if entry.key == key {
				{ found: True, value: entry.value }
			} else {
				acc
			},
		)
		if lookup.found {
			{ runtime, value: lookup.value }
		} else {
			{
				runtime: { ..runtime, states: List.append(runtime.states, { key, value: initial }) },
				value: initial,
			}
		}
	}

	finish_state_update : EvalState, Str, NodeValue, NodeValue, Bool -> EvalResult
	finish_state_update = |state, key, previous, next, had_event| {
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
			runtime1 = set_state({ ..state.runtime, metrics: metrics1 }, key, next)
			state1 = { ..state, runtime: runtime1, updated_keys: List.append(state.updated_keys, key) }
			{ state: state1, value: next }
		} else {
			{ state, value: previous }
		}
	}

	set_state : Runtime, Str, NodeValue -> Runtime
	set_state = |runtime, key, value| {
		upsert = List.fold(
			runtime.states,
			{ entries: [], found: False },
			|acc, entry| if entry.key == key {
				{ entries: List.append(acc.entries, { key, value }), found: True }
			} else {
				{ ..acc, entries: List.append(acc.entries, entry) }
			},
		)
		if upsert.found {
			{ ..runtime, states: upsert.entries }
		} else {
			{ ..runtime, states: List.append(upsert.entries, { key, value }) }
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
			_ => ...
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
			pairs = List.map2(left, right, |a, b| (a, b))
			!List.any(pairs, |(a, b)| !node_value_equal(a, b))
		}
	}
}
