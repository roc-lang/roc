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

	EventId : { key : Str, id : U64 }

	StateEntry : { key : Str, value : NodeValue }

	StateVersionEntry : { key : Str, version : U64 }

	SignalCacheEntry : { key : Str, value : NodeValue, deps : List(StateVersionEntry) }

	EventStateDep : { event_key : Str, state_key : Str }

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

	CommandSnapshot : {
		kind : U64,
		a : U64,
		b : U64,
		value_bool : Bool,
		value_bytes : List(U8),
	}

	Runtime : {
		root : Elem,
		states : List(StateEntry),
		event_ids : List(EventId),
		next_event_id : U64,
		previous_commands : List(CommandSnapshot),
		signal_cache : List(SignalCacheEntry),
		state_versions : List(StateVersionEntry),
		event_state_deps : List(EventStateDep),
		metrics : RuntimeMetrics,
	}

	DispatchResult : {
		runtime : Box(Runtime),
		commands : List(Command),
		metrics : RuntimeMetrics,
	}

	ActiveEvent := [
		NoEvent,
		Occurrence({ id : U64, value : NodeValue }),
	]

	ActiveEventKey := [
		NoActiveEventKey,
		ActiveEventKey(Str),
	]

	EvalState : {
		runtime : Runtime,
		active_event : ActiveEvent,
		dirty_state_keys : List(Str),
		updated_keys : List(Str),
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
	}

	StateLookup : { found : Bool, value : NodeValue }

	EventLookup : { runtime : Runtime, id : U64 }

	UpsertState : { entries : List(StateEntry), found : Bool }

	UpsertStateVersion : { entries : List(StateVersionEntry), found : Bool }

	UpsertSignalCache : { entries : List(SignalCacheEntry), found : Bool }

	CacheLookup := [
		CacheHit(NodeValue),
		CacheMiss,
		StaleCacheMiss,
	]

	VersionLookup := [
		VersionFound(U64),
		VersionMissing,
	]

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
		runtime = {
			root,
			states: [],
			event_ids: [],
			next_event_id: 1,
			previous_commands: [],
			signal_cache: [],
			state_versions: [],
			event_state_deps: [],
			metrics: zero_metrics,
		}
		rendered = render_runtime(runtime, NoEvent, [], True)
		runtime_box = Box.box(rendered.runtime)
		result = {
			runtime: runtime_box,
			commands: rendered.emit_commands,
			metrics: rendered.runtime.metrics,
		}
		result
	}

	dispatch : Box(Runtime), HostEvent -> DispatchResult
	dispatch = |boxed_runtime, host_event| {
		runtime0 = Box.unbox(boxed_runtime)
		active_event = host_event_to_active(host_event)
		active_event_key = active_event_key_for_runtime(runtime0, active_event)
		dirty_state_keys = dirty_state_keys_for_active_event(runtime0.event_state_deps, active_event_key)
		metrics = runtime0.metrics
		runtime1 = { ..runtime0, metrics: { ..metrics,
				events_processed: metrics.events_processed + 1,
				retained_graph_dispatches: metrics.retained_graph_dispatches + 1,
			}
		}
		rendered = render_runtime(runtime1, active_event, dirty_state_keys, False)
		{
			runtime: Box.box(rendered.runtime),
			commands: rendered.emit_commands,
			metrics: rendered.runtime.metrics,
		}
	}

	active_event_key_for_runtime : Runtime, ActiveEvent -> ActiveEventKey
	active_event_key_for_runtime = |runtime, active_event| {
		match active_event {
			NoEvent => NoActiveEventKey
			Occurrence({ id }) => event_key_for_id(runtime.event_ids, id)
		}
	}

	event_key_for_id : List(EventId), U64 -> ActiveEventKey
	event_key_for_id = |event_ids, id| {
		List.fold(
			event_ids,
			NoActiveEventKey,
			|acc, entry| match acc {
				ActiveEventKey(_) => acc
				NoActiveEventKey =>
					if entry.id == id {
						ActiveEventKey(entry.key)
					} else {
						NoActiveEventKey
					}
			},
		)
	}

	dirty_state_keys_for_active_event : List(EventStateDep), ActiveEventKey -> List(Str)
	dirty_state_keys_for_active_event = |deps, active_event_key| {
		match active_event_key {
			NoActiveEventKey => []
			ActiveEventKey(event_key) =>
				List.fold(
					deps,
					[],
					|acc, dep| if dep.event_key == event_key {
						append_unique_str(acc, dep.state_key)
					} else {
						acc
					},
				)
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

	render_runtime : Runtime, ActiveEvent, List(Str), Bool -> RenderResult
	render_runtime = |runtime, active_event, dirty_state_keys, force_full| {
		state = { runtime, active_event, dirty_state_keys, updated_keys: [] }
		render_state = {
			state,
			commands: [ResetDom],
			next_elem_id: 1,
		}
		rendered = render_elem(render_state, runtime.root, 0)
		full_commands = rendered.commands
		previous_snapshots = rendered.state.runtime.previous_commands
		next_snapshots = command_snapshots(full_commands)
		full_batch = force_full or !(same_structure(previous_snapshots, next_snapshots))
		emit_commands =
			if full_batch {
				full_commands
			} else {
				diff_non_structural(previous_snapshots, full_commands)
			}
		metrics0 = rendered.state.runtime.metrics
		metrics1 =
			if full_batch {
				{ ..metrics0,
					commands_emitted: metrics0.commands_emitted + List.len(emit_commands),
					full_render_batches: metrics0.full_render_batches + 1,
					structural_resets: metrics0.structural_resets + 1,
				}
			} else {
				{ ..metrics0,
					commands_emitted: metrics0.commands_emitted + List.len(emit_commands),
					incremental_batches: metrics0.incremental_batches + 1,
				}
		}
		{
			runtime: { ..rendered.state.runtime, previous_commands: next_snapshots, metrics: metrics1 },
			emit_commands,
		}
	}

	command_snapshots : List(Command) -> List(CommandSnapshot)
	command_snapshots = |commands| {
		List.map(commands, command_snapshot)
	}

	command_snapshot : Command -> CommandSnapshot
	command_snapshot = |command| {
		match command {
			ResetDom => { kind: 0, a: 0, b: 0, value_bool: False, value_bytes: [] }
			CreateElement(payload) => { kind: 1, a: payload.id, b: 0, value_bool: False, value_bytes: Str.to_utf8(payload.tag) }
			AppendChild(payload) => { kind: 2, a: payload.parent, b: payload.child, value_bool: False, value_bytes: [] }
			SetText(payload) => { kind: 3, a: payload.id, b: 0, value_bool: False, value_bytes: Str.to_utf8(payload.value) }
			SetRole(payload) => { kind: 4, a: payload.id, b: 0, value_bool: False, value_bytes: Str.to_utf8(payload.value) }
			SetLabel(payload) => { kind: 5, a: payload.id, b: 0, value_bool: False, value_bytes: Str.to_utf8(payload.value) }
			SetTestId(payload) => { kind: 6, a: payload.id, b: 0, value_bool: False, value_bytes: Str.to_utf8(payload.value) }
			SetValue(payload) => { kind: 7, a: payload.id, b: 0, value_bool: False, value_bytes: Str.to_utf8(payload.value) }
			SetChecked(payload) => { kind: 8, a: payload.id, b: 0, value_bool: payload.value, value_bytes: [] }
			SetDisabled(payload) => { kind: 9, a: payload.id, b: 0, value_bool: payload.value, value_bytes: [] }
			BindClick(payload) => { kind: 10, a: payload.id, b: payload.event, value_bool: False, value_bytes: [] }
			BindInput(payload) => { kind: 11, a: payload.id, b: payload.event, value_bool: False, value_bytes: [] }
			BindCheck(payload) => { kind: 12, a: payload.id, b: payload.event, value_bool: False, value_bytes: [] }
		}
	}

	same_structure : List(CommandSnapshot), List(CommandSnapshot) -> Bool
	same_structure = |previous, next| {
		previous_structure = structural_snapshots(previous)
		next_structure = structural_snapshots(next)
		if List.len(previous_structure) != List.len(next_structure) {
			False
		} else {
			pairs = List.map2(previous_structure, next_structure, |a, b| (a, b))
			!List.any(pairs, |(a, b)| !snapshot_equal(a, b))
		}
	}

	structural_snapshots : List(CommandSnapshot) -> List(CommandSnapshot)
	structural_snapshots = |snapshots| {
		List.keep_if(snapshots, snapshot_is_structural)
	}

	snapshot_is_structural : CommandSnapshot -> Bool
	snapshot_is_structural = |snapshot| {
		snapshot.kind == 0 or snapshot.kind == 1 or snapshot.kind == 2
	}

	command_is_structural : Command -> Bool
	command_is_structural = |command| {
		snapshot_is_structural(command_snapshot(command))
	}

	diff_non_structural : List(CommandSnapshot), List(Command) -> List(Command)
	diff_non_structural = |previous, next| {
		List.keep_if(next, |command| if command_is_structural(command) {
			False
		} else {
			snapshot = command_snapshot(command)
			!List.any(previous, |old| snapshot_equal(old, snapshot))
		})
	}

	snapshot_equal : CommandSnapshot, CommandSnapshot -> Bool
	snapshot_equal = |left, right| {
		left.kind == right.kind
			and left.a == right.a
			and left.b == right.b
			and left.value_bool == right.value_bool
			and u8_list_equal(left.value_bytes, right.value_bytes)
	}

	u8_list_equal : List(U8), List(U8) -> Bool
	u8_list_equal = |left, right| {
		if List.len(left) != List.len(right) {
			False
		} else {
			pairs = List.map2(left, right, |a, b| (a, b))
			!List.any(pairs, |(a, b)| a != b)
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
		match event.expr {
			Graph.EventExpr.Source(key) => event_id_for_key(runtime, key)
			_ => ...
		}
	}

	event_id_for_key : Runtime, Str -> EventLookup
	event_id_for_key = |runtime, key| {
		metrics0 = runtime.metrics
		runtime_with_count = { ..runtime, metrics: { ..metrics0, event_lookups: metrics0.event_lookups + 1 } }
		found = List.fold(
			runtime_with_count.event_ids,
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
			{ runtime: runtime_with_count, id: found.id }
		} else {
			id = runtime_with_count.next_event_id
			entry = { key, id }
			{
				runtime: { ..runtime_with_count, event_ids: List.append(runtime_with_count.event_ids, entry), next_event_id: id + 1
				},
				id,
			}
		}
	}

	eval_event : EvalState, Graph.EventNode -> EventResult
	eval_event = |state, event| {
		match event.expr {
			Graph.EventExpr.Source(key) => {
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
				if signal_depends_on_dirty_state(signal, state.dirty_state_keys) {
					metrics0 = state.runtime.metrics
					state1 = { ..state, runtime: { ..state.runtime, metrics: { ..metrics0, signal_cache_misses: metrics0.signal_cache_misses + 1 } } }
					eval_signal_uncached(state1, signal)
				} else {
					match signal_cache_lookup(state.runtime, signal.deps, key) {
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

				Graph.SignalExpr.Hold({ key, initial, event }) => {
					event_node = Box.unbox(event)
					runtime_with_deps = register_state_event_deps(state_with_count.runtime, key, event_node.sources)
					current = state_value(runtime_with_deps, key, initial)
					state1 = { ..state_with_count, runtime: current.runtime }
					if List.contains(state1.updated_keys, key) {
						{ state: state1, value: current.value }
					} else {
						event_result = eval_event(state1, event_node)
						next_value = List.fold(event_result.values, current.value, |_acc, value| value)
						finish_state_update(event_result.state, key, current.value, next_value, !List.is_empty(event_result.values))
					}
				}

				Graph.SignalExpr.Fold({ key, initial, event, step }) => {
					event_node = Box.unbox(event)
					runtime_with_deps = register_state_event_deps(state_with_count.runtime, key, event_node.sources)
					current = state_value(runtime_with_deps, key, initial)
					state1 = { ..state_with_count, runtime: current.runtime }
					if List.contains(state1.updated_keys, key) {
						{ state: state1, value: current.value }
					} else {
						event_result = eval_event(state1, event_node)
						fn = Box.unbox(step)
						metrics = event_result.state.runtime.metrics
						state2 = { ..event_result.state, runtime: { ..event_result.state.runtime, metrics: { ..metrics, callbacks: metrics.callbacks + List.len(event_result.values) }
							}
						}
						next_value = List.fold(event_result.values, current.value, |acc, evt| fn((acc, evt)))
						finish_state_update(state2, key, current.value, next_value, !List.is_empty(event_result.values))
					}
				}

				Graph.SignalExpr.FoldI64({ key, initial, event, step }) => {
					event_node = Box.unbox(event)
					runtime_with_deps = register_state_event_deps(state_with_count.runtime, key, event_node.sources)
					initial_nv = NodeValue.from_i64(initial)
					current = state_value(runtime_with_deps, key, initial_nv)
					state1 = { ..state_with_count, runtime: current.runtime }
					if List.contains(state1.updated_keys, key) {
						{ state: state1, value: current.value }
					} else {
						event_result = eval_event(state1, event_node)
						fn = Box.unbox(step)
						metrics = event_result.state.runtime.metrics
						state2 = { ..event_result.state, runtime: { ..event_result.state.runtime, metrics: { ..metrics, callbacks: metrics.callbacks + List.len(event_result.values) }
							}
						}
						next_i64 = List.fold(event_result.values, NodeValue.to_i64(current.value), |acc, evt| fn((acc, NodeValue.to_i64(evt))))
						finish_state_update(state2, key, current.value, NodeValue.from_i64(next_i64), !List.is_empty(event_result.values))
					}
				}

				Graph.SignalExpr.FoldBoolToggle({ key, initial, event }) => {
					event_node = Box.unbox(event)
					runtime_with_deps = register_state_event_deps(state_with_count.runtime, key, event_node.sources)
					initial_nv = NodeValue.from_bool(initial)
					current = state_value(runtime_with_deps, key, initial_nv)
					state1 = { ..state_with_count, runtime: current.runtime }
					if List.contains(state1.updated_keys, key) {
						{ state: state1, value: current.value }
					} else {
						event_result = eval_event(state1, event_node)
						next_bool = List.fold(event_result.values, node_bool(current.value), |acc, _evt| !acc)
						finish_state_update(event_result.state, key, current.value, NodeValue.from_bool(next_bool), !List.is_empty(event_result.values))
					}
				}
			}
		cache_signal_result(signal.cache_key, signal.deps, result)
	}

	cache_signal_result : Graph.SignalCacheKey, List(Str), EvalResult -> EvalResult
	cache_signal_result = |cache_key, deps, result| {
		match cache_key {
			Graph.SignalCacheKey.NoSignalCacheKey => result
			Graph.SignalCacheKey.SignalCacheKey(key) => {
				runtime1 = set_signal_cache(result.state.runtime, key, deps, result.value)
				{ state: { ..result.state, runtime: runtime1 }, value: result.value }
			}
		}
	}

	signal_depends_on_dirty_state : Graph.SignalNode, List(Str) -> Bool
	signal_depends_on_dirty_state = |signal, dirty_state_keys| {
		List.any(signal.deps, |dep| str_list_contains(dirty_state_keys, dep))
	}

	signal_cache_lookup : Runtime, List(Str), Str -> CacheLookup
	signal_cache_lookup = |runtime, signal_deps, key| {
		List.fold(
			runtime.signal_cache,
			CacheMiss,
			|acc, entry| match acc {
				CacheHit(_) => acc
				StaleCacheMiss => acc
				CacheMiss =>
					if entry.key == key {
						if signal_cache_entry_valid(runtime, signal_deps, entry.deps) {
							CacheHit(entry.value)
						} else {
							StaleCacheMiss
						}
					} else {
						CacheMiss
					}
			},
		)
	}

	signal_cache_entry_valid : Runtime, List(Str), List(StateVersionEntry) -> Bool
	signal_cache_entry_valid = |runtime, signal_deps, cached_deps| {
		current_deps_present = List.all(
			signal_deps,
			|dep|
				match state_version_lookup(cached_deps, dep) {
					VersionFound(version) => current_state_version(runtime, dep) == version
					VersionMissing => False
				},
		)
		no_stale_cached_deps = List.all(
			cached_deps,
			|dep| str_list_contains(signal_deps, dep.key) and current_state_version(runtime, dep.key) == dep.version,
		)
		current_deps_present and no_stale_cached_deps
	}

	state_versions_for_deps : Runtime, List(Str) -> List(StateVersionEntry)
	state_versions_for_deps = |runtime, deps| {
		List.map(deps, |dep| { key: dep, version: current_state_version(runtime, dep) })
	}

	current_state_version : Runtime, Str -> U64
	current_state_version = |runtime, key| {
		match state_version_lookup(runtime.state_versions, key) {
			VersionFound(version) => version
			VersionMissing => 0
		}
	}

	state_version_lookup : List(StateVersionEntry), Str -> VersionLookup
	state_version_lookup = |entries, key| {
		List.fold(
			entries,
			VersionMissing,
			|acc, entry| match acc {
				VersionFound(_) => acc
				VersionMissing =>
					if entry.key == key {
						VersionFound(entry.version)
					} else {
						VersionMissing
					}
			},
		)
	}

	set_signal_cache : Runtime, Str, List(Str), NodeValue -> Runtime
	set_signal_cache = |runtime, key, signal_deps, value| {
		deps = state_versions_for_deps(runtime, signal_deps)
		initial : UpsertSignalCache
		initial = { entries: [], found: False }
		upsert = List.fold(
			runtime.signal_cache,
			initial,
			|acc, entry| if entry.key == key {
				{ entries: List.append(acc.entries, { key, value, deps }), found: True }
			} else {
				{ ..acc, entries: List.append(acc.entries, entry) }
			},
		)
		if upsert.found {
			{ ..runtime, signal_cache: upsert.entries }
		} else {
			{ ..runtime, signal_cache: List.append(upsert.entries, { key, value, deps }) }
		}
	}

	register_state_event_deps : Runtime, Str, List(Str) -> Runtime
	register_state_event_deps = |runtime, state_key, event_keys| {
		List.fold(event_keys, runtime, |acc, event_key| register_state_event_dep(acc, event_key, state_key))
	}

	register_state_event_dep : Runtime, Str, Str -> Runtime
	register_state_event_dep = |runtime, event_key, state_key| {
		if event_state_dep_exists(runtime.event_state_deps, event_key, state_key) {
			runtime
		} else {
			{ ..runtime, event_state_deps: List.append(runtime.event_state_deps, { event_key, state_key }) }
		}
	}

	event_state_dep_exists : List(EventStateDep), Str, Str -> Bool
	event_state_dep_exists = |deps, event_key, state_key| {
		List.any(deps, |dep| dep.event_key == event_key and dep.state_key == state_key)
	}

	append_unique_str : List(Str), Str -> List(Str)
	append_unique_str = |items, item| {
		if str_list_contains(items, item) {
			items
		} else {
			List.append(items, item)
		}
	}

	str_list_contains : List(Str), Str -> Bool
	str_list_contains = |items, item| {
		List.any(items, |existing| existing == item)
	}

	state_value : Runtime, Str, NodeValue -> { runtime : Runtime, value : NodeValue }
	state_value = |runtime, key, initial| {
		metrics0 = runtime.metrics
		runtime_with_count = { ..runtime, metrics: { ..metrics0, state_lookups: metrics0.state_lookups + 1 } }
		lookup = List.fold(
			runtime_with_count.states,
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
			{ runtime: runtime_with_count, value: lookup.value }
		} else {
			{
				runtime: { ..runtime_with_count, states: List.append(runtime_with_count.states, { key, value: initial }) },
				value: initial,
			}
		}
	}

	bump_state_version : Runtime, Str -> Runtime
	bump_state_version = |runtime, key| {
		next_version = current_state_version(runtime, key) + 1
		metrics0 = runtime.metrics
		runtime_with_count = { ..runtime, metrics: { ..metrics0, state_version_bumps: metrics0.state_version_bumps + 1 } }
		initial : UpsertStateVersion
		initial = { entries: [], found: False }
		upsert = List.fold(
			runtime_with_count.state_versions,
			initial,
			|acc, entry| if entry.key == key {
				{ entries: List.append(acc.entries, { key, version: next_version }), found: True }
			} else {
				{ ..acc, entries: List.append(acc.entries, entry) }
			},
		)
		if upsert.found {
			{ ..runtime_with_count, state_versions: upsert.entries }
		} else {
			{ ..runtime_with_count, state_versions: List.append(upsert.entries, { key, version: next_version }) }
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
			runtime2 =
				if changed {
					bump_state_version(runtime1, key)
				} else {
					runtime1
				}
			state1 = { ..state, runtime: runtime2, updated_keys: List.append(state.updated_keys, key) }
			{ state: state1, value: next }
		} else {
			{ state, value: previous }
		}
	}

	set_state : Runtime, Str, NodeValue -> Runtime
	set_state = |runtime, key, value| {
		initial : UpsertState
		initial = { entries: [], found: False }
		upsert = List.fold(
			runtime.states,
			initial,
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
