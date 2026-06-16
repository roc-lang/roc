app [BoundaryPayload, program] { pf: platform "./platform/main.roc" }

import pf.Elem
import pf.NodeValue exposing [NodeValue]
import pf.Reactive

Counter := { count : I64 }.{
	init : I64 -> Counter
	init = |count| { count: count }

	encode : Counter, NodeValue -> Try(NodeValue, [])
	encode = |counter, fmt| counter.count.encode(fmt)

	decode : NodeValue, NodeValue -> (Try(Counter, [TypeMismatch]), NodeValue)
	decode = |nv, fmt| {
		(result, rest) = I64.decode(nv, fmt)
		counter_result =
			match result {
				Ok(count) => Ok(Counter.init(count))
				Err(err) => Err(err)
			}

		(counter_result, rest)
	}

	render! : Reactive.Signal(Counter) => Elem.Component(Counter)
	render! = |state| {
		{ sender: dec_send, receiver: dec_clicks } = Reactive.Event.unit_channel!()
		{ sender: inc_send, receiver: inc_clicks } = Reactive.Event.unit_channel!()

		dec_changes =
			Reactive.Event.with_latest_unit(
				dec_clicks,
				state,
				|counter| Counter.init(counter.count - 1),
			)

		inc_changes =
			Reactive.Event.with_latest_unit(
				inc_clicks,
				state,
				|counter| Counter.init(counter.count + 1),
			)

		changes = Reactive.Event.merge(dec_changes, inc_changes)

		count : Reactive.Signal(I64)
		count = Reactive.Signal.map(state, |counter| counter.count)
		count_label = Reactive.Signal.map_i64_str(count, |n| n.to_str())

		Elem.component(
			Elem.div(
				[
					Elem.button({ on_click: dec_send, label: Reactive.Signal.const_str("-") }),
					Elem.label(count_label),
					Elem.button({ on_click: inc_send, label: Reactive.Signal.const_str("+") }),
				],
			),
			changes,
		)
	}
}

Item := { id : Str, label : Str }.{
	make : Str, Str -> Item
	make = |id, label| { id, label }

	encode : Item, NodeValue -> Try(NodeValue, [])
	encode = |item, fmt| [item.id, item.label].encode(fmt)

	decode : NodeValue, NodeValue -> (Try(Item, [TypeMismatch]), NodeValue)
	decode = |nv, fmt| {
		(result, rest) = NodeValue.decode_list(fmt, nv, |source, f| Str.decode(source, f))
		item_result =
			match result {
				Ok(fields) =>
					match (List.get(fields, 0), List.get(fields, 1)) {
						(Ok(id), Ok(label)) => Ok(Item.make(id, label))
						_ => Err(TypeMismatch)
					}

				Err(err) => Err(err)
			}

		(item_result, rest)
	}
}

App := { left : Counter, right : Counter }.{
	init : {} -> App
	init = |_| App.make(Counter.init(0), Counter.init(0))

	make : Counter, Counter -> App
	make = |left, right| { left, right }

	encode : App, NodeValue -> Try(NodeValue, [])
	encode = |model, fmt| [model.left, model.right].encode(fmt)

	decode : NodeValue, NodeValue -> (Try(App, [TypeMismatch]), NodeValue)
	decode = |nv, fmt| {
		(result, rest) = NodeValue.decode_list(fmt, nv, |source, f| Counter.decode(source, f))
		app_result =
			match result {
				Ok(fields) =>
					match (List.get(fields, 0), List.get(fields, 1)) {
						(Ok(left), Ok(right)) => Ok(App.make(left, right))
						_ => Err(TypeMismatch)
					}

				Err(err) => Err(err)
			}

		(app_result, rest)
	}

	local_counter : Str, I64 => { elem : Elem, count : Reactive.Signal(I64) }
	local_counter = |name, initial| {
		{ sender: dec_send, receiver: dec_clicks } = Reactive.Event.unit_channel!()
		{ sender: inc_send, receiver: inc_clicks } = Reactive.Event.unit_channel!()

		decs : Reactive.Event(I64)
		decs = Reactive.Event.map_unit_i64_const(dec_clicks, -1)
		incs : Reactive.Event(I64)
		incs = Reactive.Event.map_unit_i64_const(inc_clicks, 1)
		changes = Reactive.Event.merge(decs, incs)
		count = Reactive.Signal.fold_i64(initial, changes, |current, delta| current + delta)
		count_label = Reactive.Signal.map_i64_str(count, |n| n.to_str())

		{
			elem: Elem.div(
				[
					Elem.text(name),
					Elem.button({ on_click: dec_send, label: Reactive.Signal.const_str("-") }),
					Elem.label(count_label),
					Elem.button({ on_click: inc_send, label: Reactive.Signal.const_str("+") }),
				],
			),
			count,
		}
	}

	render_item :
		Reactive.EventSender(Reactive.Unit),
		Reactive.EventSender(Reactive.Unit),
		Item => Elem
	render_item = |mount_send, unmount_send, item| {
		{ sender: inc_send, receiver: inc_clicks } = Reactive.Event.unit_channel!()
		incs : Reactive.Event(I64)
		incs = Reactive.Event.map_unit_i64_const(inc_clicks, 1)
		count : Reactive.Signal(I64)
		count = Reactive.Signal.fold_i64(0, incs, |current, delta| current + delta)
		count_label = Reactive.Signal.map_i64_str(count, |n| n.to_str())

		Elem.div(
			[
				Elem.lifecycle({ on_mount: mount_send, on_unmount: unmount_send }),
				Elem.text(item.label),
				Elem.button({ on_click: inc_send, label: Reactive.Signal.const_str("+") }),
				Elem.label(count_label),
			],
		)
	}

	render! : Reactive.Signal(App) => Elem.Component(App)
	render! = |state| {
		left_render! =
			Elem.translate(
				Counter.render!,
				|model| model.left,
				|model, counter| App.make(counter, model.right),
			)

		right_render! =
			Elem.translate(
				Counter.render!,
				|model| model.right,
				|model, counter| App.make(model.left, counter),
			)

		controlled_left = left_render!(state)
		controlled_right = right_render!(state)
		controlled_changes =
			Reactive.Event.merge(
				Elem.Component.changes(controlled_left),
				Elem.Component.changes(controlled_right),
			)
		controlled_total : Reactive.Signal(I64)
		controlled_total = Reactive.Signal.map(state, |model| model.left.count + model.right.count)
		controlled_total_label = Reactive.Signal.map_i64_str(controlled_total, |n| n.to_str())

		local_left = App.local_counter("Local left", 0)
		local_right = App.local_counter("Local right", 10)
		local_total : Reactive.Signal(I64)
		local_total =
			Reactive.Signal.map2_i64_i64(
				local_left.count,
				local_right.count,
				|left, right| left + right,
			)
		local_total_label = Reactive.Signal.map_i64_str(local_total, |n| n.to_str())
		diamond_left : Reactive.Signal(I64)
		diamond_left = Reactive.Signal.map_i64_i64(local_total, |n| n + 1)
		diamond_right : Reactive.Signal(I64)
		diamond_right = Reactive.Signal.map_i64_i64(local_total, |n| n * 10)
		diamond_label =
			Reactive.Signal.map2_i64_i64_str(
				diamond_left,
				diamond_right,
				|left, right| Str.concat(Str.concat(left.to_str(), "/"), right.to_str()),
			)

		{ sender: merge_send, receiver: merge_clicks } = Reactive.Event.unit_channel!()
		merge_left : Reactive.Event(I64)
		merge_left = Reactive.Event.map_unit_i64_const(merge_clicks, 1)
		merge_right : Reactive.Event(I64)
		merge_right = Reactive.Event.map_unit_i64_const(merge_clicks, 10)
		merge_count : Reactive.Signal(I64)
		merge_count = Reactive.Signal.fold_i64(0, Reactive.Event.merge(merge_left, merge_right), |current, delta| current + delta)
		merge_label = Reactive.Signal.map_i64_str(merge_count, |n| n.to_str())

		{ sender: toggle_send, receiver: toggle_clicks } = Reactive.Event.unit_channel!()
		visible : Reactive.Signal(Bool)
		visible = Reactive.Signal.fold_bool_toggle(True, toggle_clicks)
		{ sender: dyn_mount_send, receiver: dyn_mounts } = Reactive.Event.unit_channel!()
		{ sender: dyn_unmount_send, receiver: dyn_unmounts } = Reactive.Event.unit_channel!()
		dyn_mount_deltas : Reactive.Event(I64)
		dyn_mount_deltas = Reactive.Event.map_unit_i64_const(dyn_mounts, 1)
		dyn_unmount_deltas : Reactive.Event(I64)
		dyn_unmount_deltas = Reactive.Event.map_unit_i64_const(dyn_unmounts, 1)
		dyn_mount_count : Reactive.Signal(I64)
		dyn_mount_count = Reactive.Signal.fold_i64(0, dyn_mount_deltas, |current, delta| current + delta)
		dyn_unmount_count : Reactive.Signal(I64)
		dyn_unmount_count = Reactive.Signal.fold_i64(0, dyn_unmount_deltas, |current, delta| current + delta)

		{ sender: reorder_send, receiver: reorder_clicks } = Reactive.Event.unit_channel!()
		{ sender: remove_send, receiver: remove_clicks } = Reactive.Event.unit_channel!()
		reordered_items : Reactive.Event(List(Item))
		reordered_items =
			Reactive.Event.map(
				reorder_clicks,
				|_| [Item.make("b", "Beta"), Item.make("a", "Alpha")],
			)
		removed_items : Reactive.Event(List(Item))
		removed_items =
			Reactive.Event.map(
				remove_clicks,
				|_| [Item.make("a", "Alpha")],
			)
		item_commands = Reactive.Event.merge(reordered_items, removed_items)
		items : Reactive.Signal(List(Item))
		items =
			Reactive.Signal.fold(
				[Item.make("a", "Alpha"), Item.make("b", "Beta")],
				item_commands,
				|_current, next| next,
			)
		{ sender: item_mount_send, receiver: item_mounts } = Reactive.Event.unit_channel!()
		{ sender: item_unmount_send, receiver: item_unmounts } = Reactive.Event.unit_channel!()
		item_mount_deltas : Reactive.Event(I64)
		item_mount_deltas = Reactive.Event.map_unit_i64_const(item_mounts, 1)
		item_unmount_deltas : Reactive.Event(I64)
		item_unmount_deltas = Reactive.Event.map_unit_i64_const(item_unmounts, 1)
		item_mount_count : Reactive.Signal(I64)
		item_mount_count = Reactive.Signal.fold_i64(0, item_mount_deltas, |current, delta| current + delta)
		item_unmount_count : Reactive.Signal(I64)
		item_unmount_count = Reactive.Signal.fold_i64(0, item_unmount_deltas, |current, delta| current + delta)

		Elem.component(
			Elem.div(
				[
					Elem.text("Controlled"),
					Elem.Component.elem(controlled_left),
					Elem.Component.elem(controlled_right),
					Elem.text("Controlled total"),
					Elem.label(controlled_total_label),

					Elem.text("Local"),
					local_left.elem,
					local_right.elem,
					Elem.text("Local total"),
					Elem.label(local_total_label),
					Elem.text("Diamond"),
					Elem.label(diamond_label),
					Elem.text("Merge"),
					Elem.button({ on_click: merge_send, label: Reactive.Signal.const_str("merge") }),
					Elem.label(merge_label),

					Elem.text("Dynamic"),
					Elem.label(Reactive.Signal.map_i64_str(dyn_mount_count, |n| n.to_str())),
					Elem.label(Reactive.Signal.map_i64_str(dyn_unmount_count, |n| n.to_str())),
					Elem.button({ on_click: toggle_send, label: Reactive.Signal.const_str("toggle") }),
					Elem.when(
						visible,
						Elem.div(
							[
								Elem.lifecycle({ on_mount: dyn_mount_send, on_unmount: dyn_unmount_send }),
								Elem.text("Dynamic on"),
							],
						),
						Elem.div([Elem.text("Dynamic off")]),
					),

					Elem.text("Keyed"),
					Elem.label(Reactive.Signal.map_i64_str(item_mount_count, |n| n.to_str())),
					Elem.label(Reactive.Signal.map_i64_str(item_unmount_count, |n| n.to_str())),
					Elem.button({ on_click: reorder_send, label: Reactive.Signal.const_str("reorder") }),
					Elem.button({ on_click: remove_send, label: Reactive.Signal.const_str("remove") }),
					Elem.each(items, |item| item.id, |item| App.render_item(item_mount_send, item_unmount_send, item)),
				],
			),
			controlled_changes,
		)
	}
}

main! : () => {}
main! = || {
	Elem.run_component!(App.init({}), App.render!)
}

BoundaryPayload : { model : App, item : Item, items : List(Item) }

program = {
	main!,
	init_boundary_payload: |_| {
		model: App.init({}),
		item: Item.make("a", "Alpha changed"),
		items: [Item.make("a", "Alpha"), Item.make("b", "Beta")],
	},
	boundary_payload_total: |payload| payload.model.left.count + payload.model.right.count,
	boundary_payload_item_key: |payload| payload.item.id,
	boundary_payload_item_label: |payload| payload.item.label,
	boundary_payload_items_len: |payload| List.len(payload.items),
}
