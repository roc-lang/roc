app [BoundaryPayload, program] { pf: platform "../platform/main.roc" }

import pf.Elem
import pf.NodeValue exposing [NodeValue]
import pf.Reactive

Task := { id : Str, label : Str }.{
	make : Str, Str -> Task
	make = |id, label| { id, label }

	encode : Task, NodeValue -> Try(NodeValue, [])
	encode = |task, fmt| [task.id, task.label].encode(fmt)

	decode : NodeValue, NodeValue -> (Try(Task, [TypeMismatch]), NodeValue)
	decode = |nv, fmt| {
		(result, rest) = NodeValue.decode_list(fmt, nv, |source, f| Str.decode(source, f))
		task_result =
			match result {
				Ok(fields) =>
					match (List.get(fields, 0), List.get(fields, 1)) {
						(Ok(id), Ok(label)) => Ok(Task.make(id, label))
						_ => Err(TypeMismatch)
					}

				Err(err) => Err(err)
			}

		(task_result, rest)
	}
}

count_label : Str, I64 -> Str
count_label = |name, value| Str.concat(Str.concat(name, ": "), value.to_str())

render_task : Reactive.EventSender(Reactive.Unit), Reactive.EventSender(Reactive.Unit), Task => Elem.Elem
render_task = |mount_send, unmount_send, task| {
	{ sender: progress_send, receiver: progress_clicks } = Reactive.Event.unit_channel!()
	{ sender: note_send, receiver: note_clicks } = Reactive.Event.unit_channel!()
	progress_deltas : Reactive.Event(I64)
	progress_deltas = Reactive.Event.map_unit_i64_const(progress_clicks, 1)
	note_deltas : Reactive.Event(I64)
	note_deltas = Reactive.Event.map_unit_i64_const(note_clicks, 1)
	progress : Reactive.Signal(I64)
	progress = Reactive.Signal.fold_i64(0, progress_deltas, |current, delta| current + delta)
	notes : Reactive.Signal(I64)
	notes = Reactive.Signal.fold_i64(0, note_deltas, |current, delta| current + delta)
	status =
		Reactive.Signal.map2_i64_i64_str(
			progress,
			notes,
			|done, note_count| {
				done_text = count_label("progress", done)
				note_text = count_label("notes", note_count)
				Str.concat(Str.concat(done_text, " / "), note_text)
			},
		)

	Elem.div(
		[
			Elem.lifecycle({ on_mount: mount_send, on_unmount: unmount_send }),
			Elem.text(task.label),
			Elem.button({ on_click: progress_send, label: Reactive.Signal.const_str("advance") }),
			Elem.button({ on_click: note_send, label: Reactive.Signal.const_str("note") }),
			Elem.label(status),
		],
	)
}

main! : () => {}
main! = || {
	initial_tasks = [Task.make("a", "Design signal graph"), Task.make("b", "Write platform glue"), Task.make("c", "Tune keyed diff")]

	{ sender: reorder_send, receiver: reorder_clicks } = Reactive.Event.unit_channel!()
	{ sender: archive_send, receiver: archive_clicks } = Reactive.Event.unit_channel!()
	{ sender: reset_send, receiver: reset_clicks } = Reactive.Event.unit_channel!()
	reordered : Reactive.Event(List(Task))
	reordered =
		Reactive.Event.map(
			reorder_clicks,
			|_| [Task.make("c", "Tune keyed diff"), Task.make("a", "Design signal graph"), Task.make("b", "Write platform glue")],
		)
	archived : Reactive.Event(List(Task))
	archived = Reactive.Event.map(archive_clicks, |_| [Task.make("a", "Design signal graph"), Task.make("c", "Tune keyed diff")])
	reset : Reactive.Event(List(Task))
	reset = Reactive.Event.map(reset_clicks, |_| initial_tasks)
	list_commands = Reactive.Event.merge(Reactive.Event.merge(reordered, archived), reset)
	tasks : Reactive.Signal(List(Task))
	tasks = Reactive.Signal.fold(
		initial_tasks,
		list_commands,
		|_current, next| next,
	)

	{ sender: filter_send, receiver: filter_clicks } = Reactive.Event.unit_channel!()
	filter_active : Reactive.Signal(Bool)
	filter_active = Reactive.Signal.fold_bool_toggle(False, filter_clicks)
	filter_label =
		Reactive.Signal.map(
			filter_active,
			|active| if active {
				"Focus filter on"
			} else {
				"Focus filter off"
			},
		)
	visible_tasks : Reactive.Signal(List(Task))
	visible_tasks =
		Reactive.Signal.map2(
			filter_active,
			tasks,
			|active, all_tasks| if active {
				[Task.make("c", "Tune keyed diff")]
			} else {
				all_tasks
			},
		)

	{ sender: task_mount_send, receiver: task_mounts } = Reactive.Event.unit_channel!()
	{ sender: task_unmount_send, receiver: task_unmounts } = Reactive.Event.unit_channel!()
	task_mount_deltas : Reactive.Event(I64)
	task_mount_deltas = Reactive.Event.map_unit_i64_const(task_mounts, 1)
	task_unmount_deltas : Reactive.Event(I64)
	task_unmount_deltas = Reactive.Event.map_unit_i64_const(task_unmounts, 1)
	task_mount_count : Reactive.Signal(I64)
	task_mount_count = Reactive.Signal.fold_i64(0, task_mount_deltas, |current, delta| current + delta)
	task_unmount_count : Reactive.Signal(I64)
	task_unmount_count = Reactive.Signal.fold_i64(0, task_unmount_deltas, |current, delta| current + delta)

	Elem.run!(
		Elem.div(
			[
				Elem.text("Kanban board"),
				Elem.button({ on_click: reorder_send, label: Reactive.Signal.const_str("reorder") }),
				Elem.button({ on_click: archive_send, label: Reactive.Signal.const_str("archive") }),
				Elem.button({ on_click: reset_send, label: Reactive.Signal.const_str("reset") }),
				Elem.button({ on_click: filter_send, label: Reactive.Signal.const_str("focus") }),
				Elem.label(filter_label),
				Elem.label(Reactive.Signal.map_i64_str(task_mount_count, |n| count_label("mounted", n))),
				Elem.label(Reactive.Signal.map_i64_str(task_unmount_count, |n| count_label("unmounted", n))),
				Elem.each(visible_tasks, |task| task.id, |task| render_task(task_mount_send, task_unmount_send, task)),
			],
		),
	)
}

BoundaryPayload : { total : I64, item : Task, items : List(Task) }

program = {
	main!,
	init_boundary_payload: |_| {
		total: 3,
		item: Task.make("a", "Design signal graph"),
		items: [Task.make("a", "Design signal graph"), Task.make("b", "Write platform glue")],
	},
	boundary_payload_total: |payload| payload.total,
	boundary_payload_item_key: |payload| payload.item.id,
	boundary_payload_item_label: |payload| payload.item.label,
	boundary_payload_items_len: |payload| List.len(payload.items),
}
