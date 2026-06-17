app [main] { pf: platform "../platform/main.roc" }

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

concat3 : Str, Str, Str -> Str
concat3 = |a, b, c| Str.concat(Str.concat(a, b), c)

count_label : Str, I64 -> Str
count_label = |name, value| concat3(name, ": ", value.to_str())

render_task : Task -> Elem.Elem
render_task = |task| {
	{ sender: progress_send, receiver: progress_clicks } = Reactive.Event.unit_channel(Str.concat("task_progress_click:", task.label))
	{ sender: note_send, receiver: note_clicks } = Reactive.Event.unit_channel(Str.concat("task_note_click:", task.label))
	progress_deltas : Reactive.Event(I64)
	progress_deltas = Reactive.Event.map_unit_i64_const(progress_clicks, 1)
	note_deltas : Reactive.Event(I64)
	note_deltas = Reactive.Event.map_unit_i64_const(note_clicks, 1)
	progress : Reactive.Signal(I64)
	progress = Reactive.Signal.fold_i64(Str.concat("task_progress:", task.label), 0, progress_deltas, |current, delta| current + delta)
	notes : Reactive.Signal(I64)
	notes = Reactive.Signal.fold_i64(Str.concat("task_notes:", task.label), 0, note_deltas, |current, delta| current + delta)
	status = 
		Reactive.Signal.map2_i64_i64_str_keyed(
			Str.concat("task_status:", task.label),
			progress,
			notes,
			|done, note_count| {
				done_text = count_label("progress", done)
				note_text = count_label("notes", note_count)
				concat3(concat3(task.label, " ", done_text), " / ", note_text)
			},
		)

	Elem.section(
		task.label,
		[
			Elem.paragraph(task.label),
			Elem.action_button(
				{
					on_click: progress_send,
					label: Reactive.Signal.const_str(Str.concat("Advance ", task.label)),
					disabled: Reactive.Signal.const_bool(False),
				},
			),
			Elem.action_button(
				{
					on_click: note_send,
					label: Reactive.Signal.const_str(Str.concat("Add note ", task.label)),
					disabled: Reactive.Signal.const_bool(False),
				},
			),
			Elem.label(status),
		],
	)
}

main : {} -> Elem.Elem
main = |_| {
	initial_tasks = [Task.make("a", "Design signal graph"), Task.make("b", "Write platform glue"), Task.make("c", "Tune keyed diff")]

	{ sender: reorder_send, receiver: reorder_clicks } = Reactive.Event.unit_channel("reorder_click")
	{ sender: archive_send, receiver: archive_clicks } = Reactive.Event.unit_channel("archive_click")
	{ sender: reset_send, receiver: reset_clicks } = Reactive.Event.unit_channel("reset_click")
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
		"tasks",
		initial_tasks,
		list_commands,
		|_current, next| next,
	)

	{ sender: filter_send, receiver: filter_clicks } = Reactive.Event.unit_channel("filter_click")
	filter_active : Reactive.Signal(Bool)
	filter_active = Reactive.Signal.fold_bool_toggle("filter_active", False, filter_clicks)
	filter_label = 
		Reactive.Signal.map_keyed(
			"filter_label",
			filter_active,
			|active| if active {
				"Focus filter on"
			} else {
				"Focus filter off"
			},
	)
	visible_tasks : Reactive.Signal(List(Task))
	visible_tasks = 
		Reactive.Signal.map2_keyed(
			"visible_tasks",
			filter_active,
			tasks,
			|active, all_tasks| if active {
				[Task.make("c", "Tune keyed diff")]
			} else {
				all_tasks
			},
		)

	{ sender: reviewer_send, receiver: reviewer_changes } = Reactive.Event.channel("reviewer_change")
	reviewer : Reactive.Signal(Str)
	reviewer = Reactive.Signal.hold("reviewer", "", reviewer_changes)
	reviewer_label = Reactive.Signal.map_keyed("reviewer_label", reviewer, |value| Str.concat("Reviewer: ", value))

	Elem.div(
		[
			Elem.heading("Kanban board"),
			Elem.section(
				"Board controls",
				[
					Elem.action_button(
						{
							on_click: reorder_send,
							label: Reactive.Signal.const_str("Reorder cards"),
							disabled: Reactive.Signal.const_bool(False),
						},
					),
					Elem.action_button(
						{
							on_click: archive_send,
							label: Reactive.Signal.const_str("Archive platform glue"),
							disabled: Reactive.Signal.const_bool(False),
						},
					),
					Elem.action_button(
						{
							on_click: reset_send,
							label: Reactive.Signal.const_str("Reset board"),
							disabled: Reactive.Signal.const_bool(False),
						},
					),
					Elem.action_button(
						{
							on_click: filter_send,
							label: Reactive.Signal.const_str("Toggle focus filter"),
							disabled: Reactive.Signal.const_bool(False),
						},
					),
					Elem.label(filter_label),
					Elem.text_input(
						{
							label: "Reviewer",
							value: reviewer,
							on_input: reviewer_send,
							disabled: Reactive.Signal.const_bool(False),
						},
					),
					Elem.label(reviewer_label),
				],
			),
			Elem.section(
				"Doing",
				[
					Elem.each(visible_tasks, |task| task.label, render_task),
				],
			),
		],
	)
}
