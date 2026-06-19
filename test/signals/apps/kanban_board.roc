app [main] { pf: platform "../platform/main.roc" }

import pf.Elem exposing [Elem]
import pf.Html
import pf.NodeValue exposing [NodeValue]
import pf.Signal
import pf.Ui

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

increment_i64 : I64 -> I64
increment_i64 = |current| current + 1

initial_tasks : List(Task)
initial_tasks = [Task.make("a", "Design signal graph"), Task.make("b", "Write platform glue"), Task.make("c", "Tune keyed diff")]

reordered_tasks : List(Task)
reordered_tasks = [Task.make("c", "Tune keyed diff"), Task.make("a", "Design signal graph"), Task.make("b", "Write platform glue")]

archived_tasks : List(Task)
archived_tasks = [Task.make("a", "Design signal graph"), Task.make("c", "Tune keyed diff")]

focused_tasks : List(Task)
focused_tasks = [Task.make("c", "Tune keyed diff")]

render_task : Str, Signal.Signal(Task) -> Elem
render_task = |label, _task_signal| {
	initial_count : I64
	initial_count = 0

	Ui.state(
		initial_count,
		|progress| {
			Ui.state(
				initial_count,
				|notes| {
					status =
						Signal.map2(
							progress.signal(),
							notes.signal(),
							|done, note_count| {
								done_text = count_label("progress", done)
								note_text = count_label("notes", note_count)
								concat3(concat3(label, " ", done_text), " / ", note_text)
							},
						)

					Html.section(
						label,
						[],
						[
							Html.paragraph(label),
							Html.button(Str.concat("Advance ", label), progress.on_unit(increment_i64)),
							Html.button(Str.concat("Add note ", label), notes.on_unit(increment_i64)),
							Html.text_s(status),
						],
					)
				},
			)
		},
	)
}

main : {} -> Elem
main = |_| {
	initial_filter : Bool
	initial_filter = False

	Ui.state(
		initial_tasks,
		|tasks| {
			Ui.state(
				initial_filter,
				|filter_active| {
					Ui.state(
						"",
						|reviewer| {
							filter_signal = filter_active.signal()
							filter_label =
								Signal.map(
									filter_signal,
									|active| if active {
										"Focus filter on"
									} else {
										"Focus filter off"
									},
								)
							visible_tasks =
								Signal.map2(
									filter_signal,
									tasks.signal(),
									|active, all_tasks| if active {
										focused_tasks
									} else {
										all_tasks
									},
								)
							reviewer_label = Signal.map(reviewer.signal(), |value| Str.concat("Reviewer: ", value))

							Html.div(
								[],
								[
									Html.heading("Kanban board"),
									Html.section(
										"Board controls",
										[],
										[
											Html.button("Reorder cards", tasks.on_unit(|_| reordered_tasks)),
											Html.button("Archive platform glue", tasks.on_unit(|_| archived_tasks)),
											Html.button("Reset board", tasks.on_unit(|_| initial_tasks)),
											Html.button("Toggle focus filter", filter_active.on_unit(|active| !active)),
											Html.text_s(filter_label),
											Html.text_input("Reviewer", reviewer.signal(), reviewer.on_str(|_, value| value)),
											Html.text_s(reviewer_label),
										],
									),
									Html.section(
										"Doing",
										[],
										[
											Ui.each(visible_tasks, |task| task.label, render_task),
										],
									),
								],
							)
						},
					)
				},
			)
		},
	)
}
