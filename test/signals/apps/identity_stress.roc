app [main] { pf: platform "../platform/main.roc" }

import pf.Elem
import pf.NodeValue exposing [NodeValue]
import pf.Reactive

Row := { id : Str, label : Str }.{
	make : Str, Str -> Row
	make = |id, label| { id, label }

	encode : Row, NodeValue -> Try(NodeValue, [])
	encode = |row, fmt| [row.id, row.label].encode(fmt)

	decode : NodeValue, NodeValue -> (Try(Row, [TypeMismatch]), NodeValue)
	decode = |nv, fmt| {
		(result, rest) = NodeValue.decode_list(fmt, nv, |source, f| Str.decode(source, f))
		row_result =
			match result {
				Ok(fields) =>
					match (List.get(fields, 0), List.get(fields, 1)) {
						(Ok(id), Ok(label)) => Ok(Row.make(id, label))
						_ => Err(TypeMismatch)
					}

				Err(err) => Err(err)
			}

		(row_result, rest)
	}
}

concat3 : Str, Str, Str -> Str
concat3 = |a, b, c| Str.concat(Str.concat(a, b), c)

row_label : Str, I64 -> Str
row_label = |label, count| concat3(label, " detail count: ", count.to_str())

main : {} -> Elem.Elem
main = |_| {
	row_a = Row.make("a", "Alpha")
	row_b = Row.make("b", "Beta")
	row_c = Row.make("c", "Gamma")
	initial_rows = [row_a, row_b, row_c]
	reordered_rows = [row_c, row_a, row_b]

	{ sender: reorder_send, receiver: reorder_clicks } = Reactive.Event.unit_channel("identity_reorder_click")
	reordered : Reactive.Event(List(Row))
	reordered = Reactive.Event.map(reorder_clicks, |_| reordered_rows)
	rows : Reactive.Signal(List(Row))
	rows = Reactive.Signal.fold(
		"identity_rows",
		initial_rows,
		reordered,
		|_current, next| next,
	)

	panel_visible : Reactive.Signal(Bool)
	panel_visible = Reactive.Signal.const_bool(True)

	render_row = |row| {
		{ sender: bump_send, receiver: bump_clicks } = Reactive.Event.unit_channel(Str.concat("identity_bump:", row.label))
		bump_deltas : Reactive.Event(I64)
		bump_deltas = Reactive.Event.map_unit_i64_const(bump_clicks, 1)
		count : Reactive.Signal(I64)
		count = Reactive.Signal.fold_i64(Str.concat("identity_count:", row.label), 0, bump_deltas, |current, delta| current + delta)
		count_label = Reactive.Signal.map_i64_str_keyed(Str.concat("identity_count_label:", row.label), count, |n| row_label(row.label, n))
		has_count = Reactive.Signal.map_keyed(Str.concat("identity_has_count:", row.label), count, |n| n > 0)

		Elem.section(
			row.label,
			[
				Elem.action_button(
					{
						on_click: bump_send,
						label: Reactive.Signal.const_str(Str.concat("Bump ", row.label)),
						disabled: Reactive.Signal.const_bool(False),
					},
				),
				Elem.when(
					has_count,
					Elem.div(
						[
							Elem.label(count_label),
						],
					),
					Elem.paragraph(Str.concat(row.label, " waiting")),
				),
			],
		)
	}

	Elem.div(
		[
			Elem.heading("Identity stress"),
			Elem.section(
				"Controls",
				[
					Elem.action_button(
						{
							on_click: reorder_send,
							label: Reactive.Signal.const_str("Reorder rows"),
							disabled: Reactive.Signal.const_bool(False),
						},
					),
				],
			),
			Elem.when(
				panel_visible,
				Elem.section(
					"Rows active",
					[
						Elem.each(rows, |row| row.id, render_row),
					],
				),
				Elem.section(
					"Rows inactive",
					[
						Elem.paragraph("Rows hidden"),
					],
				),
			),
		],
	)
}
