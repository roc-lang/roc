app [main] { pf: platform "../platform/main.roc" }

import pf.Elem exposing [Elem]
import pf.Html
import pf.NodeValue exposing [NodeValue]
import pf.Signal
import pf.Ui

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

render_row : Str, Signal.Signal(Row) -> Elem
render_row = |label, _row_signal| {
	Ui.state(
		0,
		|count| {
			count_signal = count.signal()
			count_label = Signal.map(count_signal, |n| row_label(label, n))
			has_count = Signal.map(count_signal, |n| n > 0)

			Html.section(
				label,
				[],
				[
					Html.button(Str.concat("Bump ", label), count.on_unit(|n| n + 1)),
					Ui.when(
						has_count,
						|_| Html.div([], [Html.text_s(count_label)]),
						|_| Html.paragraph(Str.concat(label, " waiting")),
					),
				],
			)
		},
	)
}

main : {} -> Elem
main = |_| {
	row_a = Row.make("a", "Alpha")
	row_b = Row.make("b", "Beta")
	row_c = Row.make("c", "Gamma")
	initial_rows = [row_a, row_b, row_c]
	reordered_rows = [row_c, row_a, row_b]

	initial_reordered : Bool
	initial_reordered = False

	Ui.state(
		initial_reordered,
		|reordered| {
			rows =
				Signal.map(
					reordered.signal(),
					|is_reordered| if is_reordered {
						reordered_rows
					} else {
						initial_rows
					},
				)

			Html.div(
				[],
				[
					Html.heading("Identity stress"),
					Html.section(
						"Controls",
						[],
						[
							Html.button("Reorder rows", reordered.on_unit(|flag| !flag)),
						],
					),
					Ui.when(
						Signal.const_bool(True),
						|_| {
							Html.section(
								"Rows active",
								[],
								[
									Ui.each(rows, |row| row.label, render_row),
								],
							)
						},
						|_| {
							Html.section(
								"Rows inactive",
								[],
								[
									Html.paragraph("Rows hidden"),
								],
							)
						},
					),
				],
			)
		},
	)
}
