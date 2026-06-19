app [main] { pf: platform "../platform/main.roc" }

import pf.Elem exposing [Elem]
import pf.Html
import pf.Signal
import pf.Ui

concat3 : Str, Str, Str -> Str
concat3 = |a, b, c| Str.concat(Str.concat(a, b), c)

row_label : Str, I64 -> Str
row_label = |label, count| concat3(label, " detail count: ", count.to_str())

row_a : Str
row_a = "Alpha"

row_b : Str
row_b = "Beta"

row_c : Str
row_c = "Gamma"

row_d : Str
row_d = "Delta"

initial_rows : List(Str)
initial_rows = [row_a, row_b, row_c]

inserted_rows : List(Str)
inserted_rows = [row_a, row_d, row_b, row_c]

reordered_rows : List(Str)
reordered_rows = [row_c, row_a, row_b]

inserted_reordered_rows : List(Str)
inserted_reordered_rows = [row_c, row_a, row_d, row_b]

initial_filtered_rows : List(Str)
initial_filtered_rows = [row_a, row_c]

inserted_filtered_rows : List(Str)
inserted_filtered_rows = [row_a, row_d, row_c]

reordered_filtered_rows : List(Str)
reordered_filtered_rows = [row_c, row_a]

inserted_reordered_filtered_rows : List(Str)
inserted_reordered_filtered_rows = [row_c, row_a, row_d]

rows_for_shape : I64, Bool -> List(Str)
rows_for_shape = |shape, hide_beta| {
	if hide_beta {
		if shape == 0 {
			initial_filtered_rows
		} else if shape == 1 {
			inserted_filtered_rows
		} else if shape == 2 {
			reordered_filtered_rows
		} else {
			inserted_reordered_filtered_rows
		}
	} else {
		if shape == 0 {
			initial_rows
		} else if shape == 1 {
			inserted_rows
		} else if shape == 2 {
			reordered_rows
		} else {
			inserted_reordered_rows
		}
	}
}

render_row : Str, Signal.Signal(Str) -> Elem
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
	initial_reordered : Bool
	initial_reordered = False
	initial_inserted : Bool
	initial_inserted = False
	initial_filtered : Bool
	initial_filtered = False
	initial_active : Bool
	initial_active = True

	Ui.state(
		initial_reordered,
		|reordered| {
			Ui.state(
				initial_inserted,
				|inserted| {
					Ui.state(
						initial_filtered,
						|filtered| {
							Ui.state(
								initial_active,
								|active| {
									shape : Signal.Signal(I64)
									shape =
										Signal.map2(
											reordered.signal(),
											inserted.signal(),
											|is_reordered, has_delta| if is_reordered {
												if has_delta {
													3
												} else {
													2
												}
											} else {
												if has_delta {
													1
												} else {
													0
												}
											},
										)
									rows =
										Signal.map2(
											shape,
											filtered.signal(),
											|shape_code, hide_beta| rows_for_shape(shape_code, hide_beta),
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
													Html.button("Insert Delta", inserted.on_unit(|flag| !flag)),
													Html.button("Filter Beta", filtered.on_unit(|flag| !flag)),
													Html.button("Toggle rows active", active.on_unit(|flag| !flag)),
												],
											),
											Ui.when(
												active.signal(),
												|_| {
													Html.section(
														"Rows active",
														[],
														[
															Ui.each(rows, |label| label, render_row),
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
						},
					)
				},
			)
		},
	)
}
