app [main] { pf: platform "../platform/main.roc" }

import pf.Elem exposing [Elem]
import pf.Html
import pf.NodeValue exposing [NodeValue]
import pf.Signal
import pf.Ui

Line := { id : Str, label : Str }.{
	make : Str, Str -> Line
	make = |id, label| { id, label }

	encode : Line, NodeValue -> Try(NodeValue, [])
	encode = |line, fmt| [line.id, line.label].encode(fmt)

	decode : NodeValue, NodeValue -> (Try(Line, [TypeMismatch]), NodeValue)
	decode = |nv, fmt| {
		(result, rest) = NodeValue.decode_list(fmt, nv, |source, f| Str.decode(source, f))
		line_result =
			match result {
				Ok(fields) =>
					match (List.get(fields, 0), List.get(fields, 1)) {
						(Ok(id), Ok(label)) => Ok(Line.make(id, label))
						_ => Err(TypeMismatch)
					}

				Err(err) => Err(err)
			}

		(line_result, rest)
	}
}

concat3 : Str, Str, Str -> Str
concat3 = |a, b, c| Str.concat(Str.concat(a, b), c)

label_i64 : Str, I64 -> Str
label_i64 = |name, value| concat3(name, ": ", value.to_str())

step_label : I64 -> Str
step_label = |step| {
	if step == 0 {
		"Step 1 Cart"
	} else if step == 1 {
		"Step 2 Delivery"
	} else {
		"Step 3 Review"
	}
}

next_step : I64 -> I64
next_step = |step| {
	next = step + 1
	if next > 2 {
		2
	} else {
		next
	}
}

prev_step : I64 -> I64
prev_step = |step| {
	next = step - 1
	if next < 0 {
		0
	} else {
		next
	}
}

initial_lines : List(Line)
initial_lines = [Line.make("seats", "3 seats"), Line.make("support", "Priority support")]

team_lines : List(Line)
team_lines = [Line.make("seats", "3 seats"), Line.make("support", "Priority support"), Line.make("audit", "Audit log export")]

basic_lines : List(Line)
basic_lines = [Line.make("seats", "3 seats")]

render_line : Str, Signal.Signal(Line) -> Elem
render_line = |label, _line_signal| {
	initial_quantity : I64
	initial_quantity = 1

	Ui.state(
		initial_quantity,
		|quantity| {
			quantity_label =
				Signal.map(
					quantity.signal(),
					|n| concat3(label, " quantity: ", n.to_str()),
				)

			Html.section(
				label,
				[],
				[
					Html.paragraph(label),
					Html.button(
						Str.concat("Decrease ", label),
						quantity.on_unit(|current| {
							next = current - 1
							if next < 0 {
								0
							} else {
								next
							}
						}),
					),
					Html.text_s(quantity_label),
					Html.button(Str.concat("Increase ", label), quantity.on_unit(|current| current + 1)),
				],
			)
		},
	)
}

main : {} -> Elem
main = |_| {
	initial_terms : Bool
	initial_terms = False

	Ui.state(
		0,
		|step| {
			Ui.state(
				"",
				|email| {
					Ui.state(
						"",
						|address| {
							Ui.state(
								initial_terms,
								|terms| {
									Ui.state(
										initial_lines,
										|lines| {
											Ui.state(
												0,
												|submit_count| {
													step_signal = step.signal()
													step_text = Signal.map(step_signal, step_label)
													is_cart = Signal.map(step_signal, |value| value == 0)
													is_delivery = Signal.map(step_signal, |value| value == 1)
													terms_signal = terms.signal()
													terms_text =
														Signal.map(
															terms_signal,
															|accepted| if accepted {
																"Terms accepted"
															} else {
																"Terms pending"
															},
														)
													submit_disabled = Signal.map(terms_signal, |accepted| !accepted)
													review_label =
														Signal.map(
															submit_count.signal(),
															|attempts| label_i64("Orders submitted", attempts),
														)
													email_review = Signal.map(email.signal(), |value| Str.concat("Email: ", value))
													address_review = Signal.map(address.signal(), |value| Str.concat("Address: ", value))

													cart_panel =
														Html.section(
															"Cart",
															[],
															[
																Html.paragraph("Cart editor"),
																Html.button("Use team plan", lines.on_unit(|_| team_lines)),
																Html.button("Use basic plan", lines.on_unit(|_| basic_lines)),
																Ui.each(lines.signal(), |line| line.label, render_line),
															],
														)
													delivery_panel =
														Html.section(
															"Delivery",
															[],
															[
																Html.text_input("Email", email.signal(), email.on_str(|_, value| value)),
																Html.text_input("Address", address.signal(), address.on_str(|_, value| value)),
																Html.checkbox("Accept terms", terms_signal, terms.on_bool(|_, checked| checked)),
																Html.text_s(terms_text),
															],
														)
													review_panel =
														Html.section(
															"Review",
															[],
															[
																Html.text_s(email_review),
																Html.text_s(address_review),
																Html.text_s(review_label),
																Html.action_button(
																	Signal.const_str("Place order"),
																	submit_disabled,
																	submit_count.on_unit(|current| current + 1),
																),
															],
														)

													Html.div(
														[],
														[
															Html.heading("Checkout wizard"),
															Html.button("Back", step.on_unit(prev_step)),
															Html.button("Next", step.on_unit(next_step)),
															Html.text_s(step_text),
															Ui.when(
																is_cart,
																|_| cart_panel,
																|_| {
																	Ui.when(
																		is_delivery,
																		|_| delivery_panel,
																		|_| review_panel,
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
				},
			)
		},
	)
}
