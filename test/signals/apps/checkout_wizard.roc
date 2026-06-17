app [main] { pf: platform "../platform/main.roc" }

import pf.Elem
import pf.NodeValue exposing [NodeValue]
import pf.Reactive

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

render_line : Line -> Elem.Elem
render_line = |line| {
	{ sender: add_send, receiver: add_clicks } = Reactive.Event.unit_channel(Str.concat("line_add_click:", line.id))
	{ sender: remove_send, receiver: remove_clicks } = Reactive.Event.unit_channel(Str.concat("line_remove_click:", line.id))
	adds : Reactive.Event(I64)
	adds = Reactive.Event.map_unit_i64_const(add_clicks, 1)
	removes : Reactive.Event(I64)
	removes = Reactive.Event.map_unit_i64_const(remove_clicks, -1)
	quantity_events = Reactive.Event.merge(adds, removes)
	quantity : Reactive.Signal(I64)
	quantity = Reactive.Signal.fold_i64(
		Str.concat("line_quantity:", line.id),
		1,
		quantity_events,
		|current, delta| {
			next = current + delta
			if next < 0 {
				0
			} else {
				next
			}
		},
	)
	quantity_label = 
		Reactive.Signal.map_i64_str_keyed(
			Str.concat("line_quantity_label:", line.id),
			quantity,
			|n| concat3(line.label, " quantity: ", n.to_str()),
		)

	Elem.section(
		line.label,
		[
			Elem.paragraph(line.label),
			Elem.action_button(
				{
					on_click: remove_send,
					label: Reactive.Signal.const_str(Str.concat("Decrease ", line.label)),
					disabled: Reactive.Signal.const_bool(False),
				},
			),
			Elem.label(quantity_label),
			Elem.action_button(
				{
					on_click: add_send,
					label: Reactive.Signal.const_str(Str.concat("Increase ", line.label)),
					disabled: Reactive.Signal.const_bool(False),
				},
			),
		],
	)
}

main : {} -> Elem.Elem
main = |_| {
	{ sender: next_send, receiver: next_clicks } = Reactive.Event.unit_channel("next_click")
	{ sender: back_send, receiver: back_clicks } = Reactive.Event.unit_channel("back_click")
	nexts : Reactive.Event(I64)
	nexts = Reactive.Event.map_unit_i64_const(next_clicks, 1)
	backs : Reactive.Event(I64)
	backs = Reactive.Event.map_unit_i64_const(back_clicks, -1)
	step_events = Reactive.Event.merge(nexts, backs)
	step : Reactive.Signal(I64)
	step = 
		Reactive.Signal.fold_i64(
			"step",
			0,
			step_events,
			|current, delta| {
				next = current + delta
				if next < 0 {
					0
				} else if next > 2 {
					2
				} else {
					next
				}
			},
		)
	step_text = Reactive.Signal.map_i64_str_keyed("step_text", step, step_label)

	{ sender: email_send, receiver: email_changes } = Reactive.Event.channel("email_change")
	email : Reactive.Signal(Str)
	email = Reactive.Signal.hold("email", "", email_changes)
	{ sender: address_send, receiver: address_changes } = Reactive.Event.channel("address_change")
	address : Reactive.Signal(Str)
	address = Reactive.Signal.hold("address", "", address_changes)
	{ sender: terms_send, receiver: terms_changes } = Reactive.Event.channel("terms_change")
	terms : Reactive.Signal(Bool)
	terms = Reactive.Signal.hold("terms", False, terms_changes)
	terms_text = 
		Reactive.Signal.map_keyed(
			"terms_text",
			terms,
			|accepted| if accepted {
				"Terms accepted"
			} else {
				"Terms pending"
			},
	)
	submit_disabled : Reactive.Signal(Bool)
	submit_disabled = 
		Reactive.Signal.map_keyed(
			"submit_disabled",
			terms,
			|accepted| if accepted {
				False
			} else {
				True
			},
		)

	{ sender: add_support_send, receiver: add_support_clicks } = Reactive.Event.unit_channel("add_support_click")
	{ sender: basic_cart_send, receiver: basic_cart_clicks } = Reactive.Event.unit_channel("basic_cart_click")
	support_cart : Reactive.Event(List(Line))
	support_cart = 
		Reactive.Event.map(
			add_support_clicks,
			|_| [Line.make("seats", "3 seats"), Line.make("support", "Priority support"), Line.make("audit", "Audit log export")],
		)
	basic_cart : Reactive.Event(List(Line))
	basic_cart = Reactive.Event.map(basic_cart_clicks, |_| [Line.make("seats", "3 seats")])
	cart_commands = Reactive.Event.merge(support_cart, basic_cart)
	lines : Reactive.Signal(List(Line))
	lines = 
		Reactive.Signal.fold(
			"lines",
			[Line.make("seats", "3 seats"), Line.make("support", "Priority support")],
			cart_commands,
			|_current, next| next,
		)

	{ sender: submit_send, receiver: submit_clicks } = Reactive.Event.unit_channel("submit_click")
	submit_deltas : Reactive.Event(I64)
	submit_deltas = Reactive.Event.map_unit_i64_const(submit_clicks, 1)
	submit_count : Reactive.Signal(I64)
	submit_count = Reactive.Signal.fold_i64("submit_count", 0, submit_deltas, |current, delta| current + delta)
	review_label = Reactive.Signal.map_i64_str_keyed("review_label", submit_count, |attempts| label_i64("Orders submitted", attempts))
	email_review = Reactive.Signal.map_keyed("email_review", email, |value| Str.concat("Email: ", value))
	address_review = Reactive.Signal.map_keyed("address_review", address, |value| Str.concat("Address: ", value))

	step_panel = 
		Elem.dynamic_keyed(
			step,
			|current_step| current_step.to_str(),
			|current_step| if current_step == 0 {
				Elem.section(
					"Cart",
					[
						Elem.paragraph("Cart editor"),
						Elem.action_button(
							{
								on_click: add_support_send,
								label: Reactive.Signal.const_str("Use team plan"),
								disabled: Reactive.Signal.const_bool(False),
							},
						),
						Elem.action_button(
							{
								on_click: basic_cart_send,
								label: Reactive.Signal.const_str("Use basic plan"),
								disabled: Reactive.Signal.const_bool(False),
							},
						),
						Elem.each(lines, |line| line.id, render_line),
					],
				)
			} else if current_step == 1 {
				Elem.section(
					"Delivery",
					[
						Elem.text_input(
							{
								label: "Email",
								value: email,
								on_input: email_send,
								disabled: Reactive.Signal.const_bool(False),
							},
						),
						Elem.text_input(
							{
								label: "Address",
								value: address,
								on_input: address_send,
								disabled: Reactive.Signal.const_bool(False),
							},
						),
						Elem.checkbox(
							{
								label: "Accept terms",
								checked: terms,
								on_check: terms_send,
								disabled: Reactive.Signal.const_bool(False),
							},
						),
						Elem.label(terms_text),
					],
				)
			} else {
				Elem.section(
					"Review",
					[
						Elem.label(email_review),
						Elem.label(address_review),
						Elem.label(review_label),
						Elem.action_button(
							{
								on_click: submit_send,
								label: Reactive.Signal.const_str("Place order"),
								disabled: submit_disabled,
							},
						),
					],
				)
			},
		)

	Elem.div(
		[
			Elem.heading("Checkout wizard"),
			Elem.action_button(
				{
					on_click: back_send,
					label: Reactive.Signal.const_str("Back"),
					disabled: Reactive.Signal.const_bool(False),
				},
			),
			Elem.action_button(
				{
					on_click: next_send,
					label: Reactive.Signal.const_str("Next"),
					disabled: Reactive.Signal.const_bool(False),
				},
			),
			Elem.label(step_text),
			step_panel,
		],
	)
}
