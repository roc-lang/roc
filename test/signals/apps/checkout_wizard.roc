app [BoundaryPayload, program] { pf: platform "../platform/main.roc" }

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

label_i64 : Str, I64 -> Str
label_i64 = |name, value| Str.concat(Str.concat(name, ": "), value.to_str())

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

render_line : Line => Elem.Elem
render_line = |line| {
	{ sender: add_send, receiver: add_clicks } = Reactive.Event.unit_channel!()
	{ sender: remove_send, receiver: remove_clicks } = Reactive.Event.unit_channel!()
	adds : Reactive.Event(I64)
	adds = Reactive.Event.map_unit_i64_const(add_clicks, 1)
	removes : Reactive.Event(I64)
	removes = Reactive.Event.map_unit_i64_const(remove_clicks, -1)
	quantity_events = Reactive.Event.merge(adds, removes)
	quantity : Reactive.Signal(I64)
	quantity = Reactive.Signal.fold_i64(1, quantity_events, |current, delta| current + delta)
	quantity_label = Reactive.Signal.map_i64_str(quantity, |n| label_i64("qty", n))

	Elem.div(
		[
			Elem.text(line.label),
			Elem.button({ on_click: remove_send, label: Reactive.Signal.const_str("-") }),
			Elem.label(quantity_label),
			Elem.button({ on_click: add_send, label: Reactive.Signal.const_str("+") }),
		],
	)
}

main! : () => {}
main! = || {
	{ sender: next_send, receiver: next_clicks } = Reactive.Event.unit_channel!()
	{ sender: back_send, receiver: back_clicks } = Reactive.Event.unit_channel!()
	nexts : Reactive.Event(I64)
	nexts = Reactive.Event.map_unit_i64_const(next_clicks, 1)
	backs : Reactive.Event(I64)
	backs = Reactive.Event.map_unit_i64_const(back_clicks, -1)
	step_events = Reactive.Event.merge(nexts, backs)
	step : Reactive.Signal(I64)
	step =
		Reactive.Signal.fold_i64(
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
	step_text = Reactive.Signal.map_i64_str(step, step_label)

	{ sender: terms_send, receiver: terms_clicks } = Reactive.Event.unit_channel!()
	terms : Reactive.Signal(Bool)
	terms = Reactive.Signal.fold_bool_toggle(False, terms_clicks)
	terms_text =
		Reactive.Signal.map(
			terms,
			|accepted| if accepted {
				"Terms accepted"
			} else {
				"Terms pending"
			},
		)

	{ sender: add_support_send, receiver: add_support_clicks } = Reactive.Event.unit_channel!()
	{ sender: basic_cart_send, receiver: basic_cart_clicks } = Reactive.Event.unit_channel!()
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
			[Line.make("seats", "3 seats"), Line.make("support", "Priority support")],
			cart_commands,
			|_current, next| next,
		)

	{ sender: submit_send, receiver: submit_clicks } = Reactive.Event.unit_channel!()
	submit_deltas : Reactive.Event(I64)
	submit_deltas = Reactive.Event.map_unit_i64_const(submit_clicks, 1)
	submit_count : Reactive.Signal(I64)
	submit_count = Reactive.Signal.fold_i64(0, submit_deltas, |current, delta| current + delta)
	review_label = Reactive.Signal.map_i64_str(submit_count, |attempts| Str.concat("submits: ", attempts.to_str()))

	step_panel =
		Elem.dynamic(
			step,
			|current_step| if current_step == 0 {
				Elem.div([Elem.text("Cart editor active")])
			} else if current_step == 1 {
				Elem.div(
					[
						Elem.text("Delivery preferences"),
						Elem.button({ on_click: terms_send, label: Reactive.Signal.const_str("toggle terms") }),
						Elem.label(terms_text),
					],
				)
			} else {
				Elem.div(
					[
						Elem.text("Review"),
						Elem.label(review_label),
						Elem.button({ on_click: submit_send, label: Reactive.Signal.const_str("submit") }),
					],
				)
			},
		)

	Elem.run!(
		Elem.div(
			[
				Elem.text("Checkout wizard"),
				Elem.button({ on_click: back_send, label: Reactive.Signal.const_str("back") }),
				Elem.button({ on_click: next_send, label: Reactive.Signal.const_str("next") }),
				Elem.label(step_text),
				step_panel,
				Elem.text("Cart lines"),
				Elem.button({ on_click: add_support_send, label: Reactive.Signal.const_str("add plan") }),
				Elem.button({ on_click: basic_cart_send, label: Reactive.Signal.const_str("basic") }),
				Elem.each(lines, |line| line.id, render_line),
			],
		),
	)
}

BoundaryPayload : { total : I64, item : Line, items : List(Line) }

program = {
	main!,
	init_boundary_payload: |_| {
		total: 3,
		item: Line.make("support", "Priority support"),
		items: [Line.make("seats", "3 seats"), Line.make("support", "Priority support")],
	},
	boundary_payload_total: |payload| payload.total,
	boundary_payload_item_key: |payload| payload.item.id,
	boundary_payload_item_label: |payload| payload.item.label,
	boundary_payload_items_len: |payload| List.len(payload.items),
}
