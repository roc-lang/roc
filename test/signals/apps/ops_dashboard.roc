app [main] { pf: platform "../platform/main.roc" }

import pf.Elem
import pf.NodeValue exposing [NodeValue]
import pf.Reactive

Alert := { id : Str, label : Str }.{
	make : Str, Str -> Alert
	make = |id, label| { id, label }

	encode : Alert, NodeValue -> Try(NodeValue, [])
	encode = |alert, fmt| [alert.id, alert.label].encode(fmt)

	decode : NodeValue, NodeValue -> (Try(Alert, [TypeMismatch]), NodeValue)
	decode = |nv, fmt| {
		(result, rest) = NodeValue.decode_list(fmt, nv, |source, f| Str.decode(source, f))
		alert_result = 
			match result {
				Ok(fields) =>
					match (List.get(fields, 0), List.get(fields, 1)) {
						(Ok(id), Ok(label)) => Ok(Alert.make(id, label))
						_ => Err(TypeMismatch)
					}

				Err(err) => Err(err)
			}

		(alert_result, rest)
	}
}

concat3 : Str, Str, Str -> Str
concat3 = |a, b, c| Str.concat(Str.concat(a, b), c)

metric_label : Str, I64 -> Str
metric_label = |name, value| concat3(name, ": ", value.to_str())

pair_label : Str, I64, Str, I64 -> Str
pair_label = |left_name, left, right_name, right| {
	left_text = metric_label(left_name, left)
	right_text = metric_label(right_name, right)
	concat3(left_text, " / ", right_text)
}

render_alert : Alert -> Elem.Elem
render_alert = |alert| {
	ack_key = Str.concat("alert_ack_click:", alert.id)
	ack_count_key = Str.concat("alert_ack_count:", alert.id)
	{ sender: ack_send, receiver: ack_clicks } = Reactive.Event.unit_channel(ack_key)
	ack_deltas : Reactive.Event(I64)
	ack_deltas = Reactive.Event.map_unit_i64_const(ack_clicks, 1)
	ack_count : Reactive.Signal(I64)
	ack_count = Reactive.Signal.fold_i64(ack_count_key, 0, ack_deltas, |current, delta| current + delta)
	ack_label = 
		Reactive.Signal.map_i64_str_keyed(
			Str.concat("alert_ack_label:", alert.id),
			ack_count,
			|n| concat3(alert.label, " acknowledgements: ", n.to_str()),
		)

	Elem.section(
		alert.label,
		[
			Elem.paragraph(alert.label),
			Elem.action_button(
				{
					on_click: ack_send,
					label: Reactive.Signal.const_str(Str.concat("Acknowledge ", alert.label)),
					disabled: Reactive.Signal.const_bool(False),
				},
			),
			Elem.label(ack_label),
		],
	)
}

main : {} -> Elem.Elem
main = |_| {
	{ sender: traffic_send, receiver: traffic_clicks } = Reactive.Event.unit_channel("traffic_click")
	traffic_deltas : Reactive.Event(I64)
	traffic_deltas = Reactive.Event.map_unit_i64_const(traffic_clicks, 75)
	traffic : Reactive.Signal(I64)
	traffic = Reactive.Signal.fold_i64("traffic", 1200, traffic_deltas, |current, delta| current + delta)
	traffic_label = Reactive.Signal.map_i64_str_keyed("traffic_label", traffic, |n| metric_label("Requests", n))

	{ sender: fail_send, receiver: fail_clicks } = Reactive.Event.unit_channel("fail_click")
	{ sender: recover_send, receiver: recover_clicks } = Reactive.Event.unit_channel("recover_click")
	fail_deltas : Reactive.Event(I64)
	fail_deltas = Reactive.Event.map_unit_i64_const(fail_clicks, 1)
	recover_deltas : Reactive.Event(I64)
	recover_deltas = Reactive.Event.map_unit_i64_const(recover_clicks, -1)
	failure_events = Reactive.Event.merge(fail_deltas, recover_deltas)
	failures : Reactive.Signal(I64)
	failures = Reactive.Signal.fold_i64("failures", 4, failure_events, |current, delta| current + delta)
	failure_label = Reactive.Signal.map_i64_str_keyed("failure_label", failures, |n| metric_label("Failures", n))

	{ sender: queue_up_send, receiver: queue_up_clicks } = Reactive.Event.unit_channel("queue_up_click")
	{ sender: queue_down_send, receiver: queue_down_clicks } = Reactive.Event.unit_channel("queue_down_click")
	queue_up : Reactive.Event(I64)
	queue_up = Reactive.Event.map_unit_i64_const(queue_up_clicks, 3)
	queue_down : Reactive.Event(I64)
	queue_down = Reactive.Event.map_unit_i64_const(queue_down_clicks, -2)
	queue_events = Reactive.Event.merge(queue_up, queue_down)
	queue : Reactive.Signal(I64)
	queue = Reactive.Signal.fold_i64("queue", 17, queue_events, |current, delta| current + delta)
	queue_label = Reactive.Signal.map_i64_str_keyed("queue_label", queue, |n| metric_label("Queue", n))

	score : Reactive.Signal(I64)
	score = 
		Reactive.Signal.map2_i64_i64_keyed(
			"score",
			traffic,
			failures,
			|requests, failing| requests - failing * 10,
		)
	score_label = 
		Reactive.Signal.map2_i64_i64_str_keyed(
			"score_label",
			score,
			queue,
			|value, queued| pair_label("Health score", value, "queue", queued),
		)

	{ sender: incident_send, receiver: incident_clicks } = Reactive.Event.unit_channel("incident_click")
	incident_active : Reactive.Signal(Bool)
	incident_active = Reactive.Signal.fold_bool_toggle("incident_active", False, incident_clicks)
	incident_label = 
		Reactive.Signal.map_keyed(
			"incident_label",
			incident_active,
			|active| if active {
				"Incident active"
			} else {
				"All services nominal"
			},
		)

	{ sender: clear_alerts_send, receiver: clear_alerts_clicks } = Reactive.Event.unit_channel("clear_alerts_click")
	open_alerts : Reactive.Event(List(Alert))
	open_alerts = 
		Reactive.Event.map(
			incident_clicks,
			|_| [Alert.make("api", "API latency"), Alert.make("jobs", "Job backlog")],
		)
	clear_alerts : Reactive.Event(List(Alert))
	clear_alerts = Reactive.Event.map(clear_alerts_clicks, |_| [])
	alert_commands = Reactive.Event.merge(open_alerts, clear_alerts)
	alerts : Reactive.Signal(List(Alert))
	alerts = 
		Reactive.Signal.fold(
			"alerts",
			[Alert.make("db", "Database warmup")],
			alert_commands,
			|_current, next| next,
		)

	{ sender: search_send, receiver: search_changes } = Reactive.Event.str_channel("search_change")
	search : Reactive.Signal(Str)
	search = Reactive.Signal.hold("search", "", search_changes)
	search_label = Reactive.Signal.map_keyed("search_label", search, |value| Str.concat("Runbook search: ", value))

	Elem.div(
		[
			Elem.heading("Ops dashboard"),
			Elem.section(
				"Traffic",
				[
					Elem.action_button(
						{
							on_click: traffic_send,
							label: Reactive.Signal.const_str("Add traffic sample"),
							disabled: Reactive.Signal.const_bool(False),
						},
					),
					Elem.label(traffic_label),
					Elem.action_button(
						{
							on_click: fail_send,
							label: Reactive.Signal.const_str("Record failure"),
							disabled: Reactive.Signal.const_bool(False),
						},
					),
					Elem.action_button(
						{
							on_click: recover_send,
							label: Reactive.Signal.const_str("Record recovery"),
							disabled: Reactive.Signal.const_bool(False),
						},
					),
					Elem.label(failure_label),
				],
			),
			Elem.section(
				"Queue",
				[
					Elem.action_button(
						{
							on_click: queue_up_send,
							label: Reactive.Signal.const_str("Enqueue jobs"),
							disabled: Reactive.Signal.const_bool(False),
						},
					),
					Elem.action_button(
						{
							on_click: queue_down_send,
							label: Reactive.Signal.const_str("Drain jobs"),
							disabled: Reactive.Signal.const_bool(False),
						},
					),
					Elem.label(queue_label),
					Elem.label(score_label),
				],
			),
			Elem.section(
				"Incident",
				[
					Elem.action_button(
						{
							on_click: incident_send,
							label: Reactive.Signal.const_str("Toggle incident"),
							disabled: Reactive.Signal.const_bool(False),
						},
					),
					Elem.label(incident_label),
					Elem.when(
						incident_active,
						Elem.paragraph("Incident room open"),
						Elem.paragraph("No active incident"),
					),
					Elem.text_input(
						{
							label: "Runbook search",
							value: search,
							on_input: search_send,
							disabled: Reactive.Signal.const_bool(False),
						},
					),
					Elem.label(search_label),
				],
			),
			Elem.section(
				"Alerts",
				[
					Elem.action_button(
						{
							on_click: clear_alerts_send,
							label: Reactive.Signal.const_str("Clear alerts"),
							disabled: Reactive.Signal.const_bool(False),
						},
					),
					Elem.each(alerts, |alert| alert.id, render_alert),
				],
			),
		],
	)
}
