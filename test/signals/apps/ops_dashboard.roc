app [BoundaryPayload, program] { pf: platform "../platform/main.roc" }

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

metric_label : Str, I64 -> Str
metric_label = |name, value| Str.concat(Str.concat(name, ": "), value.to_str())

pair_label : Str, I64, Str, I64 -> Str
pair_label = |left_name, left, right_name, right| {
	left_text = metric_label(left_name, left)
	right_text = metric_label(right_name, right)
	Str.concat(Str.concat(left_text, " / "), right_text)
}

render_alert : Reactive.EventSender(Reactive.Unit), Reactive.EventSender(Reactive.Unit), Alert => Elem.Elem
render_alert = |mount_send, unmount_send, alert| {
	{ sender: ack_send, receiver: ack_clicks } = Reactive.Event.unit_channel!()
	ack_deltas : Reactive.Event(I64)
	ack_deltas = Reactive.Event.map_unit_i64_const(ack_clicks, 1)
	ack_count : Reactive.Signal(I64)
	ack_count = Reactive.Signal.fold_i64(0, ack_deltas, |current, delta| current + delta)
	ack_label = Reactive.Signal.map_i64_str(ack_count, |n| metric_label("acks", n))

	Elem.div(
		[
			Elem.lifecycle({ on_mount: mount_send, on_unmount: unmount_send }),
			Elem.text(alert.label),
			Elem.button({ on_click: ack_send, label: Reactive.Signal.const_str("ack") }),
			Elem.label(ack_label),
		],
	)
}

main! : () => {}
main! = || {
	{ sender: traffic_send, receiver: traffic_clicks } = Reactive.Event.unit_channel!()
	traffic_deltas : Reactive.Event(I64)
	traffic_deltas = Reactive.Event.map_unit_i64_const(traffic_clicks, 75)
	traffic : Reactive.Signal(I64)
	traffic = Reactive.Signal.fold_i64(1200, traffic_deltas, |current, delta| current + delta)
	traffic_label = Reactive.Signal.map_i64_str(traffic, |n| metric_label("requests", n))

	{ sender: fail_send, receiver: fail_clicks } = Reactive.Event.unit_channel!()
	{ sender: recover_send, receiver: recover_clicks } = Reactive.Event.unit_channel!()
	fail_deltas : Reactive.Event(I64)
	fail_deltas = Reactive.Event.map_unit_i64_const(fail_clicks, 1)
	recover_deltas : Reactive.Event(I64)
	recover_deltas = Reactive.Event.map_unit_i64_const(recover_clicks, -1)
	failure_events = Reactive.Event.merge(fail_deltas, recover_deltas)
	failures : Reactive.Signal(I64)
	failures = Reactive.Signal.fold_i64(4, failure_events, |current, delta| current + delta)
	failure_label = Reactive.Signal.map_i64_str(failures, |n| metric_label("failures", n))

	{ sender: queue_up_send, receiver: queue_up_clicks } = Reactive.Event.unit_channel!()
	{ sender: queue_down_send, receiver: queue_down_clicks } = Reactive.Event.unit_channel!()
	queue_up : Reactive.Event(I64)
	queue_up = Reactive.Event.map_unit_i64_const(queue_up_clicks, 3)
	queue_down : Reactive.Event(I64)
	queue_down = Reactive.Event.map_unit_i64_const(queue_down_clicks, -2)
	queue_events = Reactive.Event.merge(queue_up, queue_down)
	queue : Reactive.Signal(I64)
	queue = Reactive.Signal.fold_i64(17, queue_events, |current, delta| current + delta)
	queue_label = Reactive.Signal.map_i64_str(queue, |n| metric_label("queue", n))

	score : Reactive.Signal(I64)
	score =
		Reactive.Signal.map2_i64_i64(
			traffic,
			failures,
			|requests, failing| requests - failing * 10,
		)
	score_label =
		Reactive.Signal.map2_i64_i64_str(
			score,
			queue,
			|value, queued| pair_label("score", value, "queue", queued),
		)

	{ sender: incident_send, receiver: incident_clicks } = Reactive.Event.unit_channel!()
	incident_active : Reactive.Signal(Bool)
	incident_active = Reactive.Signal.fold_bool_toggle(False, incident_clicks)
	incident_label =
		Reactive.Signal.map(
			incident_active,
			|active| if active {
				"Incident active"
			} else {
				"All services nominal"
			},
		)

	{ sender: clear_alerts_send, receiver: clear_alerts_clicks } = Reactive.Event.unit_channel!()
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
			[Alert.make("db", "Database warmup")],
			alert_commands,
			|_current, next| next,
		)

	{ sender: alert_mount_send, receiver: alert_mounts } = Reactive.Event.unit_channel!()
	{ sender: alert_unmount_send, receiver: alert_unmounts } = Reactive.Event.unit_channel!()
	alert_mount_deltas : Reactive.Event(I64)
	alert_mount_deltas = Reactive.Event.map_unit_i64_const(alert_mounts, 1)
	alert_unmount_deltas : Reactive.Event(I64)
	alert_unmount_deltas = Reactive.Event.map_unit_i64_const(alert_unmounts, 1)
	alert_mount_count : Reactive.Signal(I64)
	alert_mount_count = Reactive.Signal.fold_i64(0, alert_mount_deltas, |current, delta| current + delta)
	alert_unmount_count : Reactive.Signal(I64)
	alert_unmount_count = Reactive.Signal.fold_i64(0, alert_unmount_deltas, |current, delta| current + delta)

	Elem.run!(
		Elem.div(
			[
				Elem.text("Ops dashboard"),
				Elem.button({ on_click: traffic_send, label: Reactive.Signal.const_str("traffic") }),
				Elem.label(traffic_label),
				Elem.button({ on_click: fail_send, label: Reactive.Signal.const_str("fail") }),
				Elem.button({ on_click: recover_send, label: Reactive.Signal.const_str("recover") }),
				Elem.label(failure_label),
				Elem.button({ on_click: queue_up_send, label: Reactive.Signal.const_str("enqueue") }),
				Elem.button({ on_click: queue_down_send, label: Reactive.Signal.const_str("drain") }),
				Elem.label(queue_label),
				Elem.text("SLO"),
				Elem.label(score_label),
				Elem.button({ on_click: incident_send, label: Reactive.Signal.const_str("toggle incident") }),
				Elem.label(incident_label),
				Elem.when(
					incident_active,
					Elem.div([Elem.text("Escalation channel open")]),
					Elem.div([Elem.text("No escalation")]),
				),
				Elem.text("Alerts"),
				Elem.button({ on_click: clear_alerts_send, label: Reactive.Signal.const_str("clear") }),
				Elem.label(Reactive.Signal.map_i64_str(alert_mount_count, |n| metric_label("mounted", n))),
				Elem.label(Reactive.Signal.map_i64_str(alert_unmount_count, |n| metric_label("unmounted", n))),
				Elem.each(alerts, |alert| alert.id, |alert| render_alert(alert_mount_send, alert_unmount_send, alert)),
			],
		),
	)
}

BoundaryPayload : { total : I64, item : Alert, items : List(Alert) }

program = {
	main!,
	init_boundary_payload: |_| {
		total: 42,
		item: Alert.make("api", "API latency"),
		items: [Alert.make("api", "API latency"), Alert.make("jobs", "Job backlog")],
	},
	boundary_payload_total: |payload| payload.total,
	boundary_payload_item_key: |payload| payload.item.id,
	boundary_payload_item_label: |payload| payload.item.label,
	boundary_payload_items_len: |payload| List.len(payload.items),
}
