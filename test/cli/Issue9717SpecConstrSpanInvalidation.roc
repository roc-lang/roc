app [main] { pf: platform "./issue9717-platform/main.roc" }

import pf.Elem exposing [Elem]
import pf.Html
import pf.Signal
import pf.Ui

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

increment_i64 : I64 -> I64
increment_i64 = |current| current + 1

add_traffic_sample : I64 -> I64
add_traffic_sample = |current| current + 75

record_recovery : I64 -> I64
record_recovery = |current| current - 1

enqueue_jobs : I64 -> I64
enqueue_jobs = |current| current + 3

drain_jobs : I64 -> I64
drain_jobs = |current| current - 2

same_i64 : I64 -> I64
same_i64 = |current| current

toggle_bool : Bool -> Bool
toggle_bool = |value| !value

set_true : Bool -> Bool
set_true = |_| True

health_score : I64, I64 -> I64
health_score = |requests, failing| requests - failing * 10

initial_alerts : List(Str)
initial_alerts = ["Database warmup"]

incident_alerts : List(Str)
incident_alerts = ["API latency", "Job backlog"]

cleared_alerts : List(Str)
cleared_alerts = []

visible_alerts : Bool, Bool -> List(Str)
visible_alerts = |active, cleared| {
	if cleared {
		cleared_alerts
	} else if active {
		incident_alerts
	} else {
		initial_alerts
	}
}

render_alert : Str, Signal.Signal(Str) -> Elem
render_alert = |label, _alert_signal| {
	initial_count : I64
	initial_count = 0

	Ui.state(
		initial_count,
		|ack_count| {
			ack_label =
				Signal.map(
					ack_count.signal(),
					|n| concat3(label, " acknowledgements: ", n.to_str()),
				)

			Html.section(
				label,
				[],
				[
					Html.paragraph(label),
					Html.button(Str.concat("Acknowledge ", label), ack_count.on_unit(increment_i64)),
					Html.text_s(ack_label),
				],
			)
		},
	)
}

main : {} -> Elem
main = |_| {
	initial_traffic : I64
	initial_traffic = 1200
	initial_failures : I64
	initial_failures = 4
	initial_queue : I64
	initial_queue = 17
	initial_fanout : I64
	initial_fanout = 0
	initial_incident : Bool
	initial_incident = False
	initial_cleared : Bool
	initial_cleared = False

	Ui.state(
		initial_traffic,
		|traffic| {
			Ui.state(
				initial_failures,
				|failures| {
					Ui.state(
						initial_queue,
						|queue| {
							Ui.state(
								initial_fanout,
								|fanout_base| {
									Ui.state(
										initial_incident,
										|incident_active| {
											Ui.state(
												initial_cleared,
												|alerts_cleared| {
													Ui.state(
														"",
														|search| {
															traffic_signal = traffic.signal()
															failures_signal = failures.signal()
															queue_signal = queue.signal()
															incident_signal = incident_active.signal()
															traffic_label = Signal.map(traffic_signal, |n| metric_label("Requests", n))
															failure_label = Signal.map(failures_signal, |n| metric_label("Failures", n))
															queue_label = Signal.map(queue_signal, |n| metric_label("Queue", n))
															score = Signal.map2(traffic_signal, failures_signal, health_score)
															score_label =
																Signal.map2(
																	score,
																	queue_signal,
																	|value, queued| pair_label("Health score", value, "queue", queued),
																)
															fanout_signal = fanout_base.signal()
															fanout_label_a = Signal.map(fanout_signal, |n| metric_label("Fanout A", n))
															fanout_label_b = Signal.map(fanout_signal, |n| metric_label("Fanout B", n))
															fanout_label_c = Signal.map(fanout_signal, |n| metric_label("Fanout C", n))
															fanout_label_d = Signal.map(fanout_signal, |n| metric_label("Fanout D", n))
															fanout_label_e = Signal.map(fanout_signal, |n| metric_label("Fanout E", n))
															fanout_label_f = Signal.map(fanout_signal, |n| metric_label("Fanout F", n))
															fanout_label_g = Signal.map(fanout_signal, |n| metric_label("Fanout G", n))
															fanout_label_h = Signal.map(fanout_signal, |n| metric_label("Fanout H", n))
															incident_label =
																Signal.map(
																	incident_signal,
																	|active| if active {
																		"Incident active"
																	} else {
																		"All services nominal"
																	},
																)
															alerts =
																Signal.map2(
																	incident_signal,
																	alerts_cleared.signal(),
																	visible_alerts,
																)
															search_label = Signal.map(search.signal(), |value| Str.concat("Runbook search: ", value))

															Html.div(
																[],
																[
																	Html.heading("Ops dashboard"),
																	Html.section(
																		"Traffic",
																		[],
																		[
																			Html.button("Add traffic sample", traffic.on_unit(add_traffic_sample)),
																			Html.text_s(traffic_label),
																			Html.button("Record failure", failures.on_unit(increment_i64)),
																			Html.button("Record recovery", failures.on_unit(record_recovery)),
																			Html.text_s(failure_label),
																		],
																	),
																	Html.section(
																		"Queue",
																		[],
																		[
																			Html.button("Enqueue jobs", queue.on_unit(enqueue_jobs)),
																			Html.button("Drain jobs", queue.on_unit(drain_jobs)),
																			Html.text_s(queue_label),
																			Html.text_s(score_label),
																		],
																	),
																	Html.section(
																		"Incident",
																		[],
																		[
																			Html.section(
																				"Pruning",
																				[],
																				[
																					Html.button("No-op fanout", fanout_base.on_unit(same_i64)),
																					Html.text_s(fanout_label_a),
																					Html.text_s(fanout_label_b),
																					Html.text_s(fanout_label_c),
																					Html.text_s(fanout_label_d),
																					Html.text_s(fanout_label_e),
																					Html.text_s(fanout_label_f),
																					Html.text_s(fanout_label_g),
																					Html.text_s(fanout_label_h),
																				],
																			),
																			Html.button("Toggle incident", incident_active.on_unit(toggle_bool)),
																			Html.text_s(incident_label),
																			Ui.when(
																				incident_signal,
																				|_| Html.paragraph("Incident room open"),
																				|_| Html.paragraph("No active incident"),
																			),
																			Html.text_input("Runbook search", search.signal(), search.on_str(|_, value| value)),
																			Html.text_s(search_label),
																		],
																	),
																	Html.section(
																		"Alerts",
																		[],
																		[
																			Html.button("Clear alerts", alerts_cleared.on_unit(set_true)),
																			Ui.each(alerts, |label| label, render_alert),
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
								},
							)
						},
					)
				},
			)
		},
	)
}
