app [main] { pf: platform "../platform/main.roc" }

import pf.Elem exposing [Elem]
import pf.Html
import pf.Http
import pf.Signal
import pf.Ui

Dashboard : {
	alert_a_age_min : U64,
	alert_b_age_min : U64,
	api_state_code : U64,
	billing_state_code : U64,
	db_write_rpm : U64,
	error_bar_code : U64,
	error_permille : U64,
	healthy_services : U64,
	incidents : U64,
	ingress_bar_code : U64,
	job_a_age_min : U64,
	job_a_id : U64,
	job_a_progress : U64,
	job_b_age_min : U64,
	job_b_id : U64,
	job_c_age_min : U64,
	job_c_id : U64,
	job_c_progress : U64,
	latency_bar_code : U64,
	latency_ms : U64,
	oldest_job_min : U64,
	overall_code : U64,
	queue_depth : U64,
	requests_per_minute : U64,
	running_jobs : U64,
	tone_code : U64,
	total_services : U64,
	updated_version : U64,
	webhook_rpm : U64,
	worker_state_code : U64,
}

ParseErr : [OpsBadProtocol, OpsBadNumber]

DashboardState : [
	DashboardLoading,
	DashboardReady(Dashboard),
	DashboardRequestFailed(Str),
	DashboardDecodeFailed(ParseErr),
]

concat3 : Str, Str, Str -> Str
concat3 = |a, b, c| Str.concat(Str.concat(a, b), c)

concat4 : Str, Str, Str, Str -> Str
concat4 = |a, b, c, d| Str.concat(concat3(a, b, c), d)

join_with : Str, List(Str) -> Str
join_with = |separator, parts|
	List.fold(
		parts,
		"",
		|acc, part|
			if Str.is_empty(acc) {
				part
			} else {
				concat3(acc, separator, part)
			},
	)

join_lines : List(Str) -> Str
join_lines = |lines| join_with("\n", lines)

increment_u64 : U64 -> U64
increment_u64 = |current| current + 1

decode_dashboard : Str -> DashboardState
decode_dashboard = |body|
	match parse_dashboard(body) {
		Ok(dashboard) => DashboardReady(dashboard)
		Err(err) => DashboardDecodeFailed(err)
	}

parse_error_text : ParseErr -> Str
parse_error_text = |err|
	match err {
		OpsBadProtocol => "Response did not match the dashboard protocol"
		OpsBadNumber => "Response had an invalid dashboard number"
	}

state_text : DashboardState, (Dashboard -> Str) -> Str
state_text = |state, render|
	match state {
		DashboardLoading => "Loading dashboard data..."
		DashboardReady(dashboard) => render(dashboard)
		DashboardRequestFailed(err) => concat3("Request failed: ", err, "")
		DashboardDecodeFailed(err) => parse_error_text(err)
	}

parse_dashboard : Str -> Try(Dashboard, ParseErr)
parse_dashboard = |body| {
	updated_version = parse_u64_field(Str.to_utf8(body), "updated_version")?
	overall_code = parse_u64_field(updated_version.rest, "overall_code")?
	tone_code = parse_u64_field(overall_code.rest, "tone_code")?
	incidents = parse_u64_field(tone_code.rest, "incidents")?
	requests_per_minute = parse_u64_field(incidents.rest, "requests_per_minute")?
	error_permille = parse_u64_field(requests_per_minute.rest, "error_permille")?
	latency_ms = parse_u64_field(error_permille.rest, "latency_ms")?
	webhook_rpm = parse_u64_field(latency_ms.rest, "webhook_rpm")?
	db_write_rpm = parse_u64_field(webhook_rpm.rest, "db_write_rpm")?
	ingress_bar_code = parse_u64_field(db_write_rpm.rest, "ingress_bar_code")?
	latency_bar_code = parse_u64_field(ingress_bar_code.rest, "latency_bar_code")?
	error_bar_code = parse_u64_field(latency_bar_code.rest, "error_bar_code")?
	queue_depth = parse_u64_field(error_bar_code.rest, "queue_depth")?
	running_jobs = parse_u64_field(queue_depth.rest, "running_jobs")?
	oldest_job_min = parse_u64_field(running_jobs.rest, "oldest_job_min")?
	job_a_id = parse_u64_field(oldest_job_min.rest, "job_a_id")?
	job_a_progress = parse_u64_field(job_a_id.rest, "job_a_progress")?
	job_a_age_min = parse_u64_field(job_a_progress.rest, "job_a_age_min")?
	job_b_id = parse_u64_field(job_a_age_min.rest, "job_b_id")?
	job_b_age_min = parse_u64_field(job_b_id.rest, "job_b_age_min")?
	job_c_id = parse_u64_field(job_b_age_min.rest, "job_c_id")?
	job_c_progress = parse_u64_field(job_c_id.rest, "job_c_progress")?
	job_c_age_min = parse_u64_field(job_c_progress.rest, "job_c_age_min")?
	alert_a_age_min = parse_u64_field(job_c_age_min.rest, "alert_a_age_min")?
	alert_b_age_min = parse_u64_field(alert_a_age_min.rest, "alert_b_age_min")?
	healthy_services = parse_u64_field(alert_b_age_min.rest, "healthy_services")?
	total_services = parse_u64_field(healthy_services.rest, "total_services")?
	api_state_code = parse_u64_field(total_services.rest, "api_state_code")?
	worker_state_code = parse_u64_field(api_state_code.rest, "worker_state_code")?
	billing_state_code = parse_u64_field(worker_state_code.rest, "billing_state_code")?

	if !List.is_empty(billing_state_code.rest) {
		return Err(OpsBadProtocol)
	}

	Ok(
		{
			alert_a_age_min: alert_a_age_min.value,
			alert_b_age_min: alert_b_age_min.value,
			api_state_code: api_state_code.value,
			billing_state_code: billing_state_code.value,
			db_write_rpm: db_write_rpm.value,
			error_bar_code: error_bar_code.value,
			error_permille: error_permille.value,
			healthy_services: healthy_services.value,
			incidents: incidents.value,
			ingress_bar_code: ingress_bar_code.value,
			job_a_age_min: job_a_age_min.value,
			job_a_id: job_a_id.value,
			job_a_progress: job_a_progress.value,
			job_b_age_min: job_b_age_min.value,
			job_b_id: job_b_id.value,
			job_c_age_min: job_c_age_min.value,
			job_c_id: job_c_id.value,
			job_c_progress: job_c_progress.value,
			latency_bar_code: latency_bar_code.value,
			latency_ms: latency_ms.value,
			oldest_job_min: oldest_job_min.value,
			overall_code: overall_code.value,
			queue_depth: queue_depth.value,
			requests_per_minute: requests_per_minute.value,
			running_jobs: running_jobs.value,
			tone_code: tone_code.value,
			total_services: total_services.value,
			updated_version: updated_version.value,
			webhook_rpm: webhook_rpm.value,
			worker_state_code: worker_state_code.value,
		},
	)
}

parse_u64_field : List(U8), Str -> Try({ value : U64, rest : List(U8) }, ParseErr)
parse_u64_field = |raw, name| {
	after_name = drop_prefix_bytes(raw, Str.to_utf8(name))?
	after_equals = drop_byte(after_name, 61)?
	parts = take_line(after_equals)?
	value = bytes_to_u64(parts.value)?

	Ok({ value, rest: parts.rest })
}

take_line : List(U8) -> Try({ value : List(U8), rest : List(U8) }, ParseErr)
take_line = |raw| {
	var $remaining = raw
	var $value = []

	while True {
		match List.first($remaining) {
			Ok(byte) => {
				if byte == 10 {
					return Ok({ value: $value, rest: List.drop_first($remaining, 1) })
				}

				$value = List.append($value, byte)
				$remaining = List.drop_first($remaining, 1)
			}

			Err(_) => return Ok({ value: $value, rest: [] })
		}
	}
}

drop_prefix_bytes : List(U8), List(U8) -> Try(List(U8), ParseErr)
drop_prefix_bytes = |raw, prefix| {
	var $remaining = raw
	var $prefix = prefix

	while True {
		match List.first($prefix) {
			Ok(expected) => {
				match List.first($remaining) {
					Ok(actual) =>
						if actual == expected {
							$remaining = List.drop_first($remaining, 1)
							$prefix = List.drop_first($prefix, 1)
						} else {
							return Err(OpsBadProtocol)
						}

					Err(_) => return Err(OpsBadProtocol)
				}
			}

			Err(_) => return Ok($remaining)
		}
	}
}

drop_byte : List(U8), U8 -> Try(List(U8), ParseErr)
drop_byte = |raw, expected|
	match List.first(raw) {
		Ok(actual) =>
			if actual == expected {
				Ok(List.drop_first(raw, 1))
			} else {
				Err(OpsBadProtocol)
			}

		Err(_) => Err(OpsBadProtocol)
	}

bytes_to_u64 : List(U8) -> Try(U64, ParseErr)
bytes_to_u64 = |bytes| {
	if List.is_empty(bytes) {
		return Err(OpsBadNumber)
	}

	var $remaining = bytes
	var $value = 0

	while True {
		match List.first($remaining) {
			Ok(byte) =>
				if (byte >= 48) and (byte <= 57) {
					$value = $value * 10 + U8.to_u64(byte - 48)
					$remaining = List.drop_first($remaining, 1)
				} else {
					return Err(OpsBadNumber)
				}

			Err(_) => return Ok($value)
		}
	}
}

overall_text : U64 -> Str
overall_text = |code|
	if code == 1 {
		"Degraded"
	} else {
		"Nominal"
	}

overall_detail_text : Dashboard -> Str
overall_detail_text = |dashboard|
	join_with(
		" | ",
		[
			concat3(dashboard.incidents.to_str(), " active incidents", ""),
			concat4(dashboard.healthy_services.to_str(), "/", dashboard.total_services.to_str(), " services healthy"),
			concat3("server tick ", dashboard.updated_version.to_str(), ""),
		],
	)

tone_text : U64 -> Str
tone_text = |code|
	if code == 2 {
		"bad"
	} else if code == 1 {
		"warn"
	} else {
		"good"
	}

health_state_text : U64 -> Str
health_state_text = |code|
	if code == 2 {
		"degraded"
	} else if code == 1 {
		"watch"
	} else {
			"ok"
	}

state_value : DashboardState, (Dashboard -> Str) -> Str
state_value = |state, render|
	match state {
		DashboardLoading => "--"
		DashboardReady(dashboard) => render(dashboard)
		DashboardRequestFailed(_) => "request failed"
		DashboardDecodeFailed(_) => "decode failed"
	}

state_detail : DashboardState, (Dashboard -> Str) -> Str
state_detail = |state, render|
	match state {
		DashboardLoading => "Starting first server request"
		DashboardReady(dashboard) => render(dashboard)
		DashboardRequestFailed(err) => concat3("HTTP task failed: ", err, "")
		DashboardDecodeFailed(err) => parse_error_text(err)
	}

state_is_incident : DashboardState -> Bool
state_is_incident = |state|
	match state {
		DashboardReady(dashboard) => dashboard.overall_code == 1
		_ => False
	}

bar_text : U64 -> Str
bar_text = |code|
	if code == 0 {
		"###---------"
	} else if code == 1 {
		"####--------"
	} else if code == 2 {
		"#####-------"
	} else if code == 3 {
		"######------"
	} else if code == 4 {
		"#######-----"
	} else if code == 5 {
		"########----"
	} else if code == 6 {
		"#########---"
	} else if code == 7 {
		"##########--"
	} else {
		"###########-"
	}

permille_text : U64 -> Str
permille_text = |value| concat4((value // 10).to_str(), ".", (value % 10).to_str(), "%")

job_id_text : U64 -> Str
job_id_text = |id| concat3("job-", id.to_str(), "")

metric_text : U64, Str -> Str
metric_text = |value, suffix| concat3(value.to_str(), suffix, "")

traffic_detail_text : Dashboard -> Str
traffic_detail_text = |dashboard|
	join_with(
		" | ",
		[
			concat3("p95 ", dashboard.latency_ms.to_str(), " ms"),
			concat3("errors ", permille_text(dashboard.error_permille), ""),
			concat3("webhooks ", dashboard.webhook_rpm.to_str(), " rpm"),
		],
	)

queue_detail_text : Dashboard -> Str
queue_detail_text = |dashboard|
	join_with(
		" | ",
		[
			concat3(dashboard.running_jobs.to_str(), " running", ""),
			concat3("oldest ", dashboard.oldest_job_min.to_str(), "m"),
			"worker pool primary",
		],
	)

services_detail_text : Dashboard -> Str
services_detail_text = |dashboard|
	join_with(
		" | ",
		[
			concat4(dashboard.healthy_services.to_str(), "/", dashboard.total_services.to_str(), " healthy"),
			concat3("api ", health_state_text(dashboard.api_state_code), ""),
			concat3("workers ", health_state_text(dashboard.worker_state_code), ""),
		],
	)

last_updated_text : Dashboard -> Str
last_updated_text = |dashboard| concat3("Last server update: version ", dashboard.updated_version.to_str(), "")

status_rollup_text : Dashboard -> Str
status_rollup_text = |dashboard|
	join_lines(
		[
			join_with("  ", ["region", "usw2", "traffic", concat3(dashboard.requests_per_minute.to_str(), " rpm", ""), "tone", tone_text(dashboard.tone_code)]),
			join_with("  ", ["queue", concat3(dashboard.queue_depth.to_str(), " waiting", ""), concat3(dashboard.running_jobs.to_str(), " running", ""), concat3("oldest ", dashboard.oldest_job_min.to_str(), "m")]),
			join_with("  ", ["services", concat4(dashboard.healthy_services.to_str(), "/", dashboard.total_services.to_str(), " healthy"), "database ok", concat3("billing ", health_state_text(dashboard.billing_state_code), "")]),
		],
	)

traffic_text : Dashboard -> Str
traffic_text = |dashboard|
	join_lines(
		[
			"metric          current        trend         note",
			join_with("  ", ["ingress        ", concat3(dashboard.requests_per_minute.to_str(), " rpm", ""), bar_text(dashboard.ingress_bar_code), "edge + api"]),
			join_with("  ", ["api p95        ", concat3(dashboard.latency_ms.to_str(), " ms", ""), bar_text(dashboard.latency_bar_code), "target 120 ms"]),
			join_with("  ", ["error rate     ", permille_text(dashboard.error_permille), bar_text(dashboard.error_bar_code), "5m rolling"]),
			join_with("  ", ["webhook fanout ", concat3(dashboard.webhook_rpm.to_str(), " rpm", ""), "#######-----", "retry pool steady"]),
			join_with("  ", ["db writes      ", concat3(dashboard.db_write_rpm.to_str(), " rpm", ""), "######------", "replica lag tracked"]),
		],
	)

job_line : U64, Str, U64, Str, U64, Str -> Str
job_line = |id, state, progress, owner, age_min, title|
	join_with("  ", [job_id_text(id), state, concat3(progress.to_str(), "%", ""), owner, concat3(age_min.to_str(), "m", ""), title])

jobs_text : Dashboard -> Str
jobs_text = |dashboard|
	join_lines(
		[
			"job id   state     progress  owner           age  title",
			job_line(dashboard.job_a_id, "running ", dashboard.job_a_progress, "workers/search ", dashboard.job_a_age_min, "Rebuild search index"),
			job_line(dashboard.job_b_id, "queued  ", 0, "billing        ", dashboard.job_b_age_min, "Backfill billing events"),
			job_line(dashboard.job_c_id, "running ", dashboard.job_c_progress, "compliance     ", dashboard.job_c_age_min, "Export audit archive"),
		],
	)

alerts_text : Dashboard -> Str
alerts_text = |dashboard|
	if dashboard.overall_code == 1 {
		join_lines(
			[
				"severity  service       state    age  summary",
				join_with("  ", ["critical ", "payments-api", "active ", concat3(dashboard.alert_a_age_min.to_str(), "m", ""), "Checkout latency above SLO"]),
				join_with("  ", ["warning  ", "workers     ", "active ", concat3(dashboard.alert_b_age_min.to_str(), "m", ""), "Queue age approaching cap"]),
			],
		)
	} else {
		join_lines(
			[
				"severity  service       state       age  summary",
				join_with("  ", ["warning  ", "workers     ", "monitoring", concat3(dashboard.alert_a_age_min.to_str(), "m", ""), "Retry queue elevated"]),
				join_with("  ", ["info     ", "payments-api", "recovering", concat3(dashboard.alert_b_age_min.to_str(), "m", ""), "Error budget burn below 1x"]),
			],
		)
	}

health_text : Dashboard -> Str
health_text = |dashboard|
	join_lines(
		[
			"service   state     capacity     detail",
			join_with("  ", ["edge     ", "ok       ", "8 pods      ", "all regions serving"]),
			join_with("  ", ["api      ", health_state_text(dashboard.api_state_code), "12 pods     ", concat3("p95 ", dashboard.latency_ms.to_str(), " ms")]),
			join_with("  ", ["workers  ", health_state_text(dashboard.worker_state_code), "24 slots    ", concat3(dashboard.queue_depth.to_str(), " queued jobs", "")]),
			join_with("  ", ["database ", "ok       ", "2 writers   ", "failover warm"]),
			join_with("  ", ["billing  ", health_state_text(dashboard.billing_state_code), "6 pods      ", "webhooks draining"]),
		],
	)

page_class : Str
page_class = "min-h-screen bg-zinc-50 text-zinc-950"

shell_class : Str
shell_class = "mx-auto grid max-w-7xl gap-4 px-4 py-4 sm:px-6 lg:px-8"

toolbar_class : Str
toolbar_class = "flex flex-wrap items-center justify-between gap-3 border-b border-zinc-200 bg-white px-4 py-3 sm:px-5"

toolbar_text_class : Str
toolbar_text_class = "grid gap-1"

toolbar_status_class : Str
toolbar_status_class = "text-sm text-zinc-600"

metric_grid_class : Str
metric_grid_class = "grid gap-3 sm:grid-cols-2 xl:grid-cols-4"

panel_grid_class : Str
panel_grid_class = "grid gap-4 lg:grid-cols-2"

panel_class : Str
panel_class = "grid gap-3 rounded-md border border-zinc-200 bg-white p-4"

alert_panel_class : Str
alert_panel_class = "grid gap-3 rounded-md border border-amber-300 bg-amber-50 p-4"

critical_panel_class : Str
critical_panel_class = "grid gap-3 rounded-md border border-red-300 bg-red-50 p-4"

metric_card_class : Str
metric_card_class = "grid min-h-32 gap-2 rounded-md border border-zinc-200 bg-white p-4"

metric_label_class : Str
metric_label_class = "text-xs font-semibold uppercase tracking-normal text-zinc-500"

metric_value_class : Str
metric_value_class = "text-2xl font-semibold text-zinc-950"

metric_detail_class : Str
metric_detail_class = "text-sm leading-5 text-zinc-600"

pre_class : Str
pre_class = "overflow-x-auto whitespace-pre font-mono text-xs leading-5 text-zinc-800"

primary_button_class : Str
primary_button_class = "rounded-md border border-sky-700 bg-sky-700 px-3 py-2 text-sm font-medium text-white hover:border-sky-800 hover:bg-sky-800"

render_panel : Str, Str, Signal.Signal(Str) -> Elem
render_panel = |region_label, heading, body| {
	Html.section_c(
		region_label,
		panel_class,
		[
			Html.heading_c(heading, "text-sm font-semibold uppercase tracking-normal text-zinc-600"),
			Html.pre_s_c(body, pre_class),
		],
	)
}

render_panel_with_class : Str, Str, Str, Signal.Signal(Str) -> Elem
render_panel_with_class = |region_label, heading, classes, body| {
	Html.section_c(
		region_label,
		classes,
		[
			Html.heading_c(heading, "text-sm font-semibold uppercase tracking-normal text-zinc-600"),
			Html.pre_s_c(body, pre_class),
		],
	)
}

render_metric : Str, Signal.Signal(Str), Signal.Signal(Str) -> Elem
render_metric = |label, value, detail| {
	Html.div_c(
		metric_card_class,
		[
			Html.paragraph_c(label, metric_label_class),
			Html.div_c(metric_value_class, [Html.text_s(value)]),
			Html.div_c(metric_detail_class, [Html.text_s(detail)]),
		],
	)
}

main : {} -> Elem
main = |_| {
	Ui.state(
		0,
		|manual_refresh| {
			tick = Signal.interval(2000)
			refresh_request = Signal.map2(tick, manual_refresh.signal(), |timer_value, manual_value| timer_value + manual_value)

			dashboard_task = Http.get_text_task("dashboard")
			dashboard_state =
				Signal.fold_task(
					dashboard_task,
					DashboardLoading,
					decode_dashboard,
					|err| DashboardRequestFailed(err),
				)

			manual_refresh_text = Signal.map(manual_refresh.signal(), |count| concat3("Manual refreshes: ", count.to_str(), " | auto refresh 2s"))
			last_updated = Signal.map(dashboard_state, |state| state_detail(state, last_updated_text))
			overall_value = Signal.map(dashboard_state, |state| state_value(state, |dashboard| overall_text(dashboard.overall_code)))
			overall_detail = Signal.map(dashboard_state, |state| state_detail(state, overall_detail_text))
			traffic_value = Signal.map(dashboard_state, |state| state_value(state, |dashboard| metric_text(dashboard.requests_per_minute, " rpm")))
			traffic_detail = Signal.map(dashboard_state, |state| state_detail(state, traffic_detail_text))
			queue_value = Signal.map(dashboard_state, |state| state_value(state, |dashboard| metric_text(dashboard.queue_depth, " queued")))
			queue_detail = Signal.map(dashboard_state, |state| state_detail(state, queue_detail_text))
			services_value = Signal.map(dashboard_state, |state| state_value(state, |dashboard| concat4(dashboard.healthy_services.to_str(), "/", dashboard.total_services.to_str(), " healthy")))
			services_detail = Signal.map(dashboard_state, |state| state_detail(state, services_detail_text))
			status_rollup = Signal.map(dashboard_state, |state| state_text(state, status_rollup_text))
			traffic = Signal.map(dashboard_state, |state| state_text(state, traffic_text))
			jobs = Signal.map(dashboard_state, |state| state_text(state, jobs_text))
			alerts = Signal.map(dashboard_state, |state| state_text(state, alerts_text))
			health = Signal.map(dashboard_state, |state| state_text(state, health_text))
			incident_active = Signal.map(dashboard_state, state_is_incident)

			Html.div_c(
				page_class,
				[
					Html.div_c(
						shell_class,
						[
							Html.div_c(
								toolbar_class,
								[
									Html.div_c(
										toolbar_text_class,
										[
											Html.heading_c("Ops dashboard", "text-xl font-semibold text-zinc-950"),
											Html.div_c(toolbar_status_class, [Html.text_s(last_updated)]),
										],
									),
									Html.div_c(
										"flex flex-wrap items-center gap-3 text-sm text-zinc-600",
										[
											Html.text_s(manual_refresh_text),
											Html.button_c("Refresh now", primary_button_class, manual_refresh.on_unit(increment_u64)),
										],
									),
								],
							),
							Html.div_c(
								metric_grid_class,
								[
									render_metric("System state", overall_value, overall_detail),
									render_metric("Ingress", traffic_value, traffic_detail),
									render_metric("Job queue", queue_value, queue_detail),
									render_metric("Services", services_value, services_detail),
								],
							),
							render_panel("Status summary", "Status summary", status_rollup),
							Html.div_c(
								panel_grid_class,
								[
									render_panel("Traffic", "Traffic", traffic),
									render_panel("Active jobs", "Active jobs", jobs),
									Ui.when(
										incident_active,
										|_| render_panel_with_class("Alerts", "Alerts", critical_panel_class, alerts),
										|_| render_panel_with_class("Alerts", "Alerts", alert_panel_class, alerts),
									),
									render_panel("Service health", "Service health", health),
								],
							),
							Ui.on_mount(|_| Http.get_text(dashboard_task, "/api/ops/dashboard")),
							Ui.on_change(refresh_request, |_| Http.get_text(dashboard_task, "/api/ops/dashboard")),
						],
					),
				],
			)
		},
	)
}
