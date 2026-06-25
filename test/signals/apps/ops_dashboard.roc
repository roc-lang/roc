app [main] { pf: platform "../platform/main.roc" }

import pf.Elem exposing [Elem]
import pf.Html
import pf.Http
import pf.Signal
import pf.Ui

Dashboard : {
	alert_a_age_min : U64,
	alert_a_code : U64,
	alert_b_age_min : U64,
	alert_b_code : U64,
	alert_c_age_min : U64,
	alert_c_code : U64,
	api_latency_ms : U64,
	api_state_code : U64,
	billing_latency_ms : U64,
	billing_state_code : U64,
	blocked_jobs : U64,
	budget_bar_code : U64,
	budget_remaining_permille : U64,
	burn_rate_x10 : U64,
	database_lag_sec : U64,
	database_state_code : U64,
	db_write_rpm : U64,
	edge_latency_ms : U64,
	edge_state_code : U64,
	error_bar_code : U64,
	error_permille : U64,
	healthy_services : U64,
	identity_latency_ms : U64,
	identity_state_code : U64,
	incidents : U64,
	ingress_bar_code : U64,
	job_a_age_min : U64,
	job_a_id : U64,
	job_a_progress : U64,
	job_a_state_code : U64,
	job_b_age_min : U64,
	job_b_id : U64,
	job_b_progress : U64,
	job_b_state_code : U64,
	job_c_age_min : U64,
	job_c_id : U64,
	job_c_progress : U64,
	job_c_state_code : U64,
	job_d_age_min : U64,
	job_d_id : U64,
	job_d_progress : U64,
	job_d_state_code : U64,
	latency_bar_code : U64,
	latency_ms : U64,
	latency_target_ms : U64,
	oldest_job_min : U64,
	overall_code : U64,
	phase_code : U64,
	queue_capacity : U64,
	queue_depth : U64,
	queue_trend_code : U64,
	requests_per_minute : U64,
	running_jobs : U64,
	search_refresh_sec : U64,
	search_state_code : U64,
	tone_code : U64,
	total_services : U64,
	traffic_delta_percent : U64,
	updated_hour : U64,
	updated_minute : U64,
	updated_second : U64,
	updated_version : U64,
	webhook_rpm : U64,
	worker_oldest_job_min : U64,
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

concat5 : Str, Str, Str, Str, Str -> Str
concat5 = |a, b, c, d, e| Str.concat(concat4(a, b, c, d), e)

concat6 : Str, Str, Str, Str, Str, Str -> Str
concat6 = |a, b, c, d, e, f| Str.concat(concat5(a, b, c, d, e), f)

concat7 : Str, Str, Str, Str, Str, Str, Str -> Str
concat7 = |a, b, c, d, e, f, g| Str.concat(concat6(a, b, c, d, e, f), g)

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
		DashboardLoading => "Loading"
		DashboardReady(dashboard) => render(dashboard)
		DashboardRequestFailed(err) => concat3("Request failed: ", err, "")
		DashboardDecodeFailed(err) => parse_error_text(err)
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
		DashboardLoading => "Waiting for first server response"
		DashboardReady(dashboard) => render(dashboard)
		DashboardRequestFailed(err) => concat3("HTTP task failed: ", err, "")
		DashboardDecodeFailed(err) => parse_error_text(err)
	}

state_class : DashboardState, (Dashboard -> Str), Str -> Str
state_class = |state, render, loading_class|
	match state {
		DashboardLoading => loading_class
		DashboardReady(dashboard) => render(dashboard)
		DashboardRequestFailed(_) => "border-red-300 bg-red-50 text-red-950"
		DashboardDecodeFailed(_) => "border-red-300 bg-red-50 text-red-950"
	}

parse_dashboard : Str -> Try(Dashboard, ParseErr)
parse_dashboard = |body| {
	updated_version = parse_u64_field(Str.to_utf8(body), "updated_version")?
	updated_hour = parse_u64_field(updated_version.rest, "updated_hour")?
	updated_minute = parse_u64_field(updated_hour.rest, "updated_minute")?
	updated_second = parse_u64_field(updated_minute.rest, "updated_second")?
	overall_code = parse_u64_field(updated_second.rest, "overall_code")?
	tone_code = parse_u64_field(overall_code.rest, "tone_code")?
	phase_code = parse_u64_field(tone_code.rest, "phase_code")?
	incidents = parse_u64_field(phase_code.rest, "incidents")?
	requests_per_minute = parse_u64_field(incidents.rest, "requests_per_minute")?
	traffic_delta_percent = parse_u64_field(requests_per_minute.rest, "traffic_delta_percent")?
	error_permille = parse_u64_field(traffic_delta_percent.rest, "error_permille")?
	burn_rate_x10 = parse_u64_field(error_permille.rest, "burn_rate_x10")?
	budget_remaining_permille = parse_u64_field(burn_rate_x10.rest, "budget_remaining_permille")?
	latency_ms = parse_u64_field(budget_remaining_permille.rest, "latency_ms")?
	latency_target_ms = parse_u64_field(latency_ms.rest, "latency_target_ms")?
	webhook_rpm = parse_u64_field(latency_target_ms.rest, "webhook_rpm")?
	db_write_rpm = parse_u64_field(webhook_rpm.rest, "db_write_rpm")?
	ingress_bar_code = parse_u64_field(db_write_rpm.rest, "ingress_bar_code")?
	latency_bar_code = parse_u64_field(ingress_bar_code.rest, "latency_bar_code")?
	error_bar_code = parse_u64_field(latency_bar_code.rest, "error_bar_code")?
	budget_bar_code = parse_u64_field(error_bar_code.rest, "budget_bar_code")?
	queue_depth = parse_u64_field(budget_bar_code.rest, "queue_depth")?
	queue_trend_code = parse_u64_field(queue_depth.rest, "queue_trend_code")?
	queue_capacity = parse_u64_field(queue_trend_code.rest, "queue_capacity")?
	running_jobs = parse_u64_field(queue_capacity.rest, "running_jobs")?
	blocked_jobs = parse_u64_field(running_jobs.rest, "blocked_jobs")?
	oldest_job_min = parse_u64_field(blocked_jobs.rest, "oldest_job_min")?
	job_a_id = parse_u64_field(oldest_job_min.rest, "job_a_id")?
	job_a_progress = parse_u64_field(job_a_id.rest, "job_a_progress")?
	job_a_age_min = parse_u64_field(job_a_progress.rest, "job_a_age_min")?
	job_a_state_code = parse_u64_field(job_a_age_min.rest, "job_a_state_code")?
	job_b_id = parse_u64_field(job_a_state_code.rest, "job_b_id")?
	job_b_progress = parse_u64_field(job_b_id.rest, "job_b_progress")?
	job_b_age_min = parse_u64_field(job_b_progress.rest, "job_b_age_min")?
	job_b_state_code = parse_u64_field(job_b_age_min.rest, "job_b_state_code")?
	job_c_id = parse_u64_field(job_b_state_code.rest, "job_c_id")?
	job_c_progress = parse_u64_field(job_c_id.rest, "job_c_progress")?
	job_c_age_min = parse_u64_field(job_c_progress.rest, "job_c_age_min")?
	job_c_state_code = parse_u64_field(job_c_age_min.rest, "job_c_state_code")?
	job_d_id = parse_u64_field(job_c_state_code.rest, "job_d_id")?
	job_d_progress = parse_u64_field(job_d_id.rest, "job_d_progress")?
	job_d_age_min = parse_u64_field(job_d_progress.rest, "job_d_age_min")?
	job_d_state_code = parse_u64_field(job_d_age_min.rest, "job_d_state_code")?
	alert_a_code = parse_u64_field(job_d_state_code.rest, "alert_a_code")?
	alert_a_age_min = parse_u64_field(alert_a_code.rest, "alert_a_age_min")?
	alert_b_code = parse_u64_field(alert_a_age_min.rest, "alert_b_code")?
	alert_b_age_min = parse_u64_field(alert_b_code.rest, "alert_b_age_min")?
	alert_c_code = parse_u64_field(alert_b_age_min.rest, "alert_c_code")?
	alert_c_age_min = parse_u64_field(alert_c_code.rest, "alert_c_age_min")?
	healthy_services = parse_u64_field(alert_c_age_min.rest, "healthy_services")?
	total_services = parse_u64_field(healthy_services.rest, "total_services")?
	edge_state_code = parse_u64_field(total_services.rest, "edge_state_code")?
	edge_latency_ms = parse_u64_field(edge_state_code.rest, "edge_latency_ms")?
	api_state_code = parse_u64_field(edge_latency_ms.rest, "api_state_code")?
	api_latency_ms = parse_u64_field(api_state_code.rest, "api_latency_ms")?
	worker_state_code = parse_u64_field(api_latency_ms.rest, "worker_state_code")?
	worker_oldest_job_min = parse_u64_field(worker_state_code.rest, "worker_oldest_job_min")?
	database_state_code = parse_u64_field(worker_oldest_job_min.rest, "database_state_code")?
	database_lag_sec = parse_u64_field(database_state_code.rest, "database_lag_sec")?
	billing_state_code = parse_u64_field(database_lag_sec.rest, "billing_state_code")?
	billing_latency_ms = parse_u64_field(billing_state_code.rest, "billing_latency_ms")?
	search_state_code = parse_u64_field(billing_latency_ms.rest, "search_state_code")?
	search_refresh_sec = parse_u64_field(search_state_code.rest, "search_refresh_sec")?
	identity_state_code = parse_u64_field(search_refresh_sec.rest, "identity_state_code")?
	identity_latency_ms = parse_u64_field(identity_state_code.rest, "identity_latency_ms")?

	if !List.is_empty(identity_latency_ms.rest) {
		return Err(OpsBadProtocol)
	}

	Ok(
		{
			alert_a_age_min: alert_a_age_min.value,
			alert_a_code: alert_a_code.value,
			alert_b_age_min: alert_b_age_min.value,
			alert_b_code: alert_b_code.value,
			alert_c_age_min: alert_c_age_min.value,
			alert_c_code: alert_c_code.value,
			api_latency_ms: api_latency_ms.value,
			api_state_code: api_state_code.value,
			billing_latency_ms: billing_latency_ms.value,
			billing_state_code: billing_state_code.value,
			blocked_jobs: blocked_jobs.value,
			budget_bar_code: budget_bar_code.value,
			budget_remaining_permille: budget_remaining_permille.value,
			burn_rate_x10: burn_rate_x10.value,
			database_lag_sec: database_lag_sec.value,
			database_state_code: database_state_code.value,
			db_write_rpm: db_write_rpm.value,
			edge_latency_ms: edge_latency_ms.value,
			edge_state_code: edge_state_code.value,
			error_bar_code: error_bar_code.value,
			error_permille: error_permille.value,
			healthy_services: healthy_services.value,
			identity_latency_ms: identity_latency_ms.value,
			identity_state_code: identity_state_code.value,
			incidents: incidents.value,
			ingress_bar_code: ingress_bar_code.value,
			job_a_age_min: job_a_age_min.value,
			job_a_id: job_a_id.value,
			job_a_progress: job_a_progress.value,
			job_a_state_code: job_a_state_code.value,
			job_b_age_min: job_b_age_min.value,
			job_b_id: job_b_id.value,
			job_b_progress: job_b_progress.value,
			job_b_state_code: job_b_state_code.value,
			job_c_age_min: job_c_age_min.value,
			job_c_id: job_c_id.value,
			job_c_progress: job_c_progress.value,
			job_c_state_code: job_c_state_code.value,
			job_d_age_min: job_d_age_min.value,
			job_d_id: job_d_id.value,
			job_d_progress: job_d_progress.value,
			job_d_state_code: job_d_state_code.value,
			latency_bar_code: latency_bar_code.value,
			latency_ms: latency_ms.value,
			latency_target_ms: latency_target_ms.value,
			oldest_job_min: oldest_job_min.value,
			overall_code: overall_code.value,
			phase_code: phase_code.value,
			queue_capacity: queue_capacity.value,
			queue_depth: queue_depth.value,
			queue_trend_code: queue_trend_code.value,
			requests_per_minute: requests_per_minute.value,
			running_jobs: running_jobs.value,
			search_refresh_sec: search_refresh_sec.value,
			search_state_code: search_state_code.value,
			tone_code: tone_code.value,
			total_services: total_services.value,
			traffic_delta_percent: traffic_delta_percent.value,
			updated_hour: updated_hour.value,
			updated_minute: updated_minute.value,
			updated_second: updated_second.value,
			updated_version: updated_version.value,
			webhook_rpm: webhook_rpm.value,
			worker_oldest_job_min: worker_oldest_job_min.value,
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

two_digits : U64 -> Str
two_digits = |value|
	if value < 10 {
		concat3("0", value.to_str(), "")
	} else {
		value.to_str()
	}

three_digits : U64 -> Str
three_digits = |value|
	if value < 10 {
		concat3("00", value.to_str(), "")
	} else if value < 100 {
		concat3("0", value.to_str(), "")
	} else {
		value.to_str()
	}

clock_text : Dashboard -> Str
clock_text = |dashboard|
	concat6(two_digits(dashboard.updated_hour), ":", two_digits(dashboard.updated_minute), ":", two_digits(dashboard.updated_second), " UTC")

overall_text : U64 -> Str
overall_text = |code|
	if code == 1 {
		"Degraded"
	} else {
		"Nominal"
	}

phase_text : U64 -> Str
phase_text = |code|
	if code == 2 {
		"Active incident"
	} else if code == 3 {
		"Recovering"
	} else if code == 1 {
		"Watch"
	} else {
		"Steady"
	}

tone_text : U64 -> Str
tone_text = |code|
	if code == 2 {
		"bad"
	} else if code == 1 {
		"watch"
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

job_state_text : U64 -> Str
job_state_text = |code|
	if code == 3 {
		"blocked"
	} else if code == 2 {
		"retrying"
	} else if code == 1 {
		"queued"
	} else {
		"running"
	}

queue_trend_text : U64 -> Str
queue_trend_text = |code|
	if code == 2 {
		"rising"
	} else if code == 0 {
		"draining"
	} else {
		"steady"
	}

alert_severity_text : U64 -> Str
alert_severity_text = |code|
	if code == 1 {
		"critical"
	} else if code == 2 {
		"warning"
	} else {
		"info"
	}

alert_service_text : U64 -> Str
alert_service_text = |code|
	if code == 1 {
		"payments-api"
	} else if code == 2 {
		"workers"
	} else if code == 3 {
		"edge"
	} else if code == 4 {
		"payments-api"
	} else {
		"edge"
	}

alert_state_text : U64 -> Str
alert_state_text = |code|
	if code == 1 {
		"active"
	} else if code == 2 {
		"monitoring"
	} else if code == 3 {
		"monitoring"
	} else if code == 4 {
		"recovering"
	} else {
		"steady"
	}

alert_summary_text : U64 -> Str
alert_summary_text = |code|
	if code == 1 {
		"Checkout latency above SLO"
	} else if code == 2 {
		"Queue age approaching cap"
	} else if code == 3 {
		"Canary pool shifted 10 percent"
	} else if code == 4 {
		"Error budget burn below 1x"
	} else {
		"Canary pool normal"
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

progress_bar_text : U64 -> Str
progress_bar_text = |progress| bar_text(progress // 12)

permille_text : U64 -> Str
permille_text = |value| concat4((value // 10).to_str(), ".", (value % 10).to_str(), "%")

rate_x_text : U64 -> Str
rate_x_text = |value| concat4((value // 10).to_str(), ".", (value % 10).to_str(), "x")

job_id_text : U64 -> Str
job_id_text = |id| concat3("job-", three_digits(id), "")

metric_text : U64, Str -> Str
metric_text = |value, suffix| concat3(value.to_str(), suffix, "")

tone_panel_class : U64 -> Str
tone_panel_class = |code|
	if code == 2 {
		"border-red-300 bg-red-50 text-red-950"
	} else if code == 1 {
		"border-amber-300 bg-amber-50 text-amber-950"
	} else {
		"border-emerald-200 bg-emerald-50 text-emerald-950"
	}

tone_metric_class : U64 -> Str
tone_metric_class = |code| concat3(metric_card_class, " ", tone_panel_class(code))

service_cell_class_for : U64 -> Str
service_cell_class_for = |code|
	if code == 2 {
		"grid gap-1 rounded-md border border-red-300 bg-red-50 p-3 text-red-950"
	} else if code == 1 {
		"grid gap-1 rounded-md border border-amber-300 bg-amber-50 p-3 text-amber-950"
	} else {
		"grid gap-1 rounded-md border border-zinc-200 bg-white p-3 text-zinc-950"
	}

job_row_class_for : U64 -> Str
job_row_class_for = |code|
	if code == 3 {
		"grid gap-2 rounded-md border border-red-300 bg-red-50 p-3 text-sm text-red-950"
	} else if code == 2 {
		"grid gap-2 rounded-md border border-amber-300 bg-amber-50 p-3 text-sm text-amber-950"
	} else if code == 1 {
		"grid gap-2 rounded-md border border-zinc-200 bg-zinc-50 p-3 text-sm text-zinc-800"
	} else {
		"grid gap-2 rounded-md border border-zinc-200 bg-white p-3 text-sm text-zinc-900"
	}

alert_row_class_for : U64 -> Str
alert_row_class_for = |code|
	if code == 1 {
		"grid gap-1 rounded-md border border-red-300 bg-red-50 p-3 text-sm text-red-950"
	} else if code == 2 {
		"grid gap-1 rounded-md border border-amber-300 bg-amber-50 p-3 text-sm text-amber-950"
	} else {
		"grid gap-1 rounded-md border border-zinc-200 bg-white p-3 text-sm text-zinc-800"
	}

service_state_class : DashboardState, (Dashboard -> U64) -> Str
service_state_class = |state, code_of| state_class(state, |dashboard| service_cell_class_for(code_of(dashboard)), "grid gap-1 rounded-md border border-zinc-200 bg-white p-3 text-zinc-500")

job_state_class : DashboardState, (Dashboard -> U64) -> Str
job_state_class = |state, code_of| state_class(state, |dashboard| job_row_class_for(code_of(dashboard)), "grid gap-2 rounded-md border border-zinc-200 bg-white p-3 text-sm text-zinc-500")

alert_state_class : DashboardState, (Dashboard -> U64) -> Str
alert_state_class = |state, code_of| state_class(state, |dashboard| alert_row_class_for(code_of(dashboard)), "grid gap-1 rounded-md border border-zinc-200 bg-white p-3 text-sm text-zinc-500")

status_strip_class : DashboardState -> Str
status_strip_class = |state|
	state_class(
		state,
		|dashboard| concat3("grid gap-3 rounded-md border p-4 sm:grid-cols-2 xl:grid-cols-4 ", tone_panel_class(dashboard.tone_code), ""),
		"grid gap-3 rounded-md border border-zinc-200 bg-white p-4 text-zinc-700 sm:grid-cols-2 xl:grid-cols-4",
	)

status_detail_text : Dashboard -> Str
status_detail_text = |dashboard|
	join_with(
		" | ",
		[
			concat3(dashboard.incidents.to_str(), " incidents", ""),
			concat4(dashboard.healthy_services.to_str(), "/", dashboard.total_services.to_str(), " services healthy"),
			concat3("tone ", tone_text(dashboard.tone_code), ""),
		],
	)

last_updated_text : Dashboard -> Str
last_updated_text = |dashboard|
	concat7("Last server update ", clock_text(dashboard), " | tick ", dashboard.updated_version.to_str(), " | auto refresh ", "2s", "")

traffic_detail_text : Dashboard -> Str
traffic_detail_text = |dashboard|
	join_with(
		" | ",
		[
			concat4("+", dashboard.traffic_delta_percent.to_str(), "%", " over 5m"),
			concat3("webhooks ", dashboard.webhook_rpm.to_str(), " rpm"),
			concat3("db writes ", dashboard.db_write_rpm.to_str(), " rpm"),
		],
	)

latency_detail_text : Dashboard -> Str
latency_detail_text = |dashboard|
	join_with(
		" | ",
		[
			concat3("target ", dashboard.latency_target_ms.to_str(), " ms"),
			bar_text(dashboard.latency_bar_code),
			concat3("api ", health_state_text(dashboard.api_state_code), ""),
		],
	)

queue_detail_text : Dashboard -> Str
queue_detail_text = |dashboard|
	join_with(
		" | ",
		[
			queue_trend_text(dashboard.queue_trend_code),
			concat3(dashboard.running_jobs.to_str(), " running", ""),
			concat3(dashboard.blocked_jobs.to_str(), " blocked", ""),
			concat3("oldest ", dashboard.oldest_job_min.to_str(), "m"),
		],
	)

budget_detail_text : Dashboard -> Str
budget_detail_text = |dashboard|
	join_with(
		" | ",
		[
			concat3("burn ", rate_x_text(dashboard.burn_rate_x10), ""),
			concat3("errors ", permille_text(dashboard.error_permille), ""),
			bar_text(dashboard.budget_bar_code),
		],
	)

services_detail_text : Dashboard -> Str
services_detail_text = |dashboard|
	join_with(
		" | ",
		[
			concat4(dashboard.healthy_services.to_str(), "/", dashboard.total_services.to_str(), " healthy"),
			concat3("workers ", health_state_text(dashboard.worker_state_code), ""),
			concat3("billing ", health_state_text(dashboard.billing_state_code), ""),
		],
	)

traffic_row_text : Str, Str, Str, Str -> Str
traffic_row_text = |label, value, bar, note| join_with("  ", [label, value, bar, note])

toolbar_count_text : U64 -> Str
toolbar_count_text = |count| concat3("Manual refreshes: ", count.to_str(), " | auto refresh 2s")

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
metric_grid_class = "grid gap-3 sm:grid-cols-2 xl:grid-cols-5"

main_grid_class : Str
main_grid_class = "grid gap-4 xl:grid-cols-3"

wide_column_class : Str
wide_column_class = "grid gap-4 xl:col-span-2"

side_column_class : Str
side_column_class = "grid gap-4"

panel_class : Str
panel_class = "grid gap-3 rounded-md border border-zinc-200 bg-white p-4"

metric_card_class : Str
metric_card_class = "grid min-h-32 gap-2 rounded-md border p-4"

metric_label_class : Str
metric_label_class = "text-xs font-semibold uppercase tracking-normal text-zinc-500"

metric_value_class : Str
metric_value_class = "text-2xl font-semibold"

metric_detail_class : Str
metric_detail_class = "text-sm leading-5"

section_heading_class : Str
section_heading_class = "text-sm font-semibold uppercase tracking-normal text-zinc-600"

fine_text_class : Str
fine_text_class = "text-xs leading-5 text-zinc-500"

row_label_class : Str
row_label_class = "text-xs font-semibold uppercase tracking-normal text-zinc-500"

row_value_class : Str
row_value_class = "font-mono text-sm text-zinc-950"

primary_button_class : Str
primary_button_class = "rounded-md border border-zinc-900 bg-zinc-900 px-3 py-2 text-sm font-medium text-white hover:border-zinc-700 hover:bg-zinc-700"

render_metric : Str, Signal.Signal(Str), Signal.Signal(Str), Signal.Signal(Str) -> Elem
render_metric = |label, value, detail, classes| {
	Html.div_sc(
		classes,
		[
			Html.paragraph_c(label, metric_label_class),
			Html.div_c(metric_value_class, [Html.text_s(value)]),
			Html.div_c(metric_detail_class, [Html.text_s(detail)]),
		],
	)
}

render_status_item : Str, Signal.Signal(Str), Signal.Signal(Str) -> Elem
render_status_item = |label, value, detail| {
	Html.div_c(
		"grid gap-1",
		[
			Html.paragraph_c(label, "text-xs font-semibold uppercase tracking-normal"),
			Html.div_c("text-lg font-semibold", [Html.text_s(value)]),
			Html.div_c("text-xs leading-5", [Html.text_s(detail)]),
		],
	)
}

render_panel : Str, Str, List(Elem) -> Elem
render_panel = |region_label, heading, children| {
	Html.section_c(
		region_label,
		panel_class,
		List.concat(
			[Html.heading_c(heading, section_heading_class)],
			children,
		),
	)
}

render_traffic_row : Str, Signal.Signal(Str), Signal.Signal(Str), Signal.Signal(Str) -> Elem
render_traffic_row = |label, value, bar, note| {
	Html.div_c(
		"grid gap-1 border-t border-zinc-100 py-2 sm:grid-cols-4 sm:gap-3",
		[
			Html.div_c(row_label_class, [Html.text(label)]),
			Html.div_c(row_value_class, [Html.text_s(value)]),
			Html.div_c("font-mono text-sm text-zinc-700", [Html.text_s(bar)]),
			Html.div_c("text-sm text-zinc-600", [Html.text_s(note)]),
		],
	)
}

render_job_row : Str, Signal.Signal(Str), Signal.Signal(Str), Signal.Signal(Str), Signal.Signal(Str), Signal.Signal(Str) -> Elem
render_job_row = |title, id_text, state_text_signal, progress_text, detail, classes| {
	Html.div_sc(
		classes,
		[
			Html.div_c(
				"flex flex-wrap items-center justify-between gap-2",
				[
					Html.div_c("font-semibold", [Html.text(title)]),
					Html.div_c("font-mono text-xs", [Html.text_s(id_text)]),
				],
			),
			Html.div_c(
				"grid gap-1 sm:grid-cols-3",
				[
					Html.div_c("text-sm", [Html.text_s(state_text_signal)]),
					Html.div_c("font-mono text-sm", [Html.text_s(progress_text)]),
					Html.div_c("text-sm", [Html.text_s(detail)]),
				],
			),
		],
	)
}

render_alert_row : Signal.Signal(Str), Signal.Signal(Str), Signal.Signal(Str), Signal.Signal(Str), Signal.Signal(Str), Signal.Signal(Str) -> Elem
render_alert_row = |severity, service, alert_state, age, summary, classes| {
	Html.div_sc(
		classes,
		[
			Html.div_c(
				"flex flex-wrap items-center gap-2",
				[
					Html.div_c("font-semibold", [Html.text_s(severity)]),
					Html.div_c("font-mono text-xs", [Html.text_s(service)]),
					Html.div_c("text-xs", [Html.text_s(age)]),
				],
			),
			Html.div_c("text-sm", [Html.text_s(summary)]),
			Html.div_c("text-xs", [Html.text_s(alert_state)]),
		],
	)
}

render_service_cell : Str, Signal.Signal(Str), Signal.Signal(Str), Signal.Signal(Str) -> Elem
render_service_cell = |name, state_text_signal, detail, classes| {
	Html.div_sc(
		classes,
		[
			Html.div_c("flex items-center justify-between gap-2", [Html.div_c("font-semibold", [Html.text(name)]), Html.div_c("font-mono text-xs", [Html.text_s(state_text_signal)])]),
			Html.div_c("text-sm", [Html.text_s(detail)]),
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

			manual_refresh_text = Signal.map(manual_refresh.signal(), toolbar_count_text)
			last_updated = Signal.map(dashboard_state, |state| state_detail(state, last_updated_text))
			phase_value = Signal.map(dashboard_state, |state| state_value(state, |dashboard| phase_text(dashboard.phase_code)))
			phase_detail = Signal.map(dashboard_state, |state| state_detail(state, status_detail_text))
			overall_value = Signal.map(dashboard_state, |state| state_value(state, |dashboard| overall_text(dashboard.overall_code)))
			overall_detail = Signal.map(dashboard_state, |state| state_detail(state, services_detail_text))
			traffic_value = Signal.map(dashboard_state, |state| state_value(state, |dashboard| metric_text(dashboard.requests_per_minute, " rpm")))
			traffic_detail = Signal.map(dashboard_state, |state| state_detail(state, traffic_detail_text))
			latency_value = Signal.map(dashboard_state, |state| state_value(state, |dashboard| metric_text(dashboard.latency_ms, " ms")))
			latency_detail = Signal.map(dashboard_state, |state| state_detail(state, latency_detail_text))
			queue_value = Signal.map(dashboard_state, |state| state_value(state, |dashboard| metric_text(dashboard.queue_depth, " queued")))
			queue_detail = Signal.map(dashboard_state, |state| state_detail(state, queue_detail_text))
			budget_value = Signal.map(dashboard_state, |state| state_value(state, |dashboard| permille_text(dashboard.budget_remaining_permille)))
			budget_detail = Signal.map(dashboard_state, |state| state_detail(state, budget_detail_text))
			status_classes = Signal.map(dashboard_state, status_strip_class)
			system_metric_class = Signal.map(dashboard_state, |state| state_class(state, |dashboard| tone_metric_class(dashboard.tone_code), concat3(metric_card_class, " ", "border-zinc-200 bg-white text-zinc-700")))
			traffic_metric_class = Signal.map(dashboard_state, |state| state_class(state, |_| concat3(metric_card_class, " ", "border-sky-200 bg-sky-50 text-zinc-950"), concat3(metric_card_class, " ", "border-zinc-200 bg-white text-zinc-700")))
			latency_metric_class = Signal.map(dashboard_state, |state| state_class(state, |dashboard| tone_metric_class(dashboard.api_state_code), concat3(metric_card_class, " ", "border-zinc-200 bg-white text-zinc-700")))
			queue_metric_class = Signal.map(dashboard_state, |state| state_class(state, |dashboard| tone_metric_class(dashboard.worker_state_code), concat3(metric_card_class, " ", "border-zinc-200 bg-white text-zinc-700")))
			budget_metric_class = Signal.map(dashboard_state, |state| state_class(state, |dashboard| tone_metric_class(dashboard.tone_code), concat3(metric_card_class, " ", "border-zinc-200 bg-white text-zinc-700")))

			ingress_row_value = Signal.map(dashboard_state, |state| state_value(state, |dashboard| metric_text(dashboard.requests_per_minute, " rpm")))
			ingress_row_bar = Signal.map(dashboard_state, |state| state_value(state, |dashboard| bar_text(dashboard.ingress_bar_code)))
			ingress_row_note = Signal.map(dashboard_state, |state| state_detail(state, |dashboard| concat4("+", dashboard.traffic_delta_percent.to_str(), "%", " over 5m")))
			latency_row_value = Signal.map(dashboard_state, |state| state_value(state, |dashboard| metric_text(dashboard.latency_ms, " ms")))
			latency_row_bar = Signal.map(dashboard_state, |state| state_value(state, |dashboard| bar_text(dashboard.latency_bar_code)))
			latency_row_note = Signal.map(dashboard_state, |state| state_detail(state, |dashboard| concat3("target ", dashboard.latency_target_ms.to_str(), " ms")))
			error_row_value = Signal.map(dashboard_state, |state| state_value(state, |dashboard| permille_text(dashboard.error_permille)))
			error_row_bar = Signal.map(dashboard_state, |state| state_value(state, |dashboard| bar_text(dashboard.error_bar_code)))
			error_row_note = Signal.map(dashboard_state, |state| state_detail(state, |dashboard| concat3("burn ", rate_x_text(dashboard.burn_rate_x10), "")))
			webhook_row_value = Signal.map(dashboard_state, |state| state_value(state, |dashboard| metric_text(dashboard.webhook_rpm, " rpm")))
			webhook_row_bar = Signal.map(dashboard_state, |state| state_value(state, |_| "#######-----"))
			webhook_row_note = Signal.map(dashboard_state, |state| state_detail(state, |_| "fanout and retry pool"))
			db_row_value = Signal.map(dashboard_state, |state| state_value(state, |dashboard| metric_text(dashboard.db_write_rpm, " rpm")))
			db_row_bar = Signal.map(dashboard_state, |state| state_value(state, |_| "######------"))
			db_row_note = Signal.map(dashboard_state, |state| state_detail(state, |dashboard| concat3("replica lag ", dashboard.database_lag_sec.to_str(), "s")))

			job_a_id = Signal.map(dashboard_state, |state| state_value(state, |dashboard| job_id_text(dashboard.job_a_id)))
			job_a_state = Signal.map(dashboard_state, |state| state_value(state, |dashboard| job_state_text(dashboard.job_a_state_code)))
			job_a_progress = Signal.map(dashboard_state, |state| state_value(state, |dashboard| concat4(dashboard.job_a_progress.to_str(), "% ", progress_bar_text(dashboard.job_a_progress), "")))
			job_a_detail = Signal.map(dashboard_state, |state| state_detail(state, |dashboard| concat3(dashboard.job_a_age_min.to_str(), "m | workers/search", "")))
			job_a_class = Signal.map(dashboard_state, |state| job_state_class(state, |dashboard| dashboard.job_a_state_code))
			job_b_id = Signal.map(dashboard_state, |state| state_value(state, |dashboard| job_id_text(dashboard.job_b_id)))
			job_b_state = Signal.map(dashboard_state, |state| state_value(state, |dashboard| job_state_text(dashboard.job_b_state_code)))
			job_b_progress = Signal.map(dashboard_state, |state| state_value(state, |dashboard| concat4(dashboard.job_b_progress.to_str(), "% ", progress_bar_text(dashboard.job_b_progress), "")))
			job_b_detail = Signal.map(dashboard_state, |state| state_detail(state, |dashboard| concat3(dashboard.job_b_age_min.to_str(), "m | billing", "")))
			job_b_class = Signal.map(dashboard_state, |state| job_state_class(state, |dashboard| dashboard.job_b_state_code))
			job_c_id = Signal.map(dashboard_state, |state| state_value(state, |dashboard| job_id_text(dashboard.job_c_id)))
			job_c_state = Signal.map(dashboard_state, |state| state_value(state, |dashboard| job_state_text(dashboard.job_c_state_code)))
			job_c_progress = Signal.map(dashboard_state, |state| state_value(state, |dashboard| concat4(dashboard.job_c_progress.to_str(), "% ", progress_bar_text(dashboard.job_c_progress), "")))
			job_c_detail = Signal.map(dashboard_state, |state| state_detail(state, |dashboard| concat3(dashboard.job_c_age_min.to_str(), "m | compliance", "")))
			job_c_class = Signal.map(dashboard_state, |state| job_state_class(state, |dashboard| dashboard.job_c_state_code))
			job_d_id = Signal.map(dashboard_state, |state| state_value(state, |dashboard| job_id_text(dashboard.job_d_id)))
			job_d_state = Signal.map(dashboard_state, |state| state_value(state, |dashboard| job_state_text(dashboard.job_d_state_code)))
			job_d_progress = Signal.map(dashboard_state, |state| state_value(state, |dashboard| concat4(dashboard.job_d_progress.to_str(), "% ", progress_bar_text(dashboard.job_d_progress), "")))
			job_d_detail = Signal.map(dashboard_state, |state| state_detail(state, |dashboard| concat3(dashboard.job_d_age_min.to_str(), "m | identity", "")))
			job_d_class = Signal.map(dashboard_state, |state| job_state_class(state, |dashboard| dashboard.job_d_state_code))

			alert_a_severity = Signal.map(dashboard_state, |state| state_value(state, |dashboard| alert_severity_text(dashboard.alert_a_code)))
			alert_a_service = Signal.map(dashboard_state, |state| state_value(state, |dashboard| alert_service_text(dashboard.alert_a_code)))
			alert_a_state = Signal.map(dashboard_state, |state| state_value(state, |dashboard| alert_state_text(dashboard.alert_a_code)))
			alert_a_age = Signal.map(dashboard_state, |state| state_value(state, |dashboard| concat3(dashboard.alert_a_age_min.to_str(), "m", "")))
			alert_a_summary = Signal.map(dashboard_state, |state| state_detail(state, |dashboard| alert_summary_text(dashboard.alert_a_code)))
			alert_a_class = Signal.map(dashboard_state, |state| alert_state_class(state, |dashboard| dashboard.alert_a_code))
			alert_b_severity = Signal.map(dashboard_state, |state| state_value(state, |dashboard| alert_severity_text(dashboard.alert_b_code)))
			alert_b_service = Signal.map(dashboard_state, |state| state_value(state, |dashboard| alert_service_text(dashboard.alert_b_code)))
			alert_b_state = Signal.map(dashboard_state, |state| state_value(state, |dashboard| alert_state_text(dashboard.alert_b_code)))
			alert_b_age = Signal.map(dashboard_state, |state| state_value(state, |dashboard| concat3(dashboard.alert_b_age_min.to_str(), "m", "")))
			alert_b_summary = Signal.map(dashboard_state, |state| state_detail(state, |dashboard| alert_summary_text(dashboard.alert_b_code)))
			alert_b_class = Signal.map(dashboard_state, |state| alert_state_class(state, |dashboard| dashboard.alert_b_code))
			alert_c_severity = Signal.map(dashboard_state, |state| state_value(state, |dashboard| alert_severity_text(dashboard.alert_c_code)))
			alert_c_service = Signal.map(dashboard_state, |state| state_value(state, |dashboard| alert_service_text(dashboard.alert_c_code)))
			alert_c_state = Signal.map(dashboard_state, |state| state_value(state, |dashboard| alert_state_text(dashboard.alert_c_code)))
			alert_c_age = Signal.map(dashboard_state, |state| state_value(state, |dashboard| concat3(dashboard.alert_c_age_min.to_str(), "m", "")))
			alert_c_summary = Signal.map(dashboard_state, |state| state_detail(state, |dashboard| alert_summary_text(dashboard.alert_c_code)))
			alert_c_class = Signal.map(dashboard_state, |state| alert_state_class(state, |dashboard| dashboard.alert_c_code))

			edge_state = Signal.map(dashboard_state, |state| state_value(state, |dashboard| health_state_text(dashboard.edge_state_code)))
			edge_detail = Signal.map(dashboard_state, |state| state_detail(state, |dashboard| concat3("p95 ", dashboard.edge_latency_ms.to_str(), " ms | 8 pods")))
			edge_class = Signal.map(dashboard_state, |state| service_state_class(state, |dashboard| dashboard.edge_state_code))
			api_state = Signal.map(dashboard_state, |state| state_value(state, |dashboard| health_state_text(dashboard.api_state_code)))
			api_detail = Signal.map(dashboard_state, |state| state_detail(state, |dashboard| concat3("p95 ", dashboard.api_latency_ms.to_str(), " ms | 12 pods")))
			api_class = Signal.map(dashboard_state, |state| service_state_class(state, |dashboard| dashboard.api_state_code))
			worker_state = Signal.map(dashboard_state, |state| state_value(state, |dashboard| health_state_text(dashboard.worker_state_code)))
			worker_detail = Signal.map(dashboard_state, |state| state_detail(state, |dashboard| concat5("oldest ", dashboard.worker_oldest_job_min.to_str(), "m | ", dashboard.queue_depth.to_str(), " queued")))
			worker_class = Signal.map(dashboard_state, |state| service_state_class(state, |dashboard| dashboard.worker_state_code))
			database_state = Signal.map(dashboard_state, |state| state_value(state, |dashboard| health_state_text(dashboard.database_state_code)))
			database_detail = Signal.map(dashboard_state, |state| state_detail(state, |dashboard| concat3("lag ", dashboard.database_lag_sec.to_str(), "s | 2 writers")))
			database_class = Signal.map(dashboard_state, |state| service_state_class(state, |dashboard| dashboard.database_state_code))
			billing_state = Signal.map(dashboard_state, |state| state_value(state, |dashboard| health_state_text(dashboard.billing_state_code)))
			billing_detail = Signal.map(dashboard_state, |state| state_detail(state, |dashboard| concat3("p95 ", dashboard.billing_latency_ms.to_str(), " ms | webhooks")))
			billing_class = Signal.map(dashboard_state, |state| service_state_class(state, |dashboard| dashboard.billing_state_code))
			search_state = Signal.map(dashboard_state, |state| state_value(state, |dashboard| health_state_text(dashboard.search_state_code)))
			search_detail = Signal.map(dashboard_state, |state| state_detail(state, |dashboard| concat3("refresh ", dashboard.search_refresh_sec.to_str(), "s | 5 shards")))
			search_class = Signal.map(dashboard_state, |state| service_state_class(state, |dashboard| dashboard.search_state_code))
			identity_state = Signal.map(dashboard_state, |state| state_value(state, |dashboard| health_state_text(dashboard.identity_state_code)))
			identity_detail = Signal.map(dashboard_state, |state| state_detail(state, |dashboard| concat3("p95 ", dashboard.identity_latency_ms.to_str(), " ms | session cache")))
			identity_class = Signal.map(dashboard_state, |state| service_state_class(state, |dashboard| dashboard.identity_state_code))

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
							Html.div_sc(
								status_classes,
								[
									render_status_item("Phase", phase_value, phase_detail),
									render_status_item("System", overall_value, overall_detail),
									render_status_item("Traffic", traffic_value, traffic_detail),
									render_status_item("Queue", queue_value, queue_detail),
								],
							),
							Html.div_c(
								metric_grid_class,
								[
									render_metric("System state", overall_value, overall_detail, system_metric_class),
									render_metric("Ingress", traffic_value, traffic_detail, traffic_metric_class),
									render_metric("API latency", latency_value, latency_detail, latency_metric_class),
									render_metric("Job queue", queue_value, queue_detail, queue_metric_class),
									render_metric("Error budget", budget_value, budget_detail, budget_metric_class),
								],
							),
							Html.div_c(
								main_grid_class,
								[
									Html.div_c(
										wide_column_class,
										[
											render_panel(
												"Traffic",
												"Traffic and pressure",
												[
													render_traffic_row("Ingress", ingress_row_value, ingress_row_bar, ingress_row_note),
													render_traffic_row("API p95", latency_row_value, latency_row_bar, latency_row_note),
													render_traffic_row("Error rate", error_row_value, error_row_bar, error_row_note),
													render_traffic_row("Webhook fanout", webhook_row_value, webhook_row_bar, webhook_row_note),
													render_traffic_row("DB writes", db_row_value, db_row_bar, db_row_note),
												],
											),
											render_panel(
												"Service health",
												"Service matrix",
												[
													Html.div_c(
														"grid gap-3 sm:grid-cols-2 xl:grid-cols-3",
														[
															render_service_cell("edge", edge_state, edge_detail, edge_class),
															render_service_cell("api", api_state, api_detail, api_class),
															render_service_cell("workers", worker_state, worker_detail, worker_class),
															render_service_cell("database", database_state, database_detail, database_class),
															render_service_cell("billing", billing_state, billing_detail, billing_class),
															render_service_cell("search", search_state, search_detail, search_class),
															render_service_cell("identity", identity_state, identity_detail, identity_class),
														],
													),
												],
											),
										],
									),
									Html.div_c(
										side_column_class,
										[
											render_panel(
												"Active jobs",
												"Active jobs",
												[
													render_job_row("Rebuild search index", job_a_id, job_a_state, job_a_progress, job_a_detail, job_a_class),
													render_job_row("Backfill billing events", job_b_id, job_b_state, job_b_progress, job_b_detail, job_b_class),
													render_job_row("Export audit archive", job_c_id, job_c_state, job_c_progress, job_c_detail, job_c_class),
													render_job_row("Prune stale sessions", job_d_id, job_d_state, job_d_progress, job_d_detail, job_d_class),
												],
											),
											render_panel(
												"Alerts",
												"Incidents and alerts",
												[
													render_alert_row(alert_a_severity, alert_a_service, alert_a_state, alert_a_age, alert_a_summary, alert_a_class),
													render_alert_row(alert_b_severity, alert_b_service, alert_b_state, alert_b_age, alert_b_summary, alert_b_class),
													render_alert_row(alert_c_severity, alert_c_service, alert_c_state, alert_c_age, alert_c_summary, alert_c_class),
												],
											),
										],
									),
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
