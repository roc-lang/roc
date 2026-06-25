import Dashboard
import DashboardTheme

DashboardView :: [].{
	StatusStrip : {
		tone : DashboardTheme.Tone,
		items : List(StatusItem),
	}

	StatusItem : {
		id : Str,
		label : Str,
		value : Str,
		detail : Str,
	}

	Metric : {
		id : Str,
		label : Str,
		value : Str,
		detail : Str,
		tone : DashboardTheme.Tone,
	}

	TrafficRow : {
		id : Str,
		label : Str,
		value : Str,
		bar : Str,
		note : Str,
	}

	ServiceRow : {
		id : Str,
		label : Str,
		state : Str,
		detail : Str,
		tone : DashboardTheme.Tone,
	}

	JobRow : {
		id : Str,
		label : Str,
		owner : Str,
		run_id : Str,
		state : Str,
		progress : Str,
		age : Str,
		tone : DashboardTheme.Tone,
	}

	AlertRow : {
		id : Str,
		severity : Str,
		service : Str,
		state : Str,
		age : Str,
		summary : Str,
		tone : DashboardTheme.Tone,
	}

	last_updated : Dashboard -> Str
	last_updated = |dashboard| "Last server update ${clock_text(dashboard.updated)} | tick ${dashboard.version.to_str()} | auto refresh 2s"

	status_tone : Dashboard -> DashboardTheme.Tone
	status_tone = |dashboard| {
		degraded = count_health(dashboard.services, HealthDegraded)
		watch = count_health(dashboard.services, HealthWatch)
		tone_for_counts(degraded, watch)
	}

	phase_status_item : Dashboard -> StatusItem
	phase_status_item = |dashboard| {
		ok = count_health(dashboard.services, HealthOk)
		total = List.len(dashboard.services)
		health_summary = "${ok.to_str()}/${total.to_str()} services ok"
		{ id: "phase", label: "Phase", value: phase_text(dashboard.phase), detail: "${incident_count_for_phase(dashboard.phase).to_str()} incidents | ${health_summary}" }
	}

	system_status_item : Dashboard -> StatusItem
	system_status_item = |dashboard| {
		degraded = count_health(dashboard.services, HealthDegraded)
		watch = count_health(dashboard.services, HealthWatch)
		ok = count_health(dashboard.services, HealthOk)
		total = List.len(dashboard.services)
		health_summary = "${ok.to_str()}/${total.to_str()} services ok"
		{ id: "system", label: "System", value: overall_text(degraded, watch), detail: "${health_summary} | ${watch.to_str()} watch" }
	}

	traffic_status_item : Dashboard -> StatusItem
	traffic_status_item = |dashboard| { id: "traffic", label: "Traffic", value: metric_text(dashboard.traffic.requests_per_minute, " rpm"), detail: traffic_detail(dashboard) }

	queue_status_item : Dashboard -> StatusItem
	queue_status_item = |dashboard| { id: "queue", label: "Queue", value: metric_text(dashboard.queue.depth, " queued"), detail: queue_detail(dashboard) }

	status_strip : Dashboard -> StatusStrip
	status_strip = |dashboard| {
		{
			tone: DashboardView.status_tone(dashboard),
			items: [
				DashboardView.phase_status_item(dashboard),
				DashboardView.system_status_item(dashboard),
				DashboardView.traffic_status_item(dashboard),
				DashboardView.queue_status_item(dashboard),
			],
		}
	}

	system_metric : Dashboard -> Metric
	system_metric = |dashboard| {
		degraded = count_health(dashboard.services, HealthDegraded)
		watch = count_health(dashboard.services, HealthWatch)
		ok = count_health(dashboard.services, HealthOk)
		total = List.len(dashboard.services)
		phase = phase_text(dashboard.phase)
		overall = overall_text(degraded, watch)
		health_summary = "${ok.to_str()}/${total.to_str()} services ok"
		{ id: "system", label: "System state", value: overall, detail: "${health_summary} | ${phase}", tone: tone_for_counts(degraded, watch) }
	}

	ingress_metric : Dashboard -> Metric
	ingress_metric = |dashboard| { id: "ingress", label: "Ingress", value: metric_text(dashboard.traffic.requests_per_minute, " rpm"), detail: traffic_detail(dashboard), tone: ToneInfo }

	latency_metric : Dashboard -> Metric
	latency_metric = |dashboard| { id: "latency", label: "API latency", value: metric_text(dashboard.traffic.latency_ms, " ms"), detail: latency_detail(dashboard), tone: DashboardView.status_tone(dashboard) }

	queue_metric : Dashboard -> Metric
	queue_metric = |dashboard| { id: "queue", label: "Job queue", value: metric_text(dashboard.queue.depth, " queued"), detail: queue_detail(dashboard), tone: DashboardView.status_tone(dashboard) }

	budget_metric : Dashboard -> Metric
	budget_metric = |dashboard| { id: "budget", label: "Error budget", value: permille_text(dashboard.budget.remaining_permille), detail: budget_detail(dashboard), tone: DashboardView.status_tone(dashboard) }

	metrics : Dashboard -> List(Metric)
	metrics = |dashboard| {
		[
			DashboardView.system_metric(dashboard),
			DashboardView.ingress_metric(dashboard),
			DashboardView.latency_metric(dashboard),
			DashboardView.queue_metric(dashboard),
			DashboardView.budget_metric(dashboard),
		]
	}

	ingress_traffic_row : Dashboard -> TrafficRow
	ingress_traffic_row = |dashboard|
		{
			id: "ingress",
			label: "Ingress",
			value: metric_text(dashboard.traffic.requests_per_minute, " rpm"),
			bar: bar_text(dashboard.traffic.ingress_bar_code),
			note: "+${dashboard.traffic.delta_percent.to_str()}% over 5m",
		}

	latency_traffic_row : Dashboard -> TrafficRow
	latency_traffic_row = |dashboard|
		{
			id: "latency",
			label: "API p95",
			value: metric_text(dashboard.traffic.latency_ms, " ms"),
			bar: bar_text(dashboard.traffic.latency_bar_code),
			note: "target ${dashboard.traffic.latency_target_ms.to_str()} ms",
		}

	error_traffic_row : Dashboard -> TrafficRow
	error_traffic_row = |dashboard|
		{
			id: "error-rate",
			label: "Error rate",
			value: permille_text(dashboard.budget.error_permille),
			bar: bar_text(dashboard.traffic.error_bar_code),
			note: "burn ${rate_x_text(dashboard.budget.burn_rate_x10)}",
		}

	webhooks_traffic_row : Dashboard -> TrafficRow
	webhooks_traffic_row = |dashboard|
		{
			id: "webhooks",
			label: "Webhook fanout",
			value: metric_text(dashboard.traffic.webhook_rpm, " rpm"),
			bar: bar_text(dashboard.traffic.webhook_bar_code),
			note: "fanout and retry pool",
		}

	db_writes_traffic_row : Dashboard -> TrafficRow
	db_writes_traffic_row = |dashboard|
		{
			id: "db-writes",
			label: "DB writes",
			value: metric_text(dashboard.traffic.db_write_rpm, " rpm"),
			bar: bar_text(dashboard.traffic.db_write_bar_code),
			note: "replica lag in service matrix",
		}

	traffic_rows : Dashboard -> List(TrafficRow)
	traffic_rows = |dashboard|
		[
			DashboardView.ingress_traffic_row(dashboard),
			DashboardView.latency_traffic_row(dashboard),
			DashboardView.error_traffic_row(dashboard),
			DashboardView.webhooks_traffic_row(dashboard),
			DashboardView.db_writes_traffic_row(dashboard),
		]

	service_rows : Dashboard -> List(ServiceRow)
	service_rows = |dashboard| List.map(dashboard.services, service_row)

	job_rows : Dashboard -> List(JobRow)
	job_rows = |dashboard| List.map(dashboard.jobs, job_row)

	alert_rows : Dashboard -> List(AlertRow)
	alert_rows = |dashboard| List.map(dashboard.alerts, alert_row)

	manual_refresh_text : U64 -> Str
	manual_refresh_text = |count| "Manual refreshes: ${count.to_str()} | auto refresh 2s"
}

traffic_detail : Dashboard -> Str
traffic_detail = |dashboard|
	join_with(
		" | ",
		[
			"+${dashboard.traffic.delta_percent.to_str()}% over 5m",
			"webhooks ${dashboard.traffic.webhook_rpm.to_str()} rpm",
			"db writes ${dashboard.traffic.db_write_rpm.to_str()} rpm",
		],
	)

latency_detail : Dashboard -> Str
latency_detail = |dashboard|
	join_with(
		" | ",
		[
			"target ${dashboard.traffic.latency_target_ms.to_str()} ms",
			bar_text(dashboard.traffic.latency_bar_code),
		],
	)

queue_detail : Dashboard -> Str
queue_detail = |dashboard|
	join_with(
		" | ",
		[
			queue_trend_text(dashboard.queue.trend),
			"${dashboard.queue.running_jobs.to_str()} running",
			"${dashboard.queue.blocked_jobs.to_str()} blocked",
			"oldest ${dashboard.queue.oldest_job_min.to_str()}m",
		],
	)

budget_detail : Dashboard -> Str
budget_detail = |dashboard|
	join_with(
		" | ",
		[
			"burn ${rate_x_text(dashboard.budget.burn_rate_x10)}",
			"errors ${permille_text(dashboard.budget.error_permille)}",
			bar_text(dashboard.budget.bar_code),
		],
	)

service_row : Dashboard.Service -> DashboardView.ServiceRow
service_row = |service| {
	detail = if service.latency_ms == 0 { service.detail } else { "p95 ${service.latency_ms.to_str()} ms | ${service.detail}" }
	{ id: service.id, label: service.label, state: health_text(service.health), detail, tone: tone_for_health(service.health) }
}

job_row : Dashboard.Job -> DashboardView.JobRow
job_row = |job|
	{
		id: job.id,
		label: job.label,
		owner: job.owner,
		run_id: "job-${three_digits(job.run_id)}",
		state: job_state_text(job.state),
		progress: "${job.progress.to_str()}% ${progress_bar_text(job.progress)}",
		age: "${job.age_min.to_str()}m",
		tone: tone_for_job(job.state),
	}

alert_row : Dashboard.Alert -> DashboardView.AlertRow
alert_row = |alert|
	{
		id: alert.id,
		severity: alert_severity(alert.kind),
		service: alert_service(alert.kind),
		state: alert_state(alert.kind),
		age: "${alert.age_min.to_str()}m",
		summary: alert_summary(alert.kind),
		tone: tone_for_alert(alert.kind),
	}

count_health : List(Dashboard.Service), Dashboard.Health -> U64
count_health = |services, target|
	List.fold(
		services,
		0,
		|count, service|
			if service.health == target {
				count + 1
			} else {
				count
			},
	)

tone_for_counts : U64, U64 -> DashboardTheme.Tone
tone_for_counts = |degraded, watch|
	if degraded > 0 {
		ToneBad
	} else if watch > 0 {
		ToneWatch
	} else {
		ToneGood
	}

tone_for_health : Dashboard.Health -> DashboardTheme.Tone
tone_for_health = |health|
	match health {
		HealthDegraded => ToneBad
		HealthWatch => ToneWatch
		HealthOk => ToneGood
	}

tone_for_job : Dashboard.JobState -> DashboardTheme.Tone
tone_for_job = |state|
	match state {
		JobBlocked => ToneBad
		JobRetrying => ToneWatch
		JobQueued => ToneNeutral
		JobRunning => ToneNeutral
	}

tone_for_alert : Dashboard.AlertKind -> DashboardTheme.Tone
tone_for_alert = |kind|
	match kind {
		AlertCriticalCheckout => ToneBad
		AlertWorkerQueue => ToneWatch
		AlertEdgeCanary => ToneNeutral
		AlertPaymentRecovering => ToneNeutral
		AlertEdgeSteady => ToneNeutral
	}

incident_count_for_phase : Dashboard.Phase -> U64
incident_count_for_phase = |phase|
	match phase {
		PhaseActiveIncident => 1
		_ => 0
	}

overall_text : U64, U64 -> Str
overall_text = |degraded, watch|
	if degraded > 0 {
		"Degraded"
	} else if watch > 0 {
		"Watch"
	} else {
		"Nominal"
	}

clock_text : Dashboard.Clock -> Str
clock_text = |clock| "${two_digits(clock.hour)}:${two_digits(clock.minute)}:${two_digits(clock.second)} UTC"

phase_text : Dashboard.Phase -> Str
phase_text = |phase|
	match phase {
		PhaseActiveIncident => "Active incident"
		PhaseRecovering => "Recovering"
		PhaseWatch => "Watch"
		PhaseSteady => "Steady"
	}

health_text : Dashboard.Health -> Str
health_text = |health|
	match health {
		HealthDegraded => "degraded"
		HealthWatch => "watch"
		HealthOk => "ok"
	}

queue_trend_text : Dashboard.QueueTrend -> Str
queue_trend_text = |trend|
	match trend {
		TrendRising => "rising"
		TrendDraining => "draining"
		TrendSteady => "steady"
	}

job_state_text : Dashboard.JobState -> Str
job_state_text = |state|
	match state {
		JobBlocked => "blocked"
		JobRetrying => "retrying"
		JobQueued => "queued"
		JobRunning => "running"
	}

alert_severity : Dashboard.AlertKind -> Str
alert_severity = |kind|
	match kind {
		AlertCriticalCheckout => "critical"
		AlertWorkerQueue => "warning"
		AlertEdgeCanary => "info"
		AlertPaymentRecovering => "info"
		AlertEdgeSteady => "info"
	}

alert_service : Dashboard.AlertKind -> Str
alert_service = |kind|
	match kind {
		AlertCriticalCheckout => "payments-api"
		AlertWorkerQueue => "workers"
		AlertEdgeCanary => "edge"
		AlertPaymentRecovering => "payments-api"
		AlertEdgeSteady => "edge"
	}

alert_state : Dashboard.AlertKind -> Str
alert_state = |kind|
	match kind {
		AlertCriticalCheckout => "active"
		AlertWorkerQueue => "monitoring"
		AlertEdgeCanary => "monitoring"
		AlertPaymentRecovering => "recovering"
		AlertEdgeSteady => "steady"
	}

alert_summary : Dashboard.AlertKind -> Str
alert_summary = |kind|
	match kind {
		AlertCriticalCheckout => "Checkout latency above SLO"
		AlertWorkerQueue => "Queue age approaching cap"
		AlertEdgeCanary => "Canary pool shifted 10 percent"
		AlertPaymentRecovering => "Error budget burn below 1x"
		AlertEdgeSteady => "Canary pool normal"
	}

two_digits : U64 -> Str
two_digits = |value|
	if value < 10 {
		"0${value.to_str()}"
	} else {
		value.to_str()
	}

three_digits : U64 -> Str
three_digits = |value|
	if value < 10 {
		"00${value.to_str()}"
	} else if value < 100 {
		"0${value.to_str()}"
	} else {
		value.to_str()
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
permille_text = |value| "${(value // 10).to_str()}.${(value % 10).to_str()}%"

rate_x_text : U64 -> Str
rate_x_text = |value| "${(value // 10).to_str()}.${(value % 10).to_str()}x"

metric_text : U64, Str -> Str
metric_text = |value, suffix| "${value.to_str()}${suffix}"

join_with : Str, List(Str) -> Str
join_with = |separator, parts|
	List.fold(
		parts,
		"",
		|acc, part|
			if Str.is_empty(acc) {
				part
			} else {
				"${acc}${separator}${part}"
			},
	)
