app [main] { pf: platform "../platform/main.roc" }

import Dashboard
import DashboardRemote
import DashboardTheme
import DashboardView
import pf.Elem exposing [Elem]
import pf.Html
import pf.Http
import pf.Signal
import pf.Ui

increment_u64 : U64 -> U64
increment_u64 = |current| current + 1

field : Signal.Signal(r), (r -> a) -> Signal.Signal(a)
	where [
		a.is_eq : a, a -> Bool,
	]
field = |source, select| Signal.map(source, select)

select_remote : Signal.Signal(Dashboard.State), (Dashboard -> a) -> Signal.Signal(DashboardRemote(a))
	where [
		a.is_eq : a, a -> Bool,
	]
select_remote = |state, select| Signal.map(state, |value| DashboardRemote.from_state(value, select))

ready_list : Signal.Signal(DashboardRemote(List(a))) -> Signal.Signal(List(a))
	where [
		a.is_eq : a, a -> Bool,
	]
ready_list = |remote|
	field(
		remote,
		|value|
			match value {
				RemoteReady(items) => items
				_ => []
			},
	)

remote_is_ready = |remote| field(remote, DashboardRemote.is_ready)

remote_tone = |remote|
	field(
		remote,
		|value|
			if DashboardRemote.is_failed(value) {
				ToneError
			} else {
				ToneNeutral
			},
	)

remote_message = |label, remote| {
	message = field(remote, DashboardRemote.message)
	classes = field(remote_tone(remote), DashboardTheme.remote_inline_class)

	Html.div_sc(
		classes,
		[
			Html.div_c(DashboardTheme.metric_label_class, [Html.text(label)]),
			Html.div_c(DashboardTheme.metric_detail_class, [Html.text_s(message)]),
		],
	)
}

render_panel : Str, Str, List(Elem) -> Elem
render_panel = |region_label, heading, children|
	Html.section_c(
		region_label,
		DashboardTheme.panel_class,
		List.concat(
			[Html.heading_c(heading, DashboardTheme.section_heading_class)],
			children,
		),
	)

render_status_item : Str, Signal.Signal(DashboardView.StatusItem) -> Elem
render_status_item = |_key, item| {
	label = field(item, |row| row.label)
	value = field(item, |row| row.value)
	detail = field(item, |row| row.detail)

	Html.div_c(
		DashboardTheme.status_item_class,
		[
			Html.div_c(DashboardTheme.status_label_class, [Html.text_s(label)]),
			Html.div_c(DashboardTheme.status_value_class, [Html.text_s(value)]),
			Html.div_c(DashboardTheme.status_detail_class, [Html.text_s(detail)]),
		],
	)
}

render_metric : Str, Signal.Signal(DashboardView.Metric) -> Elem
render_metric = |_key, metric| {
	label = field(metric, |row| row.label)
	value = field(metric, |row| row.value)
	detail = field(metric, |row| row.detail)
	classes = field(metric, |row| DashboardTheme.metric_class(row.tone))

	Html.div_sc(
		classes,
		[
			Html.div_c(DashboardTheme.metric_label_class, [Html.text_s(label)]),
			Html.div_c(DashboardTheme.metric_value_class, [Html.text_s(value)]),
			Html.div_c(DashboardTheme.metric_detail_class, [Html.text_s(detail)]),
		],
	)
}

render_traffic_row : Str, Signal.Signal(DashboardView.TrafficRow) -> Elem
render_traffic_row = |_key, row| {
	label = field(row, |item| item.label)
	value = field(row, |item| item.value)
	bar = field(row, |item| item.bar)
	note = field(row, |item| item.note)

	Html.div_c(
		DashboardTheme.traffic_row_class,
		[
			Html.div_c(DashboardTheme.row_label_class, [Html.text_s(label)]),
			Html.div_c(DashboardTheme.row_value_class, [Html.text_s(value)]),
			Html.div_c(DashboardTheme.row_bar_class, [Html.text_s(bar)]),
			Html.div_c(DashboardTheme.row_note_class, [Html.text_s(note)]),
		],
	)
}

render_service_cell : Str, Signal.Signal(DashboardView.ServiceRow) -> Elem
render_service_cell = |_key, service| {
	label = field(service, |row| row.label)
	state = field(service, |row| row.state)
	detail = field(service, |row| row.detail)
	classes = field(service, |row| DashboardTheme.service_class(row.tone))

	Html.div_sc(
		classes,
		[
			Html.div_c(
				DashboardTheme.row_header_class,
				[
					Html.div_c(DashboardTheme.strong_text_class, [Html.text_s(label)]),
					Html.div_c(DashboardTheme.mono_xs_class, [Html.text_s(state)]),
				],
			),
			Html.div_c(DashboardTheme.text_sm_class, [Html.text_s(detail)]),
		],
	)
}

render_job_row : Str, Signal.Signal(DashboardView.JobRow) -> Elem
render_job_row = |_key, job| {
	label = field(job, |row| row.label)
	run_id = field(job, |row| row.run_id)
	state = field(job, |row| row.state)
	progress = field(job, |row| row.progress)
	age = field(job, |row| row.age)
	owner = field(job, |row| row.owner)
	classes = field(job, |row| DashboardTheme.job_class(row.tone))

	Html.div_sc(
		classes,
		[
			Html.div_c(
				DashboardTheme.wrap_row_header_class,
				[
					Html.div_c(DashboardTheme.strong_text_class, [Html.text_s(label)]),
					Html.div_c(DashboardTheme.mono_xs_class, [Html.text_s(run_id)]),
				],
			),
			Html.div_c(
				DashboardTheme.job_detail_grid_class,
				[
					Html.div_c(DashboardTheme.text_sm_class, [Html.text_s(state)]),
					Html.div_c(DashboardTheme.mono_sm_class, [Html.text_s(progress)]),
					Html.div_c(DashboardTheme.text_sm_class, [Html.text_s(age)]),
					Html.div_c(DashboardTheme.text_sm_class, [Html.text_s(owner)]),
				],
			),
		],
	)
}

render_alert_row : Str, Signal.Signal(DashboardView.AlertRow) -> Elem
render_alert_row = |_key, alert| {
	severity = field(alert, |row| row.severity)
	service = field(alert, |row| row.service)
	state = field(alert, |row| row.state)
	age = field(alert, |row| row.age)
	summary = field(alert, |row| row.summary)
	classes = field(alert, |row| DashboardTheme.alert_class(row.tone))

	Html.div_sc(
		classes,
		[
			Html.div_c(
				DashboardTheme.inline_header_class,
				[
					Html.div_c(DashboardTheme.strong_text_class, [Html.text_s(severity)]),
					Html.div_c(DashboardTheme.mono_xs_class, [Html.text_s(service)]),
					Html.div_c(DashboardTheme.text_xs_class, [Html.text_s(age)]),
				],
			),
			Html.div_c(DashboardTheme.text_sm_class, [Html.text_s(summary)]),
			Html.div_c(DashboardTheme.text_xs_class, [Html.text_s(state)]),
		],
	)
}

render_status_items : Signal.Signal(List(DashboardView.StatusItem)) -> Elem
render_status_items = |items| Ui.each(items, |item| item.id, Ui.str_key_hash, render_status_item)

render_metrics : Signal.Signal(List(DashboardView.Metric)) -> Elem
render_metrics = |items| Ui.each(items, |item| item.id, Ui.str_key_hash, render_metric)

render_traffic_rows : Signal.Signal(List(DashboardView.TrafficRow)) -> Elem
render_traffic_rows = |items| Ui.each(items, |item| item.id, Ui.str_key_hash, render_traffic_row)

render_service_rows : Signal.Signal(List(DashboardView.ServiceRow)) -> Elem
render_service_rows = |items| Ui.each(items, |item| item.id, Ui.str_key_hash, render_service_cell)

render_job_rows : Signal.Signal(List(DashboardView.JobRow)) -> Elem
render_job_rows = |items| Ui.each(items, |item| item.id, Ui.str_key_hash, render_job_row)

render_alert_rows : Signal.Signal(List(DashboardView.AlertRow)) -> Elem
render_alert_rows = |items| Ui.each(items, |item| item.id, Ui.str_key_hash, render_alert_row)

status_strip_items : Signal.Signal(DashboardRemote(DashboardView.StatusStrip)) -> Signal.Signal(List(DashboardView.StatusItem))
status_strip_items = |remote|
	field(
		remote,
		|value|
			match value {
				RemoteReady(strip) => strip.items
				_ => []
			},
	)

status_strip_tone : Signal.Signal(DashboardRemote(DashboardView.StatusStrip)) -> Signal.Signal(DashboardTheme.Tone)
status_strip_tone = |remote|
	field(
		remote,
		|value|
			match value {
				RemoteReady(strip) => strip.tone
				RemoteFailed(_) => ToneError
				_ => ToneNeutral
			},
	)

status_strip : Signal.Signal(DashboardRemote(DashboardView.StatusStrip)) -> Elem
status_strip = |status| {
	is_ready = remote_is_ready(status)
	items = status_strip_items(status)
	classes = field(status_strip_tone(status), DashboardTheme.status_strip_class)

	Ui.component(
		|_|
			Html.div_sc(
				classes,
				[
					Ui.when(
						is_ready,
						|_| render_status_items(items),
						|_| remote_message("Status", status),
					),
				],
			),
	)
}

metric_grid = |metrics| {
	is_ready = remote_is_ready(metrics)
	items = ready_list(metrics)

	Ui.component(
		|_|
			Ui.when(
				is_ready,
				|_| Html.div_c(DashboardTheme.metric_grid_class, [render_metrics(items)]),
				|_| Html.div_c(DashboardTheme.metric_grid_class, [remote_message("Metrics", metrics)]),
			),
	)
}

traffic_panel = |traffic| {
	is_ready = remote_is_ready(traffic)
	rows = ready_list(traffic)

	Ui.component(
		|_|
			render_panel(
				"Traffic",
				"Traffic and pressure",
				[
					Ui.when(
						is_ready,
						|_| render_traffic_rows(rows),
						|_| remote_message("Traffic", traffic),
					),
				],
			),
	)
}

services_panel = |services| {
	is_ready = remote_is_ready(services)
	rows = ready_list(services)

	Ui.component(
		|_|
			render_panel(
				"Service health",
				"Service matrix",
				[
					Ui.when(
						is_ready,
						|_| Html.div_c(DashboardTheme.service_grid_class, [render_service_rows(rows)]),
						|_| remote_message("Services", services),
					),
				],
			),
	)
}

jobs_panel = |jobs| {
	is_ready = remote_is_ready(jobs)
	rows = ready_list(jobs)

	Ui.component(
		|_|
			render_panel(
				"Active jobs",
				"Active jobs",
				[
					Ui.when(
						is_ready,
						|_| render_job_rows(rows),
						|_| remote_message("Jobs", jobs),
					),
				],
			),
	)
}

alerts_panel = |alerts| {
	is_ready = remote_is_ready(alerts)
	rows = ready_list(alerts)

	Ui.component(
		|_|
			render_panel(
				"Alerts",
				"Incidents and alerts",
				[
					Ui.when(
						is_ready,
						|_| render_alert_rows(rows),
						|_| remote_message("Alerts", alerts),
					),
				],
			),
	)
}

toolbar = |last_updated, manual_refresh_text, refresh_now|
	Html.div_c(
		DashboardTheme.toolbar_class,
		[
			Html.div_c(
				DashboardTheme.toolbar_text_class,
				[
					Html.heading_c("Ops dashboard", DashboardTheme.app_heading_class),
					Html.div_c(DashboardTheme.toolbar_status_class, [Html.text_s(last_updated)]),
				],
			),
			Html.div_c(
				DashboardTheme.toolbar_actions_class,
				[
					Html.text_s(manual_refresh_text),
					Html.button_c("Refresh now", DashboardTheme.primary_button_class, refresh_now),
				],
			),
		],
	)

dashboard_page = |dashboard_state, manual_refresh_text, refresh_now, lifecycle| {
	last_updated = field(select_remote(dashboard_state, DashboardView.last_updated), DashboardRemote.text)
	status = select_remote(dashboard_state, DashboardView.status_strip)
	metrics = select_remote(dashboard_state, DashboardView.metrics)
	traffic = select_remote(dashboard_state, DashboardView.traffic_rows)
	services = select_remote(dashboard_state, DashboardView.service_rows)
	jobs = select_remote(dashboard_state, DashboardView.job_rows)
	alerts = select_remote(dashboard_state, DashboardView.alert_rows)

	Ui.component(
		|_|
			Html.div_c(
				DashboardTheme.page_class,
					[
						Html.div_c(
							DashboardTheme.shell_class,
							List.concat(
								[
									toolbar(last_updated, manual_refresh_text, refresh_now),
									status_strip(status),
									metric_grid(metrics),
									Html.div_c(
										DashboardTheme.main_grid_class,
										[
											Html.div_c(DashboardTheme.wide_column_class, [traffic_panel(traffic), services_panel(services)]),
											Html.div_c(DashboardTheme.side_column_class, [jobs_panel(jobs), alerts_panel(alerts)]),
										],
									),
								],
								lifecycle,
							),
						),
					],
				),
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
					Dashboard.loading,
					Dashboard.decode,
					Dashboard.request_failed,
				)

			manual_refresh_text = field(manual_refresh.signal(), DashboardView.manual_refresh_text)
			dashboard_page(
				dashboard_state,
				manual_refresh_text,
				manual_refresh.on_unit(increment_u64),
				[
					Ui.on_mount(|_| Http.get_text(dashboard_task, "/api/ops/dashboard")),
					Ui.on_change(refresh_request, |_| Http.get_text(dashboard_task, "/api/ops/dashboard")),
				],
			)
		},
	)
}
