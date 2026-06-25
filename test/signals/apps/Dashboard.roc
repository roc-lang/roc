Dashboard := {
	schema : U64,
	version : U64,
	updated : Dashboard.Clock,
	phase : Dashboard.Phase,
	traffic : Dashboard.Traffic,
	budget : Dashboard.Budget,
	queue : Dashboard.Queue,
	services : List(Dashboard.Service),
	jobs : List(Dashboard.Job),
	alerts : List(Dashboard.Alert),
}.{
	ParseErr : [BadProtocol, BadNumber, MissingField(Str), UnsupportedSchema(U64)]

	State := [Loading, Ready(Dashboard), RequestFailed(Str), DecodeFailed(ParseErr)]

	Clock : { hour : U64, minute : U64, second : U64 }

	Phase : [PhaseSteady, PhaseWatch, PhaseActiveIncident, PhaseRecovering]

	Health : [HealthOk, HealthWatch, HealthDegraded]

	QueueTrend : [TrendDraining, TrendSteady, TrendRising]

	JobState : [JobRunning, JobQueued, JobRetrying, JobBlocked]

	AlertKind : [AlertCriticalCheckout, AlertWorkerQueue, AlertEdgeCanary, AlertPaymentRecovering, AlertEdgeSteady]

	Traffic : {
		requests_per_minute : U64,
		delta_percent : U64,
		latency_ms : U64,
		latency_target_ms : U64,
		webhook_rpm : U64,
		db_write_rpm : U64,
		ingress_bar_code : U64,
		latency_bar_code : U64,
		error_bar_code : U64,
		webhook_bar_code : U64,
		db_write_bar_code : U64,
	}

	Budget : {
		remaining_permille : U64,
		burn_rate_x10 : U64,
		error_permille : U64,
		bar_code : U64,
	}

	Queue : {
		depth : U64,
		trend : QueueTrend,
		capacity : U64,
		running_jobs : U64,
		blocked_jobs : U64,
		oldest_job_min : U64,
	}

	Service : {
		id : Str,
		label : Str,
		health : Health,
		latency_ms : U64,
		detail : Str,
	}

	Job : {
		id : Str,
		label : Str,
		owner : Str,
		run_id : U64,
		state : JobState,
		progress : U64,
		age_min : U64,
	}

	Alert : {
		id : Str,
		kind : AlertKind,
		age_min : U64,
	}

	decode : Str -> State
	decode = |body|
		match parse_dashboard(body) {
			Ok(dashboard) => Ready(dashboard)
			Err(err) => DecodeFailed(err)
		}

	request_failed : Str -> State
	request_failed = |err| RequestFailed(err)

	loading : State
	loading = Loading
}

parse_dashboard : Str -> Try(Dashboard, Dashboard.ParseErr)
parse_dashboard = |body| {
	fields = parse_fields(body)?
	schema = get_u64(fields, "schema")?
	if schema != 1 {
		return Err(UnsupportedSchema(schema))
	}

	updated_hour = get_u64(fields, "updated_hour")?
	updated_minute = get_u64(fields, "updated_minute")?
	updated_second = get_u64(fields, "updated_second")?
	phase = decode_phase(get_u64(fields, "phase_code")?)?
	queue_trend = decode_queue_trend(get_u64(fields, "queue_trend_code")?)?

	edge_health = decode_health(get_u64(fields, "edge_state_code")?)?
	api_health = decode_health(get_u64(fields, "api_state_code")?)?
	worker_health = decode_health(get_u64(fields, "worker_state_code")?)?
	database_health = decode_health(get_u64(fields, "database_state_code")?)?
	billing_health = decode_health(get_u64(fields, "billing_state_code")?)?
	search_health = decode_health(get_u64(fields, "search_state_code")?)?
	identity_health = decode_health(get_u64(fields, "identity_state_code")?)?

	job_a_state = decode_job_state(get_u64(fields, "job_a_state_code")?)?
	job_b_state = decode_job_state(get_u64(fields, "job_b_state_code")?)?
	job_c_state = decode_job_state(get_u64(fields, "job_c_state_code")?)?
	job_d_state = decode_job_state(get_u64(fields, "job_d_state_code")?)?

	alert_a_kind = decode_alert_kind(get_u64(fields, "alert_a_code")?)?
	alert_b_kind = decode_alert_kind(get_u64(fields, "alert_b_code")?)?
	alert_c_kind = decode_alert_kind(get_u64(fields, "alert_c_code")?)?

	queue_depth = get_u64(fields, "queue_depth")?
	worker_oldest_job_min = get_u64(fields, "worker_oldest_job_min")?

	Ok(
		{
			schema,
			version: get_u64(fields, "updated_version")?,
			updated: { hour: updated_hour, minute: updated_minute, second: updated_second },
			phase,
			traffic: {
				requests_per_minute: get_u64(fields, "requests_per_minute")?,
				delta_percent: get_u64(fields, "traffic_delta_percent")?,
				latency_ms: get_u64(fields, "latency_ms")?,
				latency_target_ms: get_u64(fields, "latency_target_ms")?,
				webhook_rpm: get_u64(fields, "webhook_rpm")?,
				db_write_rpm: get_u64(fields, "db_write_rpm")?,
				ingress_bar_code: get_bar_code(fields, "ingress_bar_code")?,
				latency_bar_code: get_bar_code(fields, "latency_bar_code")?,
				error_bar_code: get_bar_code(fields, "error_bar_code")?,
				webhook_bar_code: get_bar_code(fields, "webhook_bar_code")?,
				db_write_bar_code: get_bar_code(fields, "db_write_bar_code")?,
			},
			budget: {
				remaining_permille: get_u64(fields, "budget_remaining_permille")?,
				burn_rate_x10: get_u64(fields, "burn_rate_x10")?,
				error_permille: get_u64(fields, "error_permille")?,
				bar_code: get_bar_code(fields, "budget_bar_code")?,
			},
			queue: {
				depth: queue_depth,
				trend: queue_trend,
				capacity: get_u64(fields, "queue_capacity")?,
				running_jobs: get_u64(fields, "running_jobs")?,
				blocked_jobs: get_u64(fields, "blocked_jobs")?,
				oldest_job_min: get_u64(fields, "oldest_job_min")?,
			},
			services: [
				{
					id: "edge",
					label: "edge",
					health: edge_health,
					latency_ms: get_u64(fields, "edge_latency_ms")?,
					detail: "8 pods",
				},
				{
					id: "api",
					label: "api",
					health: api_health,
					latency_ms: get_u64(fields, "api_latency_ms")?,
					detail: "12 pods",
				},
				{
					id: "workers",
					label: "workers",
					health: worker_health,
					latency_ms: 0,
					detail: "oldest ${worker_oldest_job_min.to_str()}m | ${queue_depth.to_str()} queued",
				},
				{
					id: "database",
					label: "database",
					health: database_health,
					latency_ms: 0,
					detail: "lag ${get_u64(fields, "database_lag_sec")?.to_str()}s | 2 writers",
				},
				{
					id: "billing",
					label: "billing",
					health: billing_health,
					latency_ms: get_u64(fields, "billing_latency_ms")?,
					detail: "webhooks",
				},
				{
					id: "search",
					label: "search",
					health: search_health,
					latency_ms: 0,
					detail: "refresh ${get_u64(fields, "search_refresh_sec")?.to_str()}s | 5 shards",
				},
				{
					id: "identity",
					label: "identity",
					health: identity_health,
					latency_ms: get_u64(fields, "identity_latency_ms")?,
					detail: "session cache",
				},
			],
			jobs: [
				{
					id: "search-index",
					label: "Rebuild search index",
					owner: "workers/search",
					run_id: get_u64(fields, "job_a_id")?,
					state: job_a_state,
					progress: get_u64(fields, "job_a_progress")?,
					age_min: get_u64(fields, "job_a_age_min")?,
				},
				{
					id: "billing-backfill",
					label: "Backfill billing events",
					owner: "billing",
					run_id: get_u64(fields, "job_b_id")?,
					state: job_b_state,
					progress: get_u64(fields, "job_b_progress")?,
					age_min: get_u64(fields, "job_b_age_min")?,
				},
				{
					id: "audit-export",
					label: "Export audit archive",
					owner: "compliance",
					run_id: get_u64(fields, "job_c_id")?,
					state: job_c_state,
					progress: get_u64(fields, "job_c_progress")?,
					age_min: get_u64(fields, "job_c_age_min")?,
				},
				{
					id: "session-prune",
					label: "Prune stale sessions",
					owner: "identity",
					run_id: get_u64(fields, "job_d_id")?,
					state: job_d_state,
					progress: get_u64(fields, "job_d_progress")?,
					age_min: get_u64(fields, "job_d_age_min")?,
				},
			],
			alerts: [
				{ id: alert_id(alert_a_kind), kind: alert_a_kind, age_min: get_u64(fields, "alert_a_age_min")? },
				{ id: alert_id(alert_b_kind), kind: alert_b_kind, age_min: get_u64(fields, "alert_b_age_min")? },
				{ id: alert_id(alert_c_kind), kind: alert_c_kind, age_min: get_u64(fields, "alert_c_age_min")? },
			],
		},
	)
}

Field : { name : List(U8), value : List(U8) }

parse_fields : Str -> Try(List(Field), Dashboard.ParseErr)
parse_fields = |body| {
	var $remaining = Str.to_utf8(body)
	var $fields = []

	while True {
		if List.is_empty($remaining) {
			return Ok($fields)
		}

		line = take_line($remaining)?
		$remaining = line.rest

		if !List.is_empty(line.value) {
			parts = split_once_byte(line.value, 61)?
			$fields = List.append($fields, { name: parts.before, value: parts.after })
		}
	}
}

get_u64 : List(Field), Str -> Try(U64, Dashboard.ParseErr)
get_u64 = |fields, name| {
	raw = get_field(fields, name)?
	bytes_to_u64(raw)
}

get_bar_code : List(Field), Str -> Try(U64, Dashboard.ParseErr)
get_bar_code = |fields, name| {
	value = get_u64(fields, name)?
	if value <= 8 {
		Ok(value)
	} else {
		Err(BadProtocol)
	}
}

get_field : List(Field), Str -> Try(List(U8), Dashboard.ParseErr)
get_field = |fields, name| {
	target = Str.to_utf8(name)
	var $remaining = fields

	while True {
		match List.first($remaining) {
			Ok(field) =>
				if bytes_equal(field.name, target) {
					return Ok(field.value)
				} else {
					$remaining = List.drop_first($remaining, 1)
				}

			Err(_) => return Err(MissingField(name))
		}
	}
}

take_line : List(U8) -> Try({ value : List(U8), rest : List(U8) }, Dashboard.ParseErr)
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

split_once_byte : List(U8), U8 -> Try({ before : List(U8), after : List(U8) }, Dashboard.ParseErr)
split_once_byte = |bytes, delimiter| {
	var $remaining = bytes
	var $before = []

	while True {
		match List.first($remaining) {
			Ok(byte) =>
				if byte == delimiter {
					return Ok({ before: $before, after: List.drop_first($remaining, 1) })
				} else {
					$before = List.append($before, byte)
					$remaining = List.drop_first($remaining, 1)
				}

			Err(_) => return Err(BadProtocol)
		}
	}
}

bytes_equal : List(U8), List(U8) -> Bool
bytes_equal = |left, right| {
	var $left = left
	var $right = right

	while True {
		match List.first($left) {
			Ok(left_byte) =>
				match List.first($right) {
					Ok(right_byte) =>
						if left_byte == right_byte {
							$left = List.drop_first($left, 1)
							$right = List.drop_first($right, 1)
						} else {
							return False
						}

					Err(_) => return False
				}

			Err(_) =>
				match List.first($right) {
					Ok(_) => return False
					Err(_) => return True
				}
		}
	}
}

decode_phase : U64 -> Try(Dashboard.Phase, Dashboard.ParseErr)
decode_phase = |code|
	if code == 0 {
		Ok(PhaseSteady)
	} else if code == 1 {
		Ok(PhaseWatch)
	} else if code == 2 {
		Ok(PhaseActiveIncident)
	} else if code == 3 {
		Ok(PhaseRecovering)
	} else {
		Err(BadProtocol)
	}

decode_health : U64 -> Try(Dashboard.Health, Dashboard.ParseErr)
decode_health = |code|
	if code == 0 {
		Ok(HealthOk)
	} else if code == 1 {
		Ok(HealthWatch)
	} else if code == 2 {
		Ok(HealthDegraded)
	} else {
		Err(BadProtocol)
	}

decode_queue_trend : U64 -> Try(Dashboard.QueueTrend, Dashboard.ParseErr)
decode_queue_trend = |code|
	if code == 0 {
		Ok(TrendDraining)
	} else if code == 1 {
		Ok(TrendSteady)
	} else if code == 2 {
		Ok(TrendRising)
	} else {
		Err(BadProtocol)
	}

decode_job_state : U64 -> Try(Dashboard.JobState, Dashboard.ParseErr)
decode_job_state = |code|
	if code == 0 {
		Ok(JobRunning)
	} else if code == 1 {
		Ok(JobQueued)
	} else if code == 2 {
		Ok(JobRetrying)
	} else if code == 3 {
		Ok(JobBlocked)
	} else {
		Err(BadProtocol)
	}

decode_alert_kind : U64 -> Try(Dashboard.AlertKind, Dashboard.ParseErr)
decode_alert_kind = |code|
	if code == 1 {
		Ok(AlertCriticalCheckout)
	} else if code == 2 {
		Ok(AlertWorkerQueue)
	} else if code == 3 {
		Ok(AlertEdgeCanary)
	} else if code == 4 {
		Ok(AlertPaymentRecovering)
	} else if code == 5 {
		Ok(AlertEdgeSteady)
	} else {
		Err(BadProtocol)
	}

alert_id : Dashboard.AlertKind -> Str
alert_id = |kind|
	match kind {
		AlertCriticalCheckout => "checkout-latency"
		AlertWorkerQueue => "worker-queue"
		AlertEdgeCanary => "edge-canary"
		AlertPaymentRecovering => "payment-recovery"
		AlertEdgeSteady => "edge-steady"
	}

bytes_to_u64 : List(U8) -> Try(U64, Dashboard.ParseErr)
bytes_to_u64 = |bytes| {
	if List.is_empty(bytes) {
		return Err(BadNumber)
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
					return Err(BadNumber)
				}

			Err(_) => return Ok($value)
		}
	}
}
