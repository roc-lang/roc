import Dashboard

DashboardRemote(a) := [RemoteLoading, RemoteReady(a), RemoteEmpty, RemoteFailed(Str)].{
	from_state : Dashboard.State, (Dashboard -> a) -> DashboardRemote(a)
	from_state = |state, select|
		match state {
			Loading => RemoteLoading
			Ready(dashboard) => RemoteReady(select(dashboard))
			RequestFailed(err) => RemoteFailed("Request failed: ${err}")
			DecodeFailed(err) => RemoteFailed("Decode failed: ${parse_error_text(err)}")
		}

	is_ready : DashboardRemote(a) -> Bool
	is_ready = |remote|
		match remote {
			RemoteReady(_) => True
			_ => False
		}

	is_failed : DashboardRemote(a) -> Bool
	is_failed = |remote|
		match remote {
			RemoteFailed(_) => True
			_ => False
		}

	message : DashboardRemote(a) -> Str
	message = |remote|
		match remote {
			RemoteLoading => "Waiting for first server response"
			RemoteReady(_) => ""
			RemoteEmpty => "No data returned"
			RemoteFailed(detail) => detail
		}

	text : DashboardRemote(Str) -> Str
	text = |remote|
		match remote {
			RemoteReady(value) => value
			_ => DashboardRemote.message(remote)
		}
}

parse_error_text : Dashboard.ParseErr -> Str
parse_error_text = |err|
	match err {
		BadProtocol => "response did not match the dashboard protocol"
		BadNumber => "response had an invalid dashboard number"
		MissingField(name) => "response was missing ${name}"
		UnsupportedSchema(schema) => "unsupported dashboard schema ${schema.to_str()}"
	}
