# `body` and `ran_only` read the `outcome` field of a nominal record twice:
# once to hand it to `format`, which takes ownership of its argument, and
# again to match on it and move the matched variant's payloads into another
# call. Each expect passes only when the second read still sees the field.

Output :: { outcome : Outcome }.{
	Format := [Text, Json]

	Request :: { mode : Str, directory : Str, format : Format }.{
		parts : Request -> { mode : Str, directory : Str, format : Format }
		parts = |request| { mode: request.mode, directory: request.directory, format: request.format }
	}

	Receipt : { ok : Bool, roots : List(Str), logs : Str }

	Outcome := [
		Help(Format),
		Checked(Request, Str),
		Ran(Request, Str, List(Str), Receipt),
		Failed(Str, Format),
	]

	new : Outcome -> Output
	new = |outcome| { outcome: outcome }

	body : Output -> Str
	body = |response| match format(response.outcome) {
		Format.Json => json(response.outcome)
		Format.Text => match response.outcome {
			Outcome.Help(_) => "help"
			Outcome.Checked(request, path) => checked_text(request, path)
			Outcome.Ran(request, path, spec, receipt) => ran_text(request, path, spec, receipt)
			Outcome.Failed(problem, _) => problem
		}
	}
}

format : Output.Outcome -> Output.Format
format = |outcome| match outcome {
	Output.Outcome.Help(value) | Output.Outcome.Failed(_, value) => value
	Output.Outcome.Checked(request, _) => request.parts().format
	Output.Outcome.Ran(request, _, _, _) => request.parts().format
}

json : Output.Outcome -> Str
json = |outcome| match outcome {
	Output.Outcome.Help(_) => "{}"
	Output.Outcome.Checked(request, path) => "{\"checked\":\"${checked_text(request, path)}\"}"
	Output.Outcome.Ran(request, path, spec, receipt) => "{\"ran\":\"${ran_text(request, path, spec, receipt)}\"}"
	Output.Outcome.Failed(problem, _) => "{\"failed\":\"${problem}\"}"
}

checked_text : Output.Request, Str -> Str
checked_text = |request, path| "${request.parts().mode} ${path}"

ran_text : Output.Request, Str, List(Str), Output.Receipt -> Str
ran_text = |request, path, spec, receipt| {
	status = if receipt.ok "ok" else "failed"
	"${request.parts().mode} ${path} ${Str.join_with(spec, ",")} ${Str.join_with(receipt.roots, ",")} ${receipt.logs} ${status}"
}

ran_only : Output -> Str
ran_only = |response| {
	_ = format(response.outcome)
	match response.outcome {
		Output.Outcome.Ran(request, path, spec, receipt) => ran_text(request, path, spec, receipt)
		_ => ""
	}
}

request_for : Output.Format -> Output.Request
request_for = |wanted| Output.Request.{ mode: "plan", directory: "dir", format: wanted }

sample_receipt : Output.Receipt
sample_receipt = { ok: Bool.True, roots: ["project", "dns"], logs: "/o" }

ran : Output.Format -> Output
ran = |wanted| Output.new(Output.Outcome.Ran(request_for(wanted), "d/day2.json", ["a", "b"], sample_receipt))

expect ran(Output.Format.Text).body() == "plan d/day2.json a,b project,dns /o ok"

expect ran(Output.Format.Json).body() == "{\"ran\":\"plan d/day2.json a,b project,dns /o ok\"}"

expect Output.new(Output.Outcome.Checked(request_for(Output.Format.Text), "d/day2.json")).body() == "plan d/day2.json"

expect ran_only(ran(Output.Format.Json)) == "plan d/day2.json a,b project,dns /o ok"

expect {
	response = Output.new(Output.Outcome.Failed("problem", Output.Format.Text))
	response.body() == "problem" and ran_only(response) == ""
}
