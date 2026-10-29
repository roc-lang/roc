# Passes the cached Form.submit through an open-row annotation into Api.status_of.
app [main!] { pf: platform "../../fx-open/platform/main.roc", pkg: "pkg/main.roc" }

import pf.Stdout
import pkg.Api
import pkg.Form

Msg : [SubmitDone(Try({}, Form.SubmitError))]

main! = |args| {
	Stdout.line!(update("", SubmitDone(Err(Invalid(Str.inspect(args.len()))))))
	Ok({})
}

update : Str, Msg -> Str
update = |model, msg|
	match msg {
		SubmitDone(Err(problem)) => problem_detail(Form.submit, problem)
		_ => model
	}

problem_detail : Api.Endpoint(request, answer, [Server(Api.Failure), ..others]), [Server(Api.Failure), ..others] -> Str
problem_detail = |submitted_to, error| Api.status_of(submitted_to, error).to_str()
