app [main!] {
	pf: platform "./platform/main.roc",
	pkg: "./issue_12693_pkg/main.roc",
}

import pf.Stdout
import pkg.Api
import pkg.Form

Msg : [SubmitDone(Try({}, Form.SubmitError))]

main! = || Stdout.line!("${update("", SubmitDone(Err(Invalid("x"))))} ${Form.adder(399).to_str()}")

update : Str, Msg -> Str
update = |model, msg|
	match msg {
		SubmitDone(Err(problem)) => problem_detail(Form.submit, problem)
		_ => model
	}

problem_detail : Api.Endpoint(request, answer, [Server(Api.Failure), ..others]), [Server(Api.Failure), ..others] -> Str
problem_detail = |submitted_to, error| Api.status_of(submitted_to, error).to_str()
