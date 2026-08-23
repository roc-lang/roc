platform ""
	requires {
		[Msg : msg] for program : {
			update : List(msg) -> {},
		}
	}
	exposes [Program]
	packages {}
	provides { "update_for_host": update_for_host }
	targets: {
		inputs_dir: "targets/",
		x64musl: {
			inputs: [app],
			output: Archive,
		},
	}

import Program

StepFromHost(msg) : {
	completed : List(Program.CompletionEnvelope(msg)),
}

update_for_host : StepFromHost(Msg) -> {}
update_for_host = |{ completed }| {
	messages = List.map(completed, Program.complete)
	(program.update)(messages)
}
