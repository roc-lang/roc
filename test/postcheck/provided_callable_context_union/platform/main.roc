platform "provided-callable-context-union"
	requires {
		[State : state] for program : {
			make_outcome : state -> [Response(U16), Stream({ sequence : U64, step : Box(U64 -> U64) })],
		}
	}
	exposes []
	packages {}
	provides {
		"roc_context_outcome": context_outcome_for_host,
	}
	targets: {}

context_outcome_for_host : State -> [Response(U16), Stream({ sequence : U64, step : Box(U64 -> U64) })]
context_outcome_for_host = program.make_outcome
