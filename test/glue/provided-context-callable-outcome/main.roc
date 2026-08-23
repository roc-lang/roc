platform "provided-context-callable-outcome"
	requires {
		[State : state] for program : {
			make_outcome! : U64, state => Abi.SourceOutcome,
		}
	}
	exposes [Abi]
	packages {}
	provides {
		"roc_make_outcome": make_outcome_for_host!,
	}
	targets: {}

import Abi

make_outcome_for_host! : U64, State => Abi.SourceOutcome
make_outcome_for_host! = program.make_outcome!
