platform "glue-application-only-requires"
	requires {
		[State : state] for main : Program(state)
	}
	exposes [Program, Boundary]
	packages {}
	provides { "roc_echo": echo }
	hosted { "roc_send": Boundary.send! }
	targets: {}

import Program
import Boundary

echo : Boundary.Shared -> Boundary.Shared
echo = |value| value
