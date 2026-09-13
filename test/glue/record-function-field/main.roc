platform "glue-record-function-field"
	requires {
		unused : {} -> {}
	}
	exposes []
	packages {}
	provides { "shape": shape }
	targets: {}

import Value

shape = |_| Value.value
