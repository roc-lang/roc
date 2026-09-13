platform "glue-generic-callable-arg"
	requires {
		unused : {} -> {}
	}
	exposes [Shapes]
	packages {}
	provides { "handler": handler }
	targets: {}

import Shapes

handler : {} -> Shapes.Handler(U64)
handler = |_| H(|_| {})
