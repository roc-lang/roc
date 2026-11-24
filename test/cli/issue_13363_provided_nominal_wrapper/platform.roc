platform "repro"
	requires {
		unused : {} -> {}
	}
	exposes []
	packages {}
	provides { "repro_shape": shape }
	targets: {
		inputs_dir: "targets/",
		arm64mac: { inputs: [app] },
	}

import Connections

Row := { value : Connections.Handle }

shape : List(Connections.Handle) -> List(Connections.Handle)
shape = |value| value
