platform "glue-hosted-generic-callable"
	requires { unused : {} -> {} }
	exposes [Callbacks]
	packages {}
	provides {}
	hosted {
		"roc_install": Callbacks.install!,
		"roc_callback": Callbacks.callback!,
		"roc_install_record": Callbacks.install_record!,
	}
	targets: {}

import Callbacks
