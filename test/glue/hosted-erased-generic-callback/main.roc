platform "glue-hosted-erased-generic-callback"
	requires {
		main! : () => {}
	}
	exposes [Callbacks]
	packages {}
	provides { "roc_main": main_for_host! }
	hosted {
		"roc_install": Callbacks.install!,
		"roc_notify": Callbacks.notify!,
	}
	targets: {}

import Callbacks

main_for_host! : () => {}
main_for_host! = main!
