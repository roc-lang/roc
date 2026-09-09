platform "glue-payload-free-tag-union"
	requires {
		main! : () => {}
	}
	exposes [Effects]
	packages {}
	provides {
		"roc_main": main_for_host!,
	}
	hosted {
		"roc_first_scope": Effects.first_scope!,
		"roc_second_scope": Effects.second_scope!,
	}
	targets: {}

import Effects

main_for_host! : () => {}
main_for_host! = main!
