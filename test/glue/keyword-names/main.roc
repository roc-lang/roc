platform ""
	requires {
		main! : () => {}
	}
	exposes [Keywords]
	packages {}
	provides { "roc_main": main_for_host! }
	hosted {
		"roc_keywords_fields": Keywords.fields!,
		"roc_keywords_tags": Keywords.tags!,
	}
	targets: {}

import Keywords

main_for_host! : () => {}
main_for_host! = main!
