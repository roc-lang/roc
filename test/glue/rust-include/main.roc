platform ""
	requires {
		main : U64 -> U64
	}
	exposes []
	packages {}
	provides { "roc_main": main_for_host }
	targets: {}

main_for_host : U64 -> U64
main_for_host = |n| (main)(n)
