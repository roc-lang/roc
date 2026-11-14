platform ""
	requires {
		main : Str
	}
	exposes []
	packages {
		lib: "../lib/main.roc",
	}
	provides { "roc_main": main_for_host }

import S
import lib.Twice

main_for_host : Str
main_for_host = Twice.twice(S.wrap(main))
