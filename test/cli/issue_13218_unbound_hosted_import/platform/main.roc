platform ""
	requires {
		main! : List(Str) => Try({}, [Exit(I32)])
	}
	exposes [Stdout]
	packages {}
	provides { "roc_main": main_for_host! }
	hosted { "roc_line": Host.line! }

import Host
import Stdout

main_for_host! : List(Str) => Try({}, [Exit(I32)])
main_for_host! = main!
