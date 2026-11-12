platform ""
	requires {
		main : Str
	}
	exposes [Wrap, Extra]
	packages {}
	provides { "roc_main": main_for_host }

import Wrap
import Extra

main_for_host : Str
main_for_host = Wrap.wrap("${main}${Extra.suffix}")
