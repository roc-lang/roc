platform ""
	requires {
		main! : List(Str) => Try({}, [Exit(I32)])
	}
	exposes [Host, Output]
	packages {}
	provides { "roc_main": main_for_host! }
	hosted { "host_output": Host.output! }

import Host
import Output

main_for_host! = |args| main!(args)
