platform ""
	requires {
		main! : List(Arg) => Try({}, [Exit(I32)])
	}
	exposes [Arg]
	packages {}
	provides { "roc_main": main_for_host! }

import Arg

main_for_host! : List(Str) => I32
main_for_host! = |args|
	match main!(args.map(Arg.from_str)) {
		Ok({}) => 0
		Err(Exit(code)) => code
	}
