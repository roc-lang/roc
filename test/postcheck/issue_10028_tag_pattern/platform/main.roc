platform ""
	requires {
		main! : () => Try({}, [Exit(I64), ..])
	}
	exposes [Draw]
	packages {}
	provides {
		"render_for_host": render_for_host!,
	}
	targets: {
		x64glibc: { inputs: [app] },
	}

import Draw

render_for_host! : () => Try({}, I64)
render_for_host! = || {
	match main!() {
		Ok({}) => Ok({})
		Err(Exit(code)) => Err(code)
		Err(_) => Err(-1)
	}
}
