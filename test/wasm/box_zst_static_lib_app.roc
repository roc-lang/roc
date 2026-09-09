app [main!] { pf: platform "./static-lib-platform/main.roc" }

unbox_unit : Box({}) -> {}
unbox_unit = |boxed| Box.unbox(boxed)

main! = |seed| {
	boxed = if seed == 0 {
		Box.box({})
	} else {
		Box.box({})
	}
	value = unbox_unit(boxed)
	if value == {} {
		"ok"
	} else {
		"bad"
	}
}
