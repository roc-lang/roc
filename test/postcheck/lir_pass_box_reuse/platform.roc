platform ""
	requires {} {
		first! : Box(U64) => Box(U64),
		second! : Box(U64) => Box(U64),
	}
	exposes []
	packages {}
	provides {
		"first": first_for_host!,
		"second": second_for_host!,
	}
	targets: {}

first_for_host! : Box(U64) => Box(U64)
first_for_host! = |boxed| first!(boxed)

second_for_host! : Box(U64) => Box(U64)
second_for_host! = |boxed| second!(boxed)
