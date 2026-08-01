platform ""
	requires {
		main! : () => {}
	}
	exposes [Host]
	packages {}
	provides {
		"roc_main": main_for_host!,
		"roc_test_make_boxed_callable": test_make_boxed_callable_for_host,
		"roc_test_drop_boxed_callable": test_drop_boxed_callable_for_host,
		"roc_test_drop_plain_box": test_drop_plain_box_for_host,
		"roc_test_drop_nested_boxed_callable": test_drop_nested_boxed_callable_for_host,
	}
	hosted {
		"roc_test_hosted_make_boxed_callable": Host.make!,
		"roc_test_hosted_drop_boxed_callable": Host.drop!,
	}
	targets: {}

import Host

main_for_host! : () => {}
main_for_host! = main!

test_make_boxed_callable_for_host : U64 -> Box(U64 -> U64)
test_make_boxed_callable_for_host = |offset| Box.box(|value| value + offset)

test_drop_boxed_callable_for_host : Box(U64 -> U64) -> {}
test_drop_boxed_callable_for_host = |_callable| {}

test_drop_plain_box_for_host : Box(U64) -> {}
test_drop_plain_box_for_host = |_boxed| {}

test_drop_nested_boxed_callable_for_host : Box(Box(U64 -> U64)) -> {}
test_drop_nested_boxed_callable_for_host = |_boxed| {}
