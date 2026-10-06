# An uninitialized `var` annotated with an undeclared type has no type for its
# binder. Checking reports the undeclared type and replaces the declaration
# with a runtime error, so `roc check` and `roc` do not crash while lowering it.

main! = |_args| {
	echo!("before")
	$count : UnknownType
	var $count
	$count = 1
	Ok({})
}
