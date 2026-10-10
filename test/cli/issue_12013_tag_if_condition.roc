# A tag used as an `if` condition is a type mismatch. Checking reports it and
# retires the condition, so `roc check` and `roc` report the mismatch instead
# of crashing while lowering it, and the independent `if` before it still runs.

is_big = |n| n > 10

main! = |_| {
	if is_big(3) { echo!("big") } else { echo!("small") }
	if Verbose { echo!("details...") } else { {} }
	Ok({})
}
