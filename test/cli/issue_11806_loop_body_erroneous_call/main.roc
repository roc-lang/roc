# Loop bodies whose final expression is an effectful call with an argument
# that does not resolve. Checking replaces each call with a runtime error, and
# `roc check` must report every unknown name instead of crashing while it
# lowers the loops.

app [main!] {}

main! = |_args| {
	for _x in [1] {
		echo!(nope)
	}
	for _x in [1] {
		echo!("a")
		echo!(nope)
	}
	while Bool.True {
		echo!(nope)
	}
	Ok({})
}
