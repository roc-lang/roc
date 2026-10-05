# A closure that calls a local function capturing locals of the enclosing
# body needs those locals too, even though it names only the function: a
# local function calling it, a closure passed to `List.map` calling it, and a
# closure nested in another local function that reaches it through a third.

main! = |args| {
	extra = List.len(args) + 1
	label = "x"
	shift = |x| x + extra
	tag = |x| "${label}${shift(x).to_str()}"
	twice = |y| shift(shift(y))
	nested = |z| {
		inner = |w| tag(w)
		inner(z)
	}
	shifted = [1, 2].map(|y| shift(y))
	echo!("${twice(1).to_str()} ${Str.inspect(shifted)} ${nested(5)}\n")
	Ok({})
}
