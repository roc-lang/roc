# A program with a type error still runs with `--specialize=no`, up to the
# rejected expression, and then crashes with the same message as
# `--specialize=yes`.

greet = |name| "hi ${name}"

main! = |_args| {
	echo!(greet("ok"))
	s : Str
	s = greet(42.U64)
	echo!(s)
	n = 42.U64
	echo!("n is ${n}")
	Ok({})
}
