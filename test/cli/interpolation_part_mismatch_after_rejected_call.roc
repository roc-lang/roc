# The interpolated value comes from a call whose argument was rejected, and
# the interpolation's `Str` part demand then fails at `run`'s use.
mk = |{}| |x| x

run = |{}| {
	id = mk({})
	_d = id(1.U8)
	c = id("x")
	"${c}"
}

main! = |_args| {
	echo!(run({}))
	Ok({})
}
