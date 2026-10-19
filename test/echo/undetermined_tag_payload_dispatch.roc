# The two branches bind different names. The `T7` payload is the string
# literal, which defaults to `Str`, but the `T0` payload is a type that never
# resolves to a concrete type, and `unwrap` calls `concat` on it. The call is a
# direct call from `main!`, so that dispatch must be chosen now and cannot
# be: it is reported once, as a missing method on a type that never resolved,
# and running the program crashes at the call.
unwrap = |t| match t {
	T0(v) => v.concat("0")
	T7(v) => v.concat("7")
}

main! = |_args| {
	echo!("before")
	echo!(unwrap(T7("x")))
	echo!("after")
	Ok({})
}
