# A type error inside an argument to `Str.inspect`: `List.join` takes one
# argument. The call that depends on the rejected one is not evaluated at
# compile time, so checking reports the error and running crashes where it is.
main! = |_args| {
	names = ["Ada", "Grace", "Alan"]
	echo!(Str.inspect(names.join(", ")))
	Ok({})
}
