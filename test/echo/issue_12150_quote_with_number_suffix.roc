# A quote literal converts to its suffix type through `from_quote`, which no
# builtin number type has, so each suffixed quote below is a type error. The
# program runs until it evaluates one of them.
port = "8080".U16

main! = |_| {
	echo!("before")
	local = "8080".I64
	echo!(local.to_str())
	echo!(port.to_str())
	Ok({})
}
