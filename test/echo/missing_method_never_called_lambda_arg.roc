# A call to a method the receiver's type lacks is rejected. Its argument is a
# lambda that is never called and dispatches on a parameter no use pins;
# running the program evaluates that argument and crashes at the call, in
# every lowering mode.
main! = |_args| {
	n = 3.U64
	echo!(n.missing(|x| x.to_str()).to_str())
	Ok({})
}
