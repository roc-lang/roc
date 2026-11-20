A := Missing.{
	foo = || 1.U8
}

main! = |_| {
	echo!(A.foo().to_str())
	Ok({})
}
