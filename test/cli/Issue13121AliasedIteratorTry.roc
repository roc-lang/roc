make : {} -> Try(Iter(I64), [Null])
make = |_| Ok([1.I64].iter())

count = |x| match x {
	Ok(_) => 1
	Err(Null) => 0
}

run = |_| count(make({}))

f = run

main! = |_args| Ok(echo!(f(3).to_str()))
