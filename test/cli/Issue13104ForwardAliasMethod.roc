Counter(tag) : Count(tag)

Count(tag) :: { n : U64 }.{
	twice : Counter(tag) -> Counter(tag)
	twice = |c| Counter.bump(Counter.bump(c))

	bump : Counter(tag) -> Counter(tag)
	bump = |Count.(c)| Count.({ n: c.n + 1 })
}

main! = |_args| {
	c : Counter([A])
	c = Count.({ n: 0 }).twice()
	Count.(inner) = c
	echo!("${inner.n.to_str()}\n")
	Ok({})
}
