Combinator(a) :: { fun : List(U8) -> a }.{
	const : a -> Combinator(a)
	const = |v| { fun: |_| v }

	keep : Combinator(a -> b), Combinator(a) -> Combinator(b)
	keep = |pf, pa| {
		fun: |input| {
			f = (pf.fun)(input)
			f((pa.fun)(input))
		},
	}

	run : Combinator(a), List(U8) -> a
	run = |p, input| (p.fun)(input)
}
