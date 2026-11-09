# An erased call from a generic combinator passes a value of an open tag row
# (`Green(x)` with the rest of the row left open) to an identity closure. The
# closure's descriptor for the row's extension is read from the argument's own
# descriptor, since the calling closure sees only a type variable there.
BoxyErasedOpenRowArgument(a) :: { fun : List(U8) -> a }.{
	const : a -> BoxyErasedOpenRowArgument(a)
	const = |v| { fun: |_| v }

	keep : BoxyErasedOpenRowArgument(a -> b), BoxyErasedOpenRowArgument(a) -> BoxyErasedOpenRowArgument(b)
	keep = |pf, pa| {
		fun: |input| {
			f = (pf.fun)(input)
			f((pa.fun)(input))
		},
	}

	run : BoxyErasedOpenRowArgument(a), List(U8) -> a
	run = |p, input| (p.fun)(input)
}

num : BoxyErasedOpenRowArgument(U64)
num = BoxyErasedOpenRowArgument.const(5)

main! = |_args| Ok({})

go = |s| {
	green = BoxyErasedOpenRowArgument.const(|x| Green(x)).keep(num)
	game = BoxyErasedOpenRowArgument.const(|r| { r: r }).keep(green)
	game.run(s)
}

expect go([]) == { r: Green(5) }
