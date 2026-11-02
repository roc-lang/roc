# The `Try`-shaped sibling of BoxyRestoredClosureCapture.roc: the restored
# closures return `Try({ val : a }, [Fail])`, and `apply` uses `?` on each.
BoxyRestoredClosureTry(a) :: { fun : U8 -> Try({ val : a }, [Fail]) }.{
	const : a -> BoxyRestoredClosureTry(a)
	const = |v| { fun: |_| Ok({ val: v }) }

	run : BoxyRestoredClosureTry(a), U8 -> Try({ val : a }, [Fail])
	run = |{ fun }, i| fun(i)

	apply : BoxyRestoredClosureTry(a -> b), BoxyRestoredClosureTry(a) -> BoxyRestoredClosureTry(b)
	apply = |fh, vh| { fun: |i| {
		{ val: f } = fh.run(i)?
		{ val: v } = vh.run(i)?
		Ok({ val: f(v) })
	} }
}

plus : BoxyRestoredClosureTry(U64)
plus = BoxyRestoredClosureTry.const(|d| d + 1).apply(BoxyRestoredClosureTry.const(4.U64))

expect plus.run(0) == Ok({ val: 5 })
