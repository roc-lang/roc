# A top-level constant built from generic closures that capture closures: `plus`
# holds `apply`'s closure, which captures two `const` closures, one of them
# holding a function. Compile-time evaluation stores `plus`, and Boxy restores
# it for the runtime. Each restored closure's hidden descriptors come from its
# stored captures and signature, including a field the worker represents as a
# presence slot, and a registered callee that defers its own call leaves the
# conversion to its caller's result layout for whoever makes the pending call.
BoxyRestoredClosureCapture(a) :: { fun : U8 -> { val : a } }.{
	const : a -> BoxyRestoredClosureCapture(a)
	const = |v| { fun: |_| { val: v } }

	run : BoxyRestoredClosureCapture(a), U8 -> { val : a }
	run = |{ fun }, i| fun(i)

	apply : BoxyRestoredClosureCapture(a -> b), BoxyRestoredClosureCapture(a) -> BoxyRestoredClosureCapture(b)
	apply = |fh, vh| { fun: |i| {
		{ val: f } = fh.run(i)
		{ val: v } = vh.run(i)
		{ val: f(v) }
	} }
}

plus : BoxyRestoredClosureCapture(U64)
plus = BoxyRestoredClosureCapture.const(|d| d + 1).apply(BoxyRestoredClosureCapture.const(4.U64))

expect plus.run(0).val == 5
