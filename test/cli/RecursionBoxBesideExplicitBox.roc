RecursionBoxBesideExplicitBox := {}

# repro for https://github.com/roc-lang/roc/issues/12927
# `T` reaches itself both through a recursion box (the `t` field) and through an
# explicit `Box(T)` (the `bt` field). Both boxes have the same runtime
# representation, so they must share one layout and one refcount helper.

T := [Nil, Cons({ t : T, bt : Box(T), s : Str })]

wrap : Str -> Box(T)
wrap = |s| Box.box(Cons({ t: Nil, bt: Box.box(Nil), s }))

depth : Box(T) -> I64
depth = |b| match Box.unbox(b) {
	Nil => 0
	Cons(r) => 1 + depth(r.bt) + depth(Box.box(r.t))
}

expect {
	one = wrap("x")
	two = Box.box(Cons({ t: Box.unbox(one), bt: one, s: "y" }))
	depth(two) == 3
}
