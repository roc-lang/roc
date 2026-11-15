# Repro for https://github.com/roc-lang/roc/issues/13011: the top-level value
# `store` keeps a monomorphic theme row while generalized functions widen it
# with `preference`'s tags, so both expects run under both lowering strategies.
Core := [].{
	Cell(st, a) := { get : (st -> a), modify : (st, (a -> a) -> st) }.{
		set : Cell(st, a), a -> Write(st)
		set = |c, value| {
			modify = c.modify
			{ apply: |state| modify(state, |_| value) }
		}
	}
	Write(st) :: { apply : (st -> st) }
	Store(st, h) := { init : st, cells : h }.{
		cells : Store(st, h) -> h
		cells = |s| s.cells
		with_initial : Store(st, h), Cell(st, a), a -> Store(st, h)
		with_initial = |s, c, value| {
			modify = c.modify
			{ init: modify(s.init, |_| value), cells: s.cells }
		}
		after : Store(st, h), List(Write(st)) -> Store(st, h)
		after = |s, writes| { init: writes.fold(s.init, |state, w| (w.apply)(state)), cells: s.cells }
		read : Store(st, h), Cell(st, a) -> a
		read = |s, c| (c.get)(s.init)
	}
	simple_store : a, b -> Store((a, b), { theme : Cell((a, b), a), count : Cell((a, b), b) })
	simple_store = |theme, count| {
		init: (theme, count),
		cells: {
			theme: { get: |(a, _)| a, modify: |(a, b), f| (f(a), b) },
			count: { get: |(_, b)| b, modify: |(a, b), f| (a, f(b)) },
		},
	}

}

Issue13011StoreWrites := [App].{
	Theme : [Light, Dark]
	store = Core.simple_store(Light, 0.U64)
	cells = Issue13011StoreWrites.store.cells()
	preference : Bool -> Theme
	preference = |dark| if dark {
		Dark
	} else {
		Light
	}
	sized : U64 -> List(Core.Write(_))
	sized = |n| [Issue13011StoreWrites.cells.count.set(n)]
	media_changed : [Matches(Bool), Refused] -> List(Core.Write(_))
	media_changed = |result| match result {
		Matches(value) => [Issue13011StoreWrites.cells.theme.set(Issue13011StoreWrites.preference(value))]
		_ => []
	}
	start : Bool -> Core.Store(_, _)
	start = |dark| Issue13011StoreWrites.store.with_initial(Issue13011StoreWrites.cells.theme, Issue13011StoreWrites.preference(dark))
}

## Starting with the other member.
expect Issue13011StoreWrites.start(Bool.True).read(Issue13011StoreWrites.cells.theme) == Dark

## Reading another cell and both members after writes.
expect {
	before = Issue13011StoreWrites.store
	updated = before.after(Issue13011StoreWrites.sized(20)).after(Issue13011StoreWrites.media_changed(Matches(Bool.True)))
	actual = {
		count: updated.read(Issue13011StoreWrites.cells.count),
		theme: updated.read(Issue13011StoreWrites.cells.theme),
		light: updated.after(Issue13011StoreWrites.media_changed(Matches(Bool.False))).read(Issue13011StoreWrites.cells.theme),
	}
	actual == { count: 20, theme: Dark, light: Light }
}
