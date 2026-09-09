RecordUpdateBinop :: [].{}

trigger = |state| {
	val = state.num * 10 + 5.U32
	{ ..state, num: val }
}

trigger_pinned = |state| {
	val = state.num * 10.U32 + 5.U32
	{ ..state, num: val }
}

Counter := { count : U32 }.{
	times : Counter, U32 -> Counter
	times = |c, n| { count: c.count * n }
	plus : Counter, U32 -> Counter
	plus = |c, n| { count: c.count + n }
}

expect trigger({ num: 1.U32 }).num == 15.U32
expect trigger_pinned({ num: 1.U32 }).num == 15.U32
expect trigger_pinned({ num: Counter.{ count: 1 } }).num.count == 15.U32
