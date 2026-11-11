## Tail recursion that passes lists along and reads only their lengths, so
## each specialization is shared by every item type.
RecursionReadsOnlyListLength := [].{
	keep : List(a), U64 -> U64
	keep = |xs, n| if n == 0 { List.len(xs) } else { keep(xs, n - 1) }

	swap : List(a), List(a), U64 -> U64
	swap = |xs, ys, n| if n == 0 { List.len(xs) } else { swap(ys, xs, n - 1) }

	ignore : List(a), U64 -> U64
	ignore = |xs, n| if n == 0 { 0 } else { ignore(xs, n - 1) }
}

expect RecursionReadsOnlyListLength.keep([1.U8, 2], 100000) == 2
expect RecursionReadsOnlyListLength.keep(["a", "b", "c"], 3) == 3
expect RecursionReadsOnlyListLength.swap(["a"], ["b", "c"], 3) == 2
expect RecursionReadsOnlyListLength.swap([1.U8], [2, 3], 100000) == 1
expect RecursionReadsOnlyListLength.ignore(["a"], 100000) == 0
