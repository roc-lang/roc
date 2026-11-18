BoxyGenericTailLoops := {}

# Self tail calls of generic functions that change a parameter of variable
# type. Under `--specialize=no` such a call loops, the parameter carried in
# the procedure's own frame, when it passes an independent value; one that
# swaps two parameters, or passes a part of one, replaces the frame instead.

count_down = |remaining, acc, step| if remaining == 0 acc else count_down(remaining - 1, acc + step, step)

expect count_down(100000.U64, 0, 3) == 300000
expect count_down(10.I32, 1.5, 0.5) == 6.5

append_all : List(a), List(a) -> List(a)
append_all = |acc, rest| match rest {
	[] => acc
	[first, .. as others] => append_all(List.append(acc, first), others)
}

expect append_all([], ["a", "b", "c"]) == ["a", "b", "c"]
expect append_all([1.U64], [2, 3, 4, 5]) == [1, 2, 3, 4, 5]

swap_until : a, a, U64 -> (a, a)
swap_until = |x, y, n| if n == 0 (x, y) else swap_until(y, x, n - 1)

expect swap_until("left", "right", 3) == ("right", "left")
expect swap_until([1.U8], [2], 100000) == ([1], [2])

Pair(a) : { value : a, rest : List(a) }

last_of : Pair(a) -> a
last_of = |pair| match pair.rest {
	[] => pair.value
	[next, .. as more] => last_of({ value: next, rest: more })
}

expect last_of({ value: "x", rest: ["y", "z"] }) == "z"

keep_largest : a, List(a) -> a where [a.is_gt : a, a -> Bool]
keep_largest = |best, items| match items {
	[] => best
	[first, .. as rest] => keep_largest(if first > best first else best, rest)
}

expect keep_largest(0.U64, [5, 9, 2, 7]) == 9
expect keep_largest(3.I8, [-4, 12, 8]) == 12
