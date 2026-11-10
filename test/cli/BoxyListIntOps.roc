## `List(U32)`, `List(I16)` and `List(U64)` operations of monomorphic code,
## which `--specialize=no` lowers as calls of the pre-specialized integer list
## workers, beside the same operations at a generic item type. `roc test`
## reports exactly what `--specialize=yes` reports, including a failing
## expectation and a `dbg`.
BoxyListIntOps := [].{}

words_from : U32, U64 -> List(U32)
words_from = |start, count| fill_words([], start, count)

fill_words : List(U32), U32, U64 -> List(U32)
fill_words = |acc, next, remaining|
	if remaining == 0 {
		acc
	} else {
		fill_words(acc.append(next), next + 1, remaining - 1)
	}

shorts_from : I16, U64 -> List(I16)
shorts_from = |start, count| fill_shorts([], start, count)

fill_shorts : List(I16), I16, U64 -> List(I16)
fill_shorts = |acc, next, remaining|
	if remaining == 0 {
		acc
	} else {
		fill_shorts(acc.append(next), next + 1, remaining - 1)
	}

longs_from : U64, U64 -> List(U64)
longs_from = |start, count| fill_longs([], start, count)

fill_longs : List(U64), U64, U64 -> List(U64)
fill_longs = |acc, next, remaining|
	if remaining == 0 {
		acc
	} else {
		fill_longs(acc.append(next), next + 1, remaining - 1)
	}

second : List(a) -> Try(a, [OutOfBounds])
second = |items| items.get(1)

expect words_from(70000, 4) == [70000, 70001, 70002, 70003]
expect words_from(70000, 4).get(2) == Ok(70002)
expect words_from(70000, 4).get(9) == Err(OutOfBounds)
expect words_from(70000, 4).set(0, 99) == Ok([99, 70001, 70002, 70003])
expect words_from(70000, 4).set(4, 99) == Err(OutOfBounds)
expect words_from(70000, 4).first() == Ok(70000)
expect words_from(70000, 0).first() == Err(ListWasEmpty)
expect words_from(70000, 4).last() == Ok(70003)
expect words_from(70000, 4).sublist({ start: 1, len: 2 }) == [70001, 70002]
expect words_from(1, 2).concat(words_from(7, 1)) == [1, 2, 7]
expect words_from(70000, 4).contains(70002)
expect !(words_from(70000, 4).contains(42))
expect words_from(1, 4).map(|w| w * 100000) == [100000, 200000, 300000, 400000]
expect words_from(1, 4).fold(0, |sum, w| sum + w) == 10

expect shorts_from(-2, 4) == [-2, -1, 0, 1]
expect shorts_from(-2, 4).get(0) == Ok(-2)
expect shorts_from(-2, 4).get(4) == Err(OutOfBounds)
expect shorts_from(-2, 4).set(3, -300) == Ok([-2, -1, 0, -300])
expect shorts_from(-2, 4).first() == Ok(-2)
expect shorts_from(-2, 4).last() == Ok(1)
expect shorts_from(-2, 0).last() == Err(ListWasEmpty)
expect shorts_from(-2, 4).sublist({ start: 2, len: 5 }) == [0, 1]
expect shorts_from(-2, 2).concat(shorts_from(100, 2)) == [-2, -1, 100, 101]
expect shorts_from(-2, 4).contains(-1)
expect shorts_from(-2, 4).map(|s| s * -3) == [6, 3, 0, -3]
expect shorts_from(-2, 4).fold(0, |sum, s| sum + s) == -2

expect longs_from(5000000000, 3) == [5000000000, 5000000001, 5000000002]
expect longs_from(5000000000, 3).get(1) == Ok(5000000001)
expect longs_from(5000000000, 3).set(1, 7) == Ok([5000000000, 7, 5000000002])
expect longs_from(5000000000, 3).first() == Ok(5000000000)
expect longs_from(5000000000, 3).last() == Ok(5000000002)
expect longs_from(5000000000, 3).sublist({ start: 0, len: 1 }) == [5000000000]
expect longs_from(1, 1).concat(longs_from(9, 2)) == [1, 9, 10]
expect !(longs_from(1, 3).contains(0))
expect longs_from(1, 3).map(|l| l * 4000000000) == [4000000000, 8000000000, 12000000000]
expect longs_from(5000000000, 3).fold(0, |sum, l| sum + l) == 15000000003

expect second(words_from(5, 3)) == Ok(6)
expect second(shorts_from(-5, 3)) == Ok(-4)
expect second(longs_from(5, 3)) == Ok(6)
expect second(["a", "b"]) == Ok("b")

expect {
	shorts = shorts_from(-1, 3)
	dbg shorts
	shorts.get(1) == Ok(0)
}

expect words_from(10, 4).get(1) == Ok(99)
