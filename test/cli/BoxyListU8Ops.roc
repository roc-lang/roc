## `List(U8)` operations of monomorphic code, which `--specialize=no` lowers
## as calls of the pre-specialized `List(U8)` workers, beside the same
## operations at a generic item type. `roc test` reports exactly what
## `--specialize=yes` reports, including a failing expectation and a `dbg`.
BoxyListU8Ops := [].{}

bytes_from : U8, U64 -> List(U8)
bytes_from = |start, count| fill([], start, count)

fill : List(U8), U8, U64 -> List(U8)
fill = |acc, next, remaining|
	if remaining == 0 {
		acc
	} else {
		fill(acc.append(next), next + 1, remaining - 1)
	}

second : List(a) -> Try(a, [OutOfBounds])
second = |items| items.get(1)

expect bytes_from(10, 4) == [10, 11, 12, 13]
expect bytes_from(10, 4).get(2) == Ok(12)
expect bytes_from(10, 4).get(9) == Err(OutOfBounds)
expect bytes_from(10, 4).set(0, 99) == Ok([99, 11, 12, 13])
expect bytes_from(10, 4).set(4, 99) == Err(OutOfBounds)
expect bytes_from(10, 4).len() == 4
expect bytes_from(10, 4).first() == Ok(10)
expect bytes_from(10, 0).first() == Err(ListWasEmpty)
expect bytes_from(10, 4).last() == Ok(13)
expect bytes_from(10, 4).sublist({ start: 1, len: 2 }) == [11, 12]
expect bytes_from(1, 2).concat(bytes_from(7, 1)) == [1, 2, 7]
expect bytes_from(10, 4).contains(12)
expect !(bytes_from(10, 4).contains(42))
expect bytes_from(1, 4).map(|b| b * 2) == [2, 4, 6, 8]
expect bytes_from(1, 5).keep_if(|b| b > 2) == [3, 4, 5]
expect bytes_from(1, 4).fold(0.U8, |sum, b| sum + b) == 10
expect bytes_from(1, 5).split_on(3) == [[1, 2], [4, 5]]
expect second(bytes_from(5, 3)) == Ok(6)
expect second(["a", "b"]) == Ok("b")

expect {
	bytes = bytes_from(200, 3)
	dbg bytes
	bytes.get(1) == Ok(201)
}

expect bytes_from(10, 4).get(1) == Ok(99)
