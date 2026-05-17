Iter(elem) :: {
	# The sequence being iterated, or e.g. a range, is captured in the next thunk.
	len_if_known : [Known(U64), Unknown],
	next : {} -> [One({ elem : elem, rest : Iter(elem) }), Skip(U64), Done],
}.{
	custom : source, [Known(U64), Unknown], (source -> Try((elem, Iter(elem)), [NoMore])) -> Iter(elem)
	custom = |source, len_if_known, next| {
		len_if_known,
		next: |{}|
			match next(source) {
				Ok((elem, iter)) => One({ elem, rest: iter })
				Err(NoMore) => Done
			},
	}

	range : I64, I64 -> Iter(I64)
	range = |start, end| {
		len_if_known: Known(I64.abs_diff(start, end)),
		next: |{}|
			if start == end {
				Done
			} else {
				step = if start < end 1.I64 else -1.I64

				One({ elem: start, rest: Iter.range(start + step, end) })
			},
	}

	map : Iter(a), (a -> b) -> Iter(b)
	map = |iter, transform|
		match iter {
			{ len_if_known, next } => {
				len_if_known,
				next: |{}|
					match next({}) {
						Done => Done
						Skip(n) => Skip(n)
						One({ elem, rest }) => One({ elem: transform(elem), rest: Iter.map(rest, transform) })
					},
			}
		}

	keep_if : Iter(a), (a -> Bool) -> Iter(a)
	keep_if = |iter, predicate|
		match iter {
			{ next, .. } => {
				len_if_known: Unknown,
				next: |{}| {
					match next({}) {
						Done => Done
						Skip(n) => Skip(n)
						One({ elem, rest }) =>
							if predicate(elem) {
								One({ elem, rest: Iter.keep_if(rest, predicate) })
							} else {
								kept_rest = Iter.keep_if(rest, predicate)
								(kept_rest.next)({})
							}
						}
				},
			}
		}

	drop_if : Iter(a), (a -> Bool) -> Iter(a)
	drop_if = |iter, predicate|
		match iter {
			{ next, .. } => {
				len_if_known: Unknown,
				next: |{}| {
					match next({}) {
						Done => Done
						Skip(n) => Skip(n)
						One({ elem, rest }) =>
							if predicate(elem) {
								dropped_rest = Iter.drop_if(rest, predicate)
								(dropped_rest.next)({})
							} else {
								One({ elem, rest: Iter.drop_if(rest, predicate) })
							}
						}
				},
			}
		}

	fold : Iter(a), acc, (acc, a -> acc) -> acc
	fold = |iter, initial, step| {
		var current = iter
		var acc = initial
		var done = Bool.False

		while !done {
			match (current.next)({}) {
				Done => {
					done = Bool.True
				}
				Skip(_) => {
					done = Bool.True
				}
				One({ elem, rest }) => {
					acc = step(acc, elem)
					current = rest
				}
			}
		}

		acc
	}
}

expect {
	iter = Iter.range(1.I64, 4.I64)

	match (iter.next)({}) {
		One({ elem: first, rest: rest1 }) =>
			match (rest1.next)({}) {
				One({ elem: second, rest: rest2 }) =>
					match (rest2.next)({}) {
						One({ elem: third }) => first == 1.I64 and second == 2.I64 and third == 3.I64
						_ => Bool.False
					}
				_ => Bool.False
			}
		_ => Bool.False
	}
}

expect {
	iter = Iter.range(3.I64, 0.I64)

	match (iter.next)({}) {
		One({ elem: first, rest: rest1 }) =>
			match (rest1.next)({}) {
				One({ elem: second }) => first == 3.I64 and second == 2.I64
				_ => Bool.False
			}
		_ => Bool.False
	}
}

expect {
	iter = Iter.range(2.I64, 2.I64)

	match (iter.next)({}) {
		Done => Bool.True
		_ => Bool.False
	}
}

expect {
	iter = Iter.map(Iter.range(1.I64, 4.I64), |n| n * 2.I64)

	match (iter.next)({}) {
		One({ elem: first, rest: rest1 }) =>
			match (rest1.next)({}) {
				One({ elem: second }) => first == 2.I64 and second == 4.I64
				_ => Bool.False
			}
		_ => Bool.False
	}
}

expect {
	iter = Iter.keep_if(Iter.range(1.I64, 6.I64), |n| I64.rem_by(n, 2.I64) == 0.I64)

	match (iter.next)({}) {
		One({ elem: first, rest: rest1 }) =>
			match (rest1.next)({}) {
				One({ elem: second }) => first == 2.I64 and second == 4.I64
				_ => Bool.False
			}
		_ => Bool.False
	}
}

expect {
	iter = Iter.drop_if(Iter.range(1.I64, 6.I64), |n| I64.rem_by(n, 2.I64) == 0.I64)

	match (iter.next)({}) {
		One({ elem: first, rest: rest1 }) =>
			match (rest1.next)({}) {
				One({ elem: second }) => first == 1.I64 and second == 3.I64
				_ => Bool.False
			}
		_ => Bool.False
	}
}

expect {
	_fold_i64 : Iter(I64), I64, (I64, I64 -> I64) -> I64
	_fold_i64 = Iter.fold

	Bool.True
}
