Iter(item) :: {
	# The sequence being iterated, or e.g. a range, is captured in the next thunk.
	len_if_known : [Known(U64), Unknown],
	next : () -> [One({ item : item, rest : Iter(item) }), Skip(U64), Done],
}.{
	custom : source, [Known(U64), Unknown], (source -> Try((item, Iter(item)), [NoMore])) -> Iter(item)
	custom = |source, len_if_known, next| {
		len_if_known,
		next: ||
			match next(source) {
				Ok((item, rest_iter)) => One({ item, rest: rest_iter })
				Err(NoMore) => Done
			},
	}

	iter : Iter(item) -> Iter(item)
	iter = |self| self

	map : Iter(a), (a -> b) -> Iter(b)
	map = |iterator, transform|
		match iterator {
			{ len_if_known, next } => {
				len_if_known,
				next: ||
					match next() {
						Done => Done
						Skip(n) => Skip(n)
						One({ item, rest }) => One({ item: transform(item), rest: Iter.map(rest, transform) })
					},
			}
		}

	keep_if : Iter(a), (a -> Bool) -> Iter(a)
	keep_if = |iterator, predicate|
		match iterator {
			{ next, .. } => {
				len_if_known: Unknown,
				next: || {
					match next() {
						Done => Done
						Skip(n) => Skip(n)
						One({ item, rest }) =>
							if predicate(item) {
								One({ item, rest: Iter.keep_if(rest, predicate) })
							} else {
								kept_rest = Iter.keep_if(rest, predicate)
								(kept_rest.next)()
							}
						}
				},
			}
		}

	drop_if : Iter(a), (a -> Bool) -> Iter(a)
	drop_if = |iterator, predicate|
		match iterator {
			{ next, .. } => {
				len_if_known: Unknown,
				next: || {
					match next() {
						Done => Done
						Skip(n) => Skip(n)
						One({ item, rest }) =>
							if predicate(item) {
								dropped_rest = Iter.drop_if(rest, predicate)
								(dropped_rest.next)()
							} else {
								One({ item, rest: Iter.drop_if(rest, predicate) })
							}
						}
				},
			}
		}

	fold : Iter(a), acc, (acc, a -> acc) -> acc
	fold = |iterator, acc, step|
		match (iterator.next)() {
			Done => acc
			Skip(_) => acc
			One({ item, rest }) => Iter.fold(rest, step(acc, item), step)
		}
}

range_to_i64 : I64, I64 -> Iter(I64)
range_to_i64 = |current, end| {
	len_if_known: Unknown,
	next: ||
		if current <= end {
			One({ item: current, rest: range_to_i64(current + 1.I64, end) })
		} else {
			Done
		},
}

expect {
	iterator = range_to_i64(1.I64, 4.I64)

	match (iterator.next)() {
		One({ item: first, rest: rest1 }) =>
			match (rest1.next)() {
				One({ item: second, rest: rest2 }) =>
					match (rest2.next)() {
						One({ item: third }) => first == 1.I64 and second == 2.I64 and third == 3.I64
						_ => Bool.False
					}
				_ => Bool.False
			}
		_ => Bool.False
	}
}

expect {
	iterator = range_to_i64(-3.I64, 0.I64)

	match (iterator.next)() {
		One({ item: first, rest: rest1 }) =>
			match (rest1.next)() {
				One({ item: second }) => first == -3.I64 and second == -2.I64
				_ => Bool.False
			}
		_ => Bool.False
	}
}

expect {
	iterator = range_to_i64(3.I64, 2.I64)

	match (iterator.next)() {
		Done => Bool.True
		_ => Bool.False
	}
}

expect {
	iterator = Iter.map(range_to_i64(1.I64, 4.I64), |n| n * 2.I64)

	match (iterator.next)() {
		One({ item: first, rest: rest1 }) =>
			match (rest1.next)() {
				One({ item: second }) => first == 2.I64 and second == 4.I64
				_ => Bool.False
			}
		_ => Bool.False
	}
}

expect {
	iterator = Iter.keep_if(range_to_i64(1.I64, 5.I64), |n| I64.rem_by(n, 2.I64) == 0.I64)

	match (iterator.next)() {
		One({ item: first, rest: rest1 }) =>
			match (rest1.next)() {
				One({ item: second }) => first == 2.I64 and second == 4.I64
				_ => Bool.False
			}
		_ => Bool.False
	}
}

expect {
	iterator = Iter.drop_if(range_to_i64(1.I64, 5.I64), |n| I64.rem_by(n, 2.I64) == 0.I64)

	match (iterator.next)() {
		One({ item: first, rest: rest1 }) =>
			match (rest1.next)() {
				One({ item: second }) => first == 1.I64 and second == 3.I64
				_ => Bool.False
			}
		_ => Bool.False
	}
}

expect {
	iterator = range_to_i64(1.I64, 2.I64)

	match (iterator.iter().next)() {
		One({ item }) => item == 1.I64
		_ => Bool.False
	}
}

expect {
	_fold_i64 : Iter(I64), I64, (I64, I64 -> I64) -> I64
	_fold_i64 = Iter.fold

	Bool.True
}
