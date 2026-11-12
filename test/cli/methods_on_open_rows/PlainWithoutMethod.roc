# A plain record or tag union has `is_eq` and `to_hash` when its fields or
# payloads do, and no other method: a function field, a function payload,
# and a call of `foo` are each rejected at the call.

PlainWithoutMethod :: [].{}

same = |l, r| {
	_ = l.id + r.id
	l == r
}

read_foo = |l, r| {
	_ = l.id + r.id
	l.foo(r)
}

is_red = |c| c == Red

red_foo = |c| c.foo(Red)

check = |_| is_red(Fn(|x| x))

expect same({ id: 1, f: |x| x }, { id: 1, f: |x| x })
expect read_foo({ id: 1 }, { id: 2 })
expect is_red(Fn(|x| x))
expect red_foo(Green)
expect check({})
