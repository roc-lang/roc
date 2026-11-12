import OpenRowMethods

# A method call on a record or tag union whose row ends in a row variable is
# answered by the type each call supplies (design.md "Methods On Open Rows").
# Every case is done through `==` and through a method named `foo`.

MethodsOnOpenRows :: [].{}

Tagged := { id : U64 }.{
	is_eq : Tagged, Tagged -> Bool
	is_eq = |_, _| Bool.True

	foo : Tagged, Tagged -> Bool
	foo = |_, _| Bool.True

	to_inspect : Tagged -> Str
	to_inspect = |t| Str.concat("tagged ", t.id.to_str())
}

Light := [Red, Green].{
	is_eq : Light, Light -> Bool
	is_eq = |_, _| Bool.True

	foo : Light, Light -> Bool
	foo = |_, _| Bool.True
}

one : Tagged
one = { id: 1 }

two : Tagged
two = { id: 2 }

same = |l, r| {
	_ = l.id + r.id
	l == r
}

read_foo = |l, r| {
	_ = l.id + r.id
	l.foo(r)
}

# The method call comes before the field access that makes the value a record.
same_first = |l, r| {
	result = l == r
	_ = l.id + r.id
	result
}

foo_first = |l, r| {
	result = l.foo(r)
	_ = l.id + r.id
	result
}

generic_eq = |x, y| x == y

generic_foo = |x, y| x.foo(y)

via_helper_eq = |l, r| {
	_ = l.id
	generic_eq(l, r)
}

via_helper_foo = |l, r| {
	_ = l.id
	generic_foo(l, r)
}

list_eq = |xs, ys| {
	_ = List.map(xs, |x| x.id)
	xs == ys
}

list_foo = |xs, ys| {
	_ = List.map(xs, |x| x.id)
	List.map2(xs, ys, |x, y| x.foo(y)) == List.map2(xs, ys, |_, _| Bool.True)
}

anno_eq : { id : U64, ..r }, { id : U64, ..r } -> Bool where [r.is_eq : r, r -> Bool]
anno_eq = |l, r| l == r

anno_foo : { id : U64, ..r }, { id : U64, ..r } -> Bool where [r.foo : r, r -> Bool]
anno_foo = |l, r| l.foo(r)

is_red = |c| c == Red

red_foo = |c| c.foo(Red)

show = |l| {
	_ = l.id
	Str.inspect(l)
}

# A function annotated to return a nominal type makes the record it builds
# and compares that type before the comparison is decided.
make_pair : U64, U64 -> (Bool, Tagged)
make_pair = |a, b| {
	left = { id: a }
	right = { id: b }
	(left == right, left)
}

expect same(one, two)
expect read_foo(one, two)
expect !same({ id: 1 }, { id: 2 })
expect same({ id: 1 }, { id: 1 })
expect same_first(one, two)
expect foo_first(one, two)
expect via_helper_eq(one, two)
expect via_helper_foo(one, two)
expect list_eq([one], [two])
expect list_foo([one], [two])
expect !list_eq([{ id: 1 }], [{ id: 2 }])
expect anno_eq(one, two)
expect !anno_eq({ id: 1 }, { id: 2 })
expect anno_foo(one, two)
expect !is_red(Green)
expect !is_red(Other("x"))
expect is_red(Red)
expect red_foo(Light.Green)
expect OpenRowMethods.same(one, two)
expect OpenRowMethods.read_foo(one, two)
expect !OpenRowMethods.same({ id: 1 }, { id: 2 })
expect !OpenRowMethods.is_red(Green)
expect OpenRowMethods.red_foo(Light.Green)
expect show(one) == "tagged 1"
expect make_pair(1, 2).0
