# Padded nominals with the same named fields back onto one record row, whatever
# their order or padding, and a plain record is a value of that row too. Each
# keeps its own layout, and `y` sits at a different offset in every one of them.

A := { x : U8, _ : U8, y : U64 }

B := { y : U64, _ : U16, x : U8 }

P(p) := { x : U8, _ : p, y : U64 }

# These bodies read a record by its row, so each padded nominal reaches them
# at its own layout.
row_param = |r| r.x.to_u64() + r.y

list_param = |xs| List.fold(xs, 0, |n, r| n + r.y)

box_param = |b| Box.unbox(b).y

keep = |r| {
	_ = r.y
	r
}

# A function builds a padded nominal by being annotated to return it: a record
# a function builds with no annotation never becomes one at a use.
make_a : U8, U64 -> A
make_a = |x, y| { x, y }

make_b : U8, U64 -> B
make_b = |x, y| { x, y }

make_list_a : U8, U64 -> List(A)
make_list_a = |x, y| [{ x, y }, { x, y }]

make_list_b : U8, U64 -> List(B)
make_list_b = |x, y| [{ x, y }, { x, y }]

read_y = |r| r.y

ys = |xs| List.map(xs, |e| e.y)

main! = |_args| {
	a : A
	a = { x: 1, y: 2 }
	b : B
	b = { x: 3, y: 4 }
	c : P(U64)
	c = { x: 5, y: 6 }
	d : P(Str)
	d = { x: 7, y: 8 }
	plain = { x: 9.U8, y: 10.U64 }
	echo!("${Str.inspect(row_param(a))} ${Str.inspect(row_param(b))} ${Str.inspect(row_param(c))} ${Str.inspect(row_param(d))} ${Str.inspect(row_param(plain))}\n")
	aa : List(A)
	aa = [a, a]
	bs : List(B)
	bs = [b, b]
	echo!("${Str.inspect(list_param(aa))} ${Str.inspect(list_param(bs))} ${Str.inspect(list_param([plain]))}\n")
	echo!("${Str.inspect(read_y(a))} ${Str.inspect(read_y(b))} ${Str.inspect(read_y(d))} ${Str.inspect(read_y(plain))}\n")
	echo!("${Str.inspect(box_param(Box.box(a)))} ${Str.inspect(box_param(Box.box(b)))}\n")
	made_a = make_a(1, 2)
	made_b = make_b(3, 4)
	list_a = make_list_a(5, 6)
	list_b = make_list_b(7, 8)
	echo!("${Str.inspect(made_a.y)} ${Str.inspect(made_b.y)} ${Str.inspect(ys(list_a))} ${Str.inspect(ys(list_b))}\n")
	kept_a : A
	kept_a = keep(a)
	kept_b : B
	kept_b = keep(b)
	echo!("${Str.inspect(kept_a.y)} ${Str.inspect(kept_b.y)}\n")

	Ok({})
}
