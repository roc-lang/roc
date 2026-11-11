# Padded nominals with the same named fields back onto one record row, whatever
# their order or padding, and a plain record is a value of that row too. Each
# keeps its own layout, and `y` sits at a different offset in every one of them.
import Stored

A := { x : U8, _ : U8, y : U64 }

B := { y : U64, _ : U16, x : U8 }

P(p) := { x : U8, _ : p, y : U64 }

S := { y : U64, _ : U8, s : Str }

Only := { _ : U64 }

# These bodies read a record by its row, so each padded nominal reaches them
# at its own layout.
row_param = |r| r.x.to_u64() + r.y

list_param = |xs| List.fold(xs, 0, |n, r| n + r.y)

box_param = |b| Box.unbox(b).y

keep = |r| {
	_ = r.y
	r
}

make = |x, y| { x, y }

make_list = |x, y| [{ x, y }, { x, y }]

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
	made_a : A
	made_a = make(1, 2)
	made_b : B
	made_b = make(3, 4)
	list_a : List(A)
	list_a = make_list(5, 6)
	list_b : List(B)
	list_b = make_list(7, 8)
	echo!("${Str.inspect(made_a.y)} ${Str.inspect(made_b.y)} ${Str.inspect(ys(list_a))} ${Str.inspect(ys(list_b))}\n")
	kept_a : A
	kept_a = keep(a)
	kept_b : B
	kept_b = keep(b)
	echo!("${Str.inspect(kept_a.y)} ${Str.inspect(kept_b.y)}\n")

	stored_a : List(A)
	stored_a = Stored.rows({})
	stored_b : List(B)
	stored_b = Stored.rows({})
	stored_p : List(P(Str))
	stored_p = Stored.rows({})
	echo!("${Str.inspect(ys(stored_a))} ${Str.inspect(ys(stored_b))} ${Str.inspect(ys(stored_p))} ${Str.inspect(ys(Stored.rows({})))}\n")
	nested : List(List(B))
	nested = Stored.nested({})
	empty : List(A)
	empty = Stored.empty_rows({})
	echo!("${Str.inspect(List.map(nested, ys))} ${Str.inspect(ys(empty))}\n")
	long : List(S)
	long = Stored.long({})
	units : List(Only)
	units = Stored.units({})
	echo!("${Str.inspect(List.map(long, |r| r.s))} ${Str.inspect(ys(long))} ${Str.inspect(List.len(units))}\n")
	tagged : List([Has(A), No])
	tagged = Stored.tagged({})
	tupled : (List(B), U8)
	tupled = Stored.tupled({})
	to_rows : U8 -> List(A)
	to_rows = Stored.to_rows
	tagged_ys = List.map(tagged, |v| match v {
		Has(r) => r.y
		No => 0
	})
	echo!("${Str.inspect(tagged_ys)} ${Str.inspect(ys(tupled.0))} ${Str.inspect(tupled.1)} ${Str.inspect(ys(to_rows(1)))}\n")
	boxed : Box(A)
	boxed = Stored.boxed({})
	boxes : List(Box(A))
	boxes = Stored.boxes({})
	echo!("${Str.inspect(Box.unbox(boxed).y)} ${Str.inspect(List.map(boxes, |e| Box.unbox(e).y))}\n")
	Ok({})
}
