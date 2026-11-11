# A nominal record with an unnamed field keeps its declared order in memory, so
# under `--specialize=no` its backing is a record type of its own. Whatever
# builds such a record, and whatever reads it, has to agree on that order, and a
# generic body, which reads a record by its row, gets a copy in the row's order.
import Lifted

Padded := { z : U32, _ : U32, a : Str, b : U8 }.{
	new : U32, Str -> Padded
	new = |z, a| { z, a, b: 7 }

	bump : Padded -> Padded
	bump = |p| { ..p, z: p.z + 1 }

	sum : Padded -> U64
	sum = |{ z, b, .. }| z.to_u64() + b.to_u64()
}

Plain := { z : U32, _ : U32, a : U64 }.{
	both = |p, r| p.z.to_u64() + r.a
}

Holder := [Holds(Padded), Empty]

Inner := { b : U8, _ : U8, a : Str }

Outer := { i : Inner, _ : U16, n : U64 }

Hidden :: { z : U8, _ : U32, a : U64 }.{
	new : U8, U64 -> Hidden
	new = |z, a| { z, a }

	total : Hidden -> U64
	total = |o| o.z.to_u64() + o.a
}

Only := { _ : U64 }.{
	new : {} -> Only
	new = |_| {}
}

Defaulted := { a : U8 ?? 4, _ : U8, b : U64 }

Optional := { c : U64, _ : U8, d? : U8 }

# Recursion through a list and through a box, and through an unnamed field.
Tree(a) := { v : a, _ : U8, kids : List(Tree(a)) }

Chain := { x : U8, _ : U64, next : [End, Next(Box(Chain))] }

Spacer := { x : U8, _ : [Nil, Cons(Spacer)], y : U8 }

# Generic bodies read these descriptors from their type tables.
Slot(a) := { z : U32, _ : U32, a : a, y : U8 }.{
	new : U32, a -> Slot(a)
	new = |z, a| { z, a, y: 3 }

	set : Slot(a), b -> Slot(b)
	set = |s, b| { z: s.z, a: b, y: s.y }

	value : Slot(a) -> a
	value = |s| s.a
}

Pair(a, b) := { r : b, _ : a, l : a, _ : b }.{
	new : a, b -> Pair(a, b)
	new = |l, r| { l, r }

	swap : Pair(a, b) -> Pair(b, a)
	swap = |p| { l: p.r, r: p.l }

	left : Pair(a, b) -> a
	left = |p| p.l
}

Op(a) := { f : a -> Str, _ : U8, v : a }.{
	run : Op(a) -> Str
	run = |o| (o.f)(o.v)
}

Table := { items : List(Plain), _ : U8, name : Str }

# Generated codecs walk the record row, and read and build the padded record.
Encoded := { z : U32, _ : U32, a : Str ?? "fallback" }.{
	encoder_for : _
	parser_for : _
}

EncodedSlot(a) := { z : U32, _ : U8, a : a }.{
	encoder_for : _
	parser_for : _
}

EncodedOuter := { p : Encoded, _ : U16, n : U64 }.{
	encoder_for : _
	parser_for : _
}

size : Tree(a) -> U64
size = |t| List.fold(t.kids, 1, |n, k| n + size(k))

depth : Chain -> U64
depth = |c| match c.next {
	End => 1
	Next(b) => 1 + depth(Box.unbox(b))
}

keep : Slot(a) -> Slot(a)
keep = |s| s

slots : a, b -> (Slot(a), Slot(b))
slots = |a, b| (Slot.new(1, a), Slot.new(2, b))

# These bodies know the row, and the nominal only at their callers.
make = |z, a| { z, a }

make_list = |z, a| [{ z, a }]

make_nothing = |_| {}

read_z = |r| r.z

poke = |r| { ..r, z: 5 }

read_each = |xs| List.map(xs, |r| r.z)

# These bodies read a record by its row, at every position a padded nominal
# can reach them.
closed_param = |r| r.z

list_param = |xs| List.fold(xs, 0, |n, r| n + r.a)

tag_param = |t| match t {
	Has(r) => r.z
	No => 0
}

tuple_param = |(r, b)| r.z + b.to_u32()

nested_param = |r| r.p.a + r.n.to_u64()

# An empty record takes no space where the padded record it stands for does, so
# the field beside it moves.
unit_param = |r| r.n

apply : (a -> b), a -> b
apply = |f, a| f(a)

stored : Plain
stored = { z: 11, a: 12 }

stored_list : List(Plain)
stored_list = [stored, { z: 13, a: 14 }]

stored_fn : U32 -> Plain
stored_fn = |z| { z, a: 15 }

stored_table : Table
stored_table = { items: stored_list, name: "a stored table with a name too long to be inline" }

stored_generic : Pair(U8, Str)
stored_generic = Pair.new(3, "a stored pair with a string too long to be inline")

evaluated : U64
evaluated = Pair.left(Pair.new(16.U64, "s"))

adder : Plain -> (U64 -> U64)
adder = |p| |x| x + p.a

stored_adder : U64 -> U64
stored_adder = adder({ z: 1, a: 20 })

main! = |args| {
	suffix = Str.join_with(args.drop_first(1), "")
	long = "a string that is too long to be stored inline"
	p = Padded.new(1, long)
	q = Padded.bump(p)
	direct : Padded
	direct = { z: 5, a: "d", b: 6 }
	ctor = Padded.{ z: 8, a: "c", b: 9 }
	echo!("${Str.inspect(q.z)} ${q.a} ${Str.inspect(Padded.sum(q))} ${Str.inspect(direct.z + ctor.z)} ${ctor.a}\n")
	boxed = Box.box(q)
	held : Holder
	held = Holds(q)
	inner = match held {
		Holds(value) => value.a
		Empty => "empty"
	}
	echo!("${Str.inspect(List.map([p, q, direct, ctor], |e| e.z))} ${Str.inspect(Box.unbox(boxed).z)} ${inner}\n")

	outer : Outer
	outer = { i: { b: 1, a: long }, n: 9 }
	add = |x| x + outer.n
	echo!("${outer.i.a} ${Str.inspect(outer.n)} ${Str.inspect(outer.i.b)} ${Str.inspect(add(1))} ${Str.inspect(outer)}\n")
	constructed = Outer.{ i: { b: 2, a: "constructed" }, n: 10 }
	echo!("${constructed.i.a} ${Str.inspect(constructed.i.b + 1)}\n")
	hidden = Hidden.new(1, 2)
	echo!("${Str.inspect(Hidden.total(hidden))} ${Str.inspect(hidden)}\n")

	defaulted : Defaulted
	defaulted = { b: 1 }
	optional : Optional
	optional = { c: 1 }
	unit : Only
	unit = make_nothing(0)
	echo!("${Str.inspect(defaulted.a)} ${Str.inspect(optional.c)} ${Str.inspect(Only.new({}))} ${Str.inspect(unit)}\n")

	tree : Tree(Str)
	tree = { v: "root", kids: [{ v: "leaf", kids: [] }] }
	chain : Chain
	chain = { x: 1, next: Next(Box.box({ x: 2, next: End })) }
	spacer : Spacer
	spacer = { x: 1, y: 2 }
	echo!("${Str.inspect(size(tree))} ${Str.inspect(depth(chain))} ${Str.inspect(spacer.x + spacer.y)}\n")

	text = Slot.new(1, long)
	number = Slot.new(9, 42.U64)
	changed = Slot.set(number, "changed")
	(left, right) = slots("x", 2.U8)
	echo!("${Slot.value(text)} ${Str.inspect(number.a)} ${Str.inspect(number.z)} ${changed.a} ${Str.inspect(keep(changed).z)} ${left.a} ${Str.inspect(right.a)}\n")
	pair = Pair.new(long, 3.U8)
	swapped = Pair.swap(pair)
	echo!("${Str.inspect(Pair.left(swapped))} ${swapped.r} ${Str.inspect(pair)}\n")
	op : Op(U64)
	op = { f: |n| Str.inspect(n), v: 5 }
	echo!("${Op.run(op)}\n")

	plain : Plain
	plain = make(1, 2)
	poked : Plain
	poked = poke(plain)
	made : List(Plain)
	made = make_list(3, 4)
	echo!("${Str.inspect(plain.z)} ${Str.inspect(read_z(plain))} ${Str.inspect(poked.z)} ${Str.inspect(poked.a)} ${Str.inspect(List.map(made, |e| e.a))}\n")
	tagged : [Has(Plain), No]
	tagged = Has(plain)
	plains : List(Plain)
	plains = [plain, plain, plain]
	echo!("${Str.inspect(closed_param(plain))} ${Str.inspect(read_each([plain, plain]))} ${Str.inspect(list_param(plains))} ${Str.inspect(tag_param(tagged))} ${Str.inspect(tuple_param((plain, 7)))} ${Str.inspect(plain.both(plain))}\n")

	read_value : Plain -> U32
	read_value = read_z
	echo!("${Str.inspect(List.map(plains, read_z))} ${Str.inspect(apply(read_z, plain))} ${Str.inspect(read_value(plain))}\n")

	lifted : Plain
	lifted = Lifted.plain({})
	lifted_list : List(Plain)
	lifted_list = Lifted.plains({})
	lifted_fn : U32 -> Plain
	lifted_fn = Lifted.to_plain
	lifted_unit : Only
	lifted_unit = Lifted.nothing({})
	beside : { k : Only, n : U64 }
	beside = Lifted.beside({})
	nested : { p : Plain, n : U8 }
	nested = { p: plain, n: 3 }
	echo!("${Str.inspect(beside.n)} ${Str.inspect(unit_param(beside))} ${Str.inspect(nested_param(nested))} ${Str.inspect(stored_adder(1))}\n")
	echo!("${Str.inspect(lifted.a)} ${Str.inspect(List.map(lifted_list, |e| e.a))} ${Str.inspect(lifted_fn(1).a)} ${Str.inspect(apply(Lifted.to_plain, 2.U32).a)} ${Str.inspect(lifted_unit)}\n")

	echo!("${Str.inspect(stored.a)} ${Str.inspect(List.map(stored_list, |e| e.a))} ${Str.inspect(stored_fn(1).a)} ${Str.inspect(List.len(stored_table.items))} ${stored_table.name} ${stored_generic.r} ${Str.inspect(evaluated)}\n")

	encoded : Encoded
	encoded = { z: 4, a: "text" }
	encoded_slot : EncodedSlot(Str)
	encoded_slot = { z: 5, a: "slot" }
	encoded_outer : EncodedOuter
	encoded_outer = { p: encoded, n: 11 }
	echo!("${Json.to_str(encoded)} ${Json.to_str(encoded_slot)} ${Json.to_str(encoded_outer)}\n")
	parsed : Try(Encoded, _)
	parsed = Json.parse(Str.concat("{\"a\": \"parsed\", \"z\": 6}", suffix))
	defaulted_parse : Try(Encoded, _)
	defaulted_parse = Json.parse(Str.concat("{\"z\": 7}", suffix))
	parsed_slot : Try(EncodedSlot(U64), _)
	parsed_slot = Json.parse(Str.concat("{\"a\": 7, \"z\": 8}", suffix))
	parsed_outer : Try(EncodedOuter, _)
	parsed_outer = Json.parse(Str.concat("{\"p\": {\"z\": 9}, \"n\": 10}", suffix))
	echo!("${Str.inspect(parsed.map_ok(|v| v.a))} ${Str.inspect(parsed.map_ok(|v| v.z))} ${Str.inspect(defaulted_parse.map_ok(|v| v.a))} ${Str.inspect(parsed_slot.map_ok(|v| v.a))} ${Str.inspect(parsed_outer.map_ok(|v| v.p.a))} ${Str.inspect(parsed_outer.map_ok(|v| v.n))}\n")
	Ok({})
}
