UncommittedTailsAtDepth :: [].{}

# Each unannotated function returns a record literal whose uncommitted tail
# stays quantified, so each use can widen its own copy with an absent
# optional field, at any depth of the result.

make = |value| { a: value }
wrap = |n| [{ a: n }]
nest = |n| { outer: { a: n } }
tagged = |n| Thing({ a: n })
ok_unit = |_n| Ok({})
pair = |n| ({ a: n }, 1.U8)

expect {
	record : { a ?: U8, b ?: U8 }
	record = make(11)
	(record.?a ?? 0) + (record.?b ?? 20) == 31
}

expect {
	record : { a ?: U8, b ?: Str }
	record = make(5)
	record.?b == Err(MissingField)
}

expect {
	xs : List({ a : U8, b ?: U8 })
	xs = wrap(1)
	List.map(xs, |r| r.?b ?? 7) == [7]
}

expect {
	r : { outer : { a : U8, b ?: U8 } }
	r = nest(2)
	(r.outer.?b ?? 9) == 9
}

expect {
	t : [Thing({ a : U8, b ?: U8 })]
	t = tagged(3)
	match t {
		Thing(r) => (r.?b ?? 4) == 4
	}
}

expect {
	o : Try({ b ?: U8 }, [Nope])
	o = ok_unit(1)
	match o {
		Ok(r) => (r.?b ?? 5) == 5
		Err(_) => Bool.False
	}
}

expect {
	p : ({ a : U8, b ?: Str }, U8)
	p = pair(1)
	p.0.?b == Err(MissingField)
}

expect Str.inspect(make(1.U8)) == "{ a: 1 }"
