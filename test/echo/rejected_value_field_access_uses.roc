# Each name below is read out of a tuple, record, or tag that holds a value
# rejected after the name was checked: a loop binder over a literal whose
# default is rejected, a method call rejected at its receiver's default, or a
# string literal a later `?` rejects as a `Try`. Evaluating the tuple, record,
# or tag crashes at that value, so the name binds nothing, and its uses, which
# conflict with each other, add no report beyond the original rejection.
# Running the program crashes only once such a value is reached.
tuple_access = |_x| {
	for y in 5 {
		z = (y, 1).0
		a = Str.concat(z, "a")
		b = List.len(z)
		_ = (a, b)
	}
	{}
}

named_record_access = |_x| {
	for y in 5 {
		r = { f: y }
		z = r.f
		a = Str.concat(z, "a")
		b = List.len(z)
		_ = (a, b)
	}
	{}
}

tag_payload = |_x| {
	for y in 5 {
		z = match Wrapped(y) {
			Wrapped(v) => v
		}
		a = Str.concat(z, "a")
		b = List.len(z)
		_ = (a, b)
	}
	{}
}

rejected_dispatch_access = |_x| {
	s = 5
	z = (s.foo(), 1).0
	a = Str.concat(z, "a")
	b = List.len(z)
	(a, b)
}

literal_rejected_later = |_x| {
	y = { a: "abc" }.a
	w = y?
	a = Str.concat(y, "a")
	b = List.len(y)
	Ok((w, a, b))
}

main! = |args| {
	echo!("before")
	if List.len(args) > 100 {
		named_record_access(1)
		tag_payload(1)
		_ = rejected_dispatch_access(1)
		_ = literal_rejected_later(1)
	}
	tuple_access(1)
	echo!("after")
	Ok({})
}
