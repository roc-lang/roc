# A function-body type whose `is_eq` and `to_hash` capture a local, hashed by
# builtin `Set` code: through a user-written generic helper, and as the
# component of a generic nominal whose own `to_hash` dispatches to it.

Wrap(a) := { inner : a }.{
	is_eq = |x, y| x.inner == y.inner
	to_hash = |w, hasher| w.inner.to_hash(hasher)
}

count_distinct = |items| Set.from_list(items).len()

main! = |args| {
	modulus = List.len(args) + 3
	Mod := { n : U64 }.{
		is_eq = |a, b| a.n % modulus == b.n % modulus
		to_hash = |m, hasher| hasher.write_u64(m.n % modulus)
	}

	items = [Mod.{ n: 1 }, Mod.{ n: 4 }, Mod.{ n: 2 }]
	wrapped = [Wrap.{ inner: Mod.{ n: 1 } }, Wrap.{ inner: Mod.{ n: 4 } }, Wrap.{ inner: Mod.{ n: 2 } }, Wrap.{ inner: Mod.{ n: 5 } }]
	echo!("${Str.inspect(count_distinct(items))} ${Str.inspect(Set.from_list(items).len())} ${Str.inspect(Set.from_list(wrapped).len())} ${Str.inspect(count_distinct(wrapped))}\n")
	Ok({})
}
