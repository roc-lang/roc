# Unannotated `is_eq` and `to_hash` methods whose bodies convert a numeric
# literal through another requirement's method (`a.n % 3`), reached as a
# component of structural equality and hashing: inside record, tuple, list,
# and tag payload literals, compared directly and through a generic function,
# and hashed by builtin `Set` code.

Mod := { n : U64 }.{
	is_eq = |a, b| a.n % 3 == b.n % 3
	to_hash = |m, hasher| hasher.write_u64(m.n % 3)
}

same = |a, b| a == b

count_distinct = |items| Set.from_list(items).len()

main! = |_args| {
	record = same({ m: Mod.{ n: 1 } }, { m: Mod.{ n: 4 } })
	tuple = same((Mod.{ n: 1 }, 2), (Mod.{ n: 7 }, 2))
	list = same([Mod.{ n: 1 }, Mod.{ n: 2 }], [Mod.{ n: 4 }, Mod.{ n: 6 }])
	tag = same(Wrapped(Mod.{ n: 2 }), Wrapped(Mod.{ n: 5 }))
	direct = { m: Mod.{ n: 1 } } == { m: Mod.{ n: 2 } }
	nested = same({ inner: (Mod.{ n: 3 }, [Mod.{ n: 1 }]) }, { inner: (Mod.{ n: 6 }, [Mod.{ n: 4 }]) })
	echo!("${Str.inspect(record)} ${Str.inspect(tuple)} ${Str.inspect(list)} ${Str.inspect(tag)} ${Str.inspect(direct)} ${Str.inspect(nested)}\n")
	distinct = count_distinct([{ m: Mod.{ n: 1 } }, { m: Mod.{ n: 4 } }, { m: Mod.{ n: 2 } }])
	distinct_tuples = count_distinct([(Mod.{ n: 1 }, 1), (Mod.{ n: 4 }, 1), (Mod.{ n: 5 }, 1)])
	echo!("${Str.inspect(distinct)} ${Str.inspect(distinct_tuples)}\n")
	Ok({})
}
