# Structural equality and hashing over values containing a function-body type
# whose `is_eq` and `to_hash` capture a local: in the declaring frame, in a
# closure there, inside generic procedures (including a nominal's derived
# method), and through builtin collections.

Wrap(a) := { inner : a }.{
	is_eq = |x, y| x.inner == y.inner
}

same = |x, y| x == y

same_rec = |x, y| { v: x } == { v: y }

wrap_eq = |x, y| Wrap.{ inner: x } == Wrap.{ inner: y }

main! = |args| {
	slack = List.len(args) + 1
	Approx := { n : U64 }.{
		is_eq = |a, b| a.n <= b.n + slack and b.n <= a.n + slack
	}

	a = Approx.{ n: 3 }
	b = Approx.{ n: 4 }
	far = Approx.{ n: 10 }
	same_record = { c: a } == { c: b }
	diff_record = { c: a } != { c: far }
	same_wrap = Wrap.{ inner: a } == Wrap.{ inner: b }
	diff_wrap = Wrap.{ inner: a } == Wrap.{ inner: far }
	same_list = [a, far] == [b, far]
	found = [far, b].contains(a)
	in_closure = (|| { c: a } == { c: b })()
	echo!("${Str.inspect(same_record)} ${Str.inspect(diff_record)} ${Str.inspect(same_wrap)} ${Str.inspect(diff_wrap)} ${Str.inspect(same_list)} ${Str.inspect(found)} ${Str.inspect(in_closure)}\n")
	echo!("${Str.inspect(same_rec(a, b))} ${Str.inspect(same_rec(a, far))} ${Str.inspect(wrap_eq(a, b))} ${Str.inspect(wrap_eq(a, far))}\n")

	modulus = List.len(args) + 3
	Mod := { n : U64 }.{
		is_eq = |x, y| x.n % modulus == y.n % modulus
		to_hash = |m, hasher| hasher.write_u64(m.n % modulus)
	}

	items : List({ m : Mod })
	items = [{ m: Mod.{ n: 1 } }, { m: Mod.{ n: 1 + modulus } }, { m: Mod.{ n: 2 } }]
	wanted : { m : Mod }
	wanted = { m: Mod.{ n: 2 + modulus } }
	other : { m : Mod }
	other = { m: Mod.{ n: 1 + modulus + modulus } }
	count = Set.from_list(items).len()
	contained = items.contains(wanted)
	echo!("${Str.inspect(count)} ${Str.inspect(contained)} ${Str.inspect(same(wanted, other))} ${Str.inspect(same(items, items))}\n")
	Ok({})
}
