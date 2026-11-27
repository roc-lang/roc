# selftest: a small program the model must accept in every mode.
z0 : {} -> U64
z0 = |_| List.len(Str.split_on("a b", " ")) - 2

pad : U64 -> Str
pad = |z| "heap-allocated-string-padding-${z.to_str()}"

expect {
	z = z0({})
	xs = [pad(z), "b"]
	alias = xs
	ys = xs.set(0, "c").ok_or([])
	rec = { a: ys, b: alias }
	zs = rec.a.append(pad(z))
	d = Dict.empty().insert("k", rec.b)
	d2 = d.insert("k", zs)
	alias == [pad(0), "b"] and ys == ["c", "b"] and zs == ["c", "b", pad(0)] and d.get("k").ok_or([]) == alias and Dict.len(d2) == 1
}
