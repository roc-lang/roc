# A record update whose observable updated values are written out of layout
# order evaluates its base, then those values in the order written, while still
# unsetting the optional field it unsets and copying the field it leaves alone,
# whether the record it updates is closed or open.
Rec : { aa : U64, label ?: Str, mm : U64, zz : U64 }

bump : Str, U64 -> U64
bump = |name, n| {
	dbg name
	n + 1
}

make : U64 -> Rec
make = |n| {
	dbg "base"
	{ aa: n, label: "hi", mm: n + 7, zz: n }
}

# The same ordering through a record update of an open record.
reorder = |r| { ..r, zz: bump("zz2", r.zz), aa: bump("aa2", r.aa) }

main! = |args| {
	n = List.len(args)
	r = { ..make(n), zz: bump("zz", n), aa: bump("aa", n + 1), label: _ }
	echo!("${Str.inspect(r)}\n")
	echo!("${Str.inspect(reorder({ aa: n, mm: n + 2, zz: n + 3 }))}\n")
	Ok({})
}
