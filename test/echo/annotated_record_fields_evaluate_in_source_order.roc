# A record literal returned at an annotated record type evaluates its fields
# in the order written.
make : U64 -> { zz : U64, aa : U64 }
make = |n| { zz: {
	dbg "zz"
	n
}, aa: {
	dbg "aa"
	n + 1
} }

main! = |args| {
	r = make(List.len(args))
	echo!("${Str.inspect(r)}\n")
	Ok({})
}
