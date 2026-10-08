# A nominal record literal that omits a defaulted field evaluates the fields
# it supplies in the order written.
Config := { zz : U64, aa : U64, mm : U64 ?? 7 }

make : U64 -> Config
make = |n| { zz: {
	dbg "zz"
	n
}, aa: {
	dbg "aa"
	n + 1
} }

main! = |args| {
	echo!("${Str.inspect(make(List.len(args)))}\n")
	Ok({})
}
