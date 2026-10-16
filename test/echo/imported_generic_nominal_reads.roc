# Values of generic nominals declared in another module, instantiated here
# with concrete types, read through every kind of access.
import ImportedGenericNominals

Local(w) := { a : { b : w, c : w } }

local : w, w -> Local(w)
local = |x, y| { a: { b: x, c: y } }

sum : ImportedGenericNominals.Pair(U64) -> U64
sum = |p| p.a.0 + p.a.1

main! = |args| {
	n = args.len()
	pair = ImportedGenericNominals.pair(n + 1, n + 2)
	echo!("field: ${pair.a.1.to_str()}\n")
	{ a: (first, _) } = pair
	echo!("destructured: ${first.to_str()}\n")
	echo!("annotated: ${sum(pair).to_str()}\n")
	concrete = ImportedGenericNominals.pair_u64(n + 3, n + 4)
	echo!("concrete export: ${concrete.a.0.to_str()}\n")
	nested = ImportedGenericNominals.nested(n + 5, n + 6)
	echo!("nested path: ${nested.a.c.to_str()}\n")
	wrapped = ImportedGenericNominals.wrapped(n + 7, n + 8)
	payload = match wrapped {
		Two((x, _)) => x
		Empty => 0
	}
	echo!("tag payload: ${payload.to_str()}\n")
	echo!("local nested path: ${local(n + 9, n + 10).a.b.to_str()}\n")
	Ok({})
}
