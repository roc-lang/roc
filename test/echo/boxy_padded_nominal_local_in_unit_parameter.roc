# A local whose record holds a padded nominal is passed to a function whose
# annotation takes a record holding an empty record. The call is a type
# mismatch: a record type an annotation writes without `..` is a plain record,
# and a nominal with unnamed fields cannot be passed where one is expected.
Only := { _ : U64 }

unit_param : { k : {} }, U64 -> U64
unit_param = |_r, n| n + 1

main! = |args| {
	local : { k : Only }
	local = { k: {} }
	echo!(unit_param(local, List.len(args)).to_str())
	Ok({})
}
