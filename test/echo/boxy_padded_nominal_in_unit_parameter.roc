# A constant holding a padded nominal is passed where a row-typed parameter
# holds an empty record, in a call that runs at runtime. The parameter's record
# has a closed row tail of its own and the nominal has a backing of its own, so
# lifting the parameter's empty record into the nominal leaves both alone.
Only := { _ : U64 }

unit_param : { k : {} }, U64 -> U64
unit_param = |_r, n| n + 1

beside : { k : Only }
beside = { k: {} }

main! = |args| {
	echo!(unit_param(beside, List.len(args)).to_str())
	Ok({})
}
