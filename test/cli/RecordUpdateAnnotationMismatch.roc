# Regression test: a record update `{ ..ruth, years: 30 }` that mismatches its
# annotation must report the type mismatch. It must not poison the type of
# `ruth`, which the earlier constant `ruth_label` already used, so post-check
# lowering never meets an erroneous type in `ruth_label` or in the base read.

Vet : { years : U8 }

label : Vet -> Str
label = |_vet| "vet"

ruth : Vet
ruth = { years: 12 }

ruth_label = label(ruth)

old_vet : { years : I64 }
old_vet = { ..ruth, years: 30 }

main! = |_args| {
	echo!(ruth_label)
	Ok({})
}
