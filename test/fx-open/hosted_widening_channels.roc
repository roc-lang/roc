app [main!] { pf: platform "./platform/fallible_widen_main.roc" }

# Rejected side of the non-`?` widening channels (platform/FallibleWiden.roc):
# an annotated binding, an argument position, and a record field, each asking a
# hosted function's result for an error row wider than the row its host ABI
# declares. Checking this app must report a type mismatch for every channel —
# the alternative, an extern emitted at the wider row, would read the host's
# Ok bytes as Err with no diagnostic at all (design.md "Host Symbol ABI").
# hosted_channels_declared.roc is the accepted counterpart.

import pf.FallibleWiden

main! : List(Str) => Try({}, [Exit(I32), ..])
main! = |_args| {
	_ = FallibleWiden.via_annotation!({})
	_ = FallibleWiden.via_argument!({})
	_ = FallibleWiden.via_record_field!({})

	Ok({})
}
