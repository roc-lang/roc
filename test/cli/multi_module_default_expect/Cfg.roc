# The default is a block whose inline expect always fails. Omitting `n`
# inlines this block into the consuming module's compile-time root; the
# expect failure must name THIS module and the expect's own location.
Cfg := { n : U8 ?? {
	expect 1 == 2
	5
} }
