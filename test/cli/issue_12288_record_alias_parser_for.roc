# repro for https://github.com/roc-lang/roc/issues/12288
# Deriving parser_for for an empty-record alias must compile and the expect
# must pass, rather than crashing the compiler in post-check monotype.

F := [F].{
	parse_record_start = |_, _| crash ""
	skip_record_field = |_, _| crash ""
	parse_record_field = |_, _, _| crash ""
	parse_record_after_field = |_, _| crash ""
}

R : {}

expect {
	_ = R.parser_for(F.F)
	Bool.True
}
