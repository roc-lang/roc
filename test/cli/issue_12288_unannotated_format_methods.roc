# https://github.com/roc-lang/roc/issues/12288
# Unannotated format methods receive compiler-generated record field and
# tag-union specifications.
F := [F].{
	parse_record_start = |_, _| crash ""
	skip_record_field = |_, _| crash ""
	parse_record_field = |_, _, _| crash ""
	parse_record_after_field = |_, _| crash ""
	parse_str = |_, _| crash ""
	rename_field = |_, name| name
	parse_tag_union = |_, _, _| crash ""
}

R : { a : Str }

T : [A, B]

expect {
	_ = R.parser_for(F.F)
	Bool.True
}

expect {
	_ = T.parser_for(F.F)
	Bool.True
}
