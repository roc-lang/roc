# META
~~~ini
description=Issue 9374 - Formatter must not crash with integer underflow on a record type whose extension has a trailing comma
type=snippet
skip=true
# TODO: Currently panics in fmt.zig formatRecordWithExtension with integer overflow on `curr_indent -= 1`
#       because curr_indent was never incremented when fields.len == 0. Unskip once fixed.
~~~
# SOURCE
~~~roc
x : { ..a, }
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# FORMATTED
~~~roc
x : {
	..a,
}
~~~
