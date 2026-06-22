# META
~~~ini
description=minimal reproduction of record parsing index out of bounds crash
type=expr
~~~
# SOURCE
~~~roc
{ i, Complete]
~~~
# EXPECTED
PARSE ERROR - fuzz_crash_033.md:1:6:1:14
PARSE ERROR - fuzz_crash_033.md:1:14:1:15
# PROBLEMS
                                                                 ┌─────────────┐
┌─ A parsing error occurred: expected_expr_record_field_name ────┤ PARSE ERROR │
│                                                                └────────────┬┘
│                                                                             │
│  { i, Complete]                                                             │
│       ‾‾‾‾‾‾‾‾                                                              │
└─────────────────────────────────────────────────────────────────────────────┘
    fuzz_crash_033.md:1:6

    This is an unexpected parsing error. Please check your syntax.
                                                                 ┌─────────────┐
┌─ A parsing error occurred: expected_expr_close_curly_or_comma ─┤ PARSE ERROR │
│                                                                └────────────┬┘
│                                                                             │
│  { i, Complete]                                                             │
│               ‾                                                             │
└─────────────────────────────────────────────────────────────────────────────┘
    fuzz_crash_033.md:1:14

    This is an unexpected parsing error. Please check your syntax.
# TOKENS
~~~zig
OpenCurly,LowerIdent,Comma,UpperIdent,CloseSquare,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-malformed (reason "expected_expr_close_curly_or_comma"))
~~~
# FORMATTED
~~~roc

~~~
# CANONICALIZE
~~~clojure
(can-ir (empty true))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs)
	(expressions))
~~~
