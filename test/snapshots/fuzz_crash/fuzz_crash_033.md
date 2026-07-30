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
EXPECTED RECORD FIELD - fuzz_crash_033.md:1:6:1:14
# PROBLEMS

┌───────────────────────┐
│ EXPECTED RECORD FIELD ├─ I was parsing a record expression, and I ──────────┐
└┬──────────────────────┘  expected a lowercase field name.                   │
 │                                                                            │
 │  { i, Complete]                                                            │
 │       ‾‾‾‾‾‾‾‾                                                             │
 └───────────────────────────────────────────────────── fuzz_crash_033.md:1:6 ┘

    Record fields start with lowercase names. After the name, either write `:
    value` or omit the value to use field punning.

    For example:
        { name: "Ada", age }

    I found `Complete` here.
    Names that start with uppercase letters are used for tags, type names, and
    mod names in Roc.

# TOKENS
~~~zig
OpenCurly,LowerIdent,Comma,UpperIdent,CloseSquare,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-malformed (reason "expected_expr_record_field_name"))
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
