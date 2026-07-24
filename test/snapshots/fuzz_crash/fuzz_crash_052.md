# META
~~~ini
description=fuzz crash
type=snippet
~~~
# SOURCE
~~~roc
S
0
~~~
# EXPECTED
TYPE APPLICATION NEEDS PARENTHESES - fuzz_crash_052.md:2:1:2:2
# PROBLEMS

┌────────────────────────────────────┐
│ TYPE APPLICATION NEEDS PARENTHESES ├─ I was parsing a type annotation, ─────┐
└┬───────────────────────────────────┘  and I found a type argument without   │
 │                                      parentheses.                          │
 │                                                                            │
 │  0                                                                         │
 │  ‾                                                                         │
 └───────────────────────────────────────────────────── fuzz_crash_052.md:2:1 ┘

    Roc type applications use parentheses around their arguments. Write
    `List(U8)`, not `List U8`.

    For example:
        List(U8)

    I found `0` here.

# TOKENS
~~~zig
UpperIdent,
Int,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-mod)
	(statements
		(s-malformed (tag "expected_colon_after_type_annotation"))))
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
