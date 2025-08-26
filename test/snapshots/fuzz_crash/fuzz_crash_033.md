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
**PARSE ERROR**
A parsing error occurred: `expected_expr_record_field_name`
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_033.md:1:6:1:14:**
```roc
{ i, Complete]
```
     ^^^^^^^^


**PARSE ERROR**
A parsing error occurred: `expected_expr_close_curly_or_comma`
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_033.md:1:14:1:15:**
```roc
{ i, Complete]
```
             ^


# TOKENS
~~~zig
OpenCurly(1:1-1:2),LowerIdent(1:3-1:4),Comma(1:4-1:5),UpperIdent(1:6-1:14),CloseSquare(1:14-1:15),
EndOfFile(2:1-2:1),
~~~
# PARSE
~~~clojure
(e-malformed @1.14-1.15 (reason "expected_expr_close_curly_or_comma"))
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
