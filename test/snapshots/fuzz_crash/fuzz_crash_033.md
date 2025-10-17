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
