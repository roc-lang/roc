# META
~~~ini
description=Record that is not closed
type=expr
~~~
# SOURCE
~~~roc
{ x: 1, y: 2
~~~
# EXPECTED
PARSE ERROR - parse_error_record_unclosed.md:2:1:2:1
# PROBLEMS
**PARSE ERROR**
A parsing error occurred: `expected_expr_close_curly_or_comma`
This is an unexpected parsing error. Please check your syntax.

**parse_error_record_unclosed.md:2:1:2:1:**
```roc

```
^


# TOKENS
~~~zig
OpenCurly,LowerIdent,OpColon,Int,Comma,LowerIdent,OpColon,Int,
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
