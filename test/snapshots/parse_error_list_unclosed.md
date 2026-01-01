# META
~~~ini
description=List that is not closed
type=expr
~~~
# SOURCE
~~~roc
[1, 2, 3
~~~
# EXPECTED
LIST NOT CLOSED - parse_error_list_unclosed.md:2:1:2:1
# PROBLEMS
**LIST NOT CLOSED**
This list is missing a closing bracket or has a syntax error.
Lists must be closed with **]** and list items must be separated by commas.
For example:     [1, 2, 3]

**parse_error_list_unclosed.md:2:1:2:1:**
```roc

```
^


# TOKENS
~~~zig
OpenSquare,Int,Comma,Int,Comma,Int,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-malformed (reason "expected_expr_close_square_or_comma"))
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
