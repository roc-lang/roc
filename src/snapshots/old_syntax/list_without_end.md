# META
~~~ini
description=list_without_end fail
type=expr
~~~
# SOURCE
~~~roc
[1, 2,
~~~
# EXPECTED
expected_expr_close_square_or_comma - list_without_end.md:1:7:1:7
# PROBLEMS
**LIST NOT CLOSED**
This list is missing a closing bracket or has a syntax error.
Lists must be closed with **]** and list items must be separated by commas.
For example:     [1, 2, 3]

Here is the problematic code:
**list_without_end.md:1:7:1:7:**
```roc
[1, 2,
```
      


# TOKENS
~~~zig
OpenSquare(1:1-1:2),Int(1:2-1:3),Comma(1:3-1:4),Int(1:5-1:6),Comma(1:6-1:7),EndOfFile(1:7-1:7),
~~~
# PARSE
~~~clojure
(e-malformed @1.7-1.7 (reason "expected_expr_close_square_or_comma"))
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
