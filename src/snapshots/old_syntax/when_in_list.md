# META
~~~ini
description=when_in_list
type=expr
~~~
# SOURCE
~~~roc
[when 2 is 8->[
]]
~~~
# EXPECTED
LIST NOT CLOSED - when_in_list.md:2:1:2:3
# PROBLEMS
**LIST NOT CLOSED**
This list is missing a closing bracket or has a syntax error.
Lists must be closed with **]** and list items must be separated by commas.
For example:     [1, 2, 3]

Here is the problematic code:
**when_in_list.md:2:1:2:3:**
```roc
]]
```
^^


# TOKENS
~~~zig
OpenSquare(1:1-1:2),LowerIdent(1:2-1:6),Int(1:7-1:8),LowerIdent(1:9-1:11),Int(1:12-1:13),OpArrow(1:13-1:15),OpenSquare(1:15-1:16),Newline(1:1-1:1),
CloseSquare(2:1-2:2),CloseSquare(2:2-2:3),EndOfFile(2:3-2:3),
~~~
# PARSE
~~~clojure
(e-malformed @2.1-2.3 (reason "expected_expr_close_square_or_comma"))
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
