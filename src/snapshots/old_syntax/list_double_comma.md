# META
~~~ini
description=list_double_comma fail
type=expr
~~~
# SOURCE
~~~roc
[1, 2, , 3]
~~~
# EXPECTED
UNEXPECTED TOKEN IN EXPRESSION - list_double_comma.md:1:8:1:11
# PROBLEMS
**UNEXPECTED TOKEN IN EXPRESSION**
The token **, 3** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**list_double_comma.md:1:8:1:11:**
```roc
[1, 2, , 3]
```
       ^^^


**LIST NOT CLOSED**
This list is missing a closing bracket or has a syntax error.
Lists must be closed with **]** and list items must be separated by commas.
For example:     [1, 2, 3]

Here is the problematic code:
**list_double_comma.md:1:11:1:12:**
```roc
[1, 2, , 3]
```
          ^


# TOKENS
~~~zig
OpenSquare(1:1-1:2),Int(1:2-1:3),Comma(1:3-1:4),Int(1:5-1:6),Comma(1:6-1:7),Comma(1:8-1:9),Int(1:10-1:11),CloseSquare(1:11-1:12),EndOfFile(1:12-1:12),
~~~
# PARSE
~~~clojure
(e-malformed @1.11-1.12 (reason "expected_expr_close_square_or_comma"))
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
