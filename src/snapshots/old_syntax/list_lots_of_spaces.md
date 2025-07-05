# META
~~~ini
description=list_lots_of_spaces
type=expr
~~~
# SOURCE
~~~roc
[J
#
,

#
u]
~~~
~~~
# EXPECTED
LIST NOT CLOSED - list_lots_of_spaces.md:6:2:6:2
# PROBLEMS
**LIST NOT CLOSED**
This list is missing a closing bracket or has a syntax error.
Lists must be closed with **]** and list items must be separated by commas.
For example:     [1, 2, 3]

Here is the problematic code:
**list_lots_of_spaces.md:6:2:6:2:**
```roc
u]
```
 


# TOKENS
~~~zig
OpenSquare(1:1-1:2),UpperIdent(1:2-1:3),Newline(1:1-1:1),
Newline(2:2-2:2),
Comma(3:1-3:2),Newline(1:1-1:1),
Newline(1:1-1:1),
Newline(5:2-5:2),
LowerIdent(6:1-6:2),CloseSquare(6:2-6:3),Newline(1:1-1:1),
MalformedUnknownToken(7:1-7:2),MalformedUnknownToken(7:2-7:3),MalformedUnknownToken(7:3-7:4),EndOfFile(7:4-7:4),
~~~
# PARSE
~~~clojure
(e-malformed @1.1-1.1 (reason "expected_expr_close_square_or_comma"))
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
