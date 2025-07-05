# META
~~~ini
description=list_comment_newline
type=expr
~~~
# SOURCE
~~~roc
[L#
,

]
~~~
~~~
# EXPECTED
LIST NOT CLOSED - list_comment_newline.md:4:1:4:1
# PROBLEMS
**LIST NOT CLOSED**
This list is missing a closing bracket or has a syntax error.
Lists must be closed with **]** and list items must be separated by commas.
For example:     [1, 2, 3]

Here is the problematic code:
**list_comment_newline.md:4:1:4:1:**
```roc
]
```



# TOKENS
~~~zig
OpenSquare(1:1-1:2),UpperIdent(1:2-1:3),Newline(1:4-1:4),
Comma(2:1-2:2),Newline(1:1-1:1),
Newline(1:1-1:1),
CloseSquare(4:1-4:2),Newline(1:1-1:1),
MalformedUnknownToken(5:1-5:2),MalformedUnknownToken(5:2-5:3),MalformedUnknownToken(5:3-5:4),EndOfFile(5:4-5:4),
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
