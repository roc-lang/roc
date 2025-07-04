# META
~~~ini
description=binop_assign_defs_nested
type=expr
~~~
# SOURCE
~~~roc
5-((e=((
r))
1))
~~~
# PROBLEMS
**PARSE ERROR**
A parsing error occurred: `expected_expr_close_round_or_comma`
This is an unexpected parsing error. Please check your syntax.

Here is the problematic code:
**binop_assign_defs_nested.md:2:2:2:4:**
```roc
r))
```
 ^^


**INVALID TUPLE ELEMENT**
This tuple element is malformed or contains invalid syntax.

# TOKENS
~~~zig
Int(1:1-1:2),OpBinaryMinus(1:2-1:3),NoSpaceOpenRound(1:3-1:4),NoSpaceOpenRound(1:4-1:5),LowerIdent(1:5-1:6),OpAssign(1:6-1:7),NoSpaceOpenRound(1:7-1:8),NoSpaceOpenRound(1:8-1:9),Newline(1:1-1:1),
LowerIdent(2:1-2:2),CloseRound(2:2-2:3),CloseRound(2:3-2:4),Newline(1:1-1:1),
Int(3:1-3:2),CloseRound(3:2-3:3),CloseRound(3:3-3:4),EndOfFile(3:4-3:4),
~~~
# PARSE
~~~clojure
(e-binop @1.1-3.2 (op "-")
	(e-int @1.1-1.2 (raw "5"))
	(e-tuple @1.3-2.4
		(e-malformed @2.2-2.4 (reason "expected_expr_close_round_or_comma"))))
~~~
# FORMATTED
~~~roc
5 - (
	,
)
~~~
# CANONICALIZE
~~~clojure
(e-binop @1.1-3.2 (op "sub")
	(e-int @1.1-1.2 (value "5"))
	(e-runtime-error (tag "tuple_elem_not_canonicalized")))
~~~
# TYPES
~~~clojure
(expr @1.1-3.2 (type "*"))
~~~
