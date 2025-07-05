# META
~~~ini
description=not_double_parens
type=expr
~~~
# SOURCE
~~~roc
!
((
E)
)
~~~
# EXPECTED
NIL
# PROBLEMS
**UNEXPECTED TOKEN IN EXPRESSION**
The token  is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**not_double_parens.md:1:1:1:1:**
```roc
!
```



# TOKENS
~~~zig
OpBang(1:1-1:2),Newline(1:1-1:1),
OpenRound(2:1-2:2),NoSpaceOpenRound(2:2-2:3),Newline(1:1-1:1),
UpperIdent(3:1-3:2),CloseRound(3:2-3:3),Newline(1:1-1:1),
CloseRound(4:1-4:2),EndOfFile(4:2-4:2),
~~~
# PARSE
~~~clojure
(e-malformed @1.1-1.1 (reason "expr_unexpected_token"))
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
