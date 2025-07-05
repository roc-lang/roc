# META
~~~ini
description=not_tag
type=expr
~~~
# SOURCE
~~~roc
!(C
2)
~~~
# EXPECTED
NIL
# PROBLEMS
**UNEXPECTED TOKEN IN EXPRESSION**
The token **!(** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**not_tag.md:1:1:1:3:**
```roc
!(C
```
^^


# TOKENS
~~~zig
OpBang(1:1-1:2),NoSpaceOpenRound(1:2-1:3),UpperIdent(1:3-1:4),Newline(1:1-1:1),
Int(2:1-2:2),CloseRound(2:2-2:3),EndOfFile(2:3-2:3),
~~~
# PARSE
~~~clojure
(e-malformed @1.1-1.3 (reason "expr_unexpected_token"))
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
