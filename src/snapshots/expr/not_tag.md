# META
~~~ini
description=not_tag
type=expr
~~~
# SOURCE
~~~roc
!(C(2))
~~~
# EXPECTED
UNEXPECTED TOKEN IN EXPRESSION - not_tag.md:1:1:1:3
# PROBLEMS
**UNEXPECTED TOKEN IN EXPRESSION**
The token **!(** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**not_tag.md:1:1:1:3:**
```roc
!(C(2))
```
^^


# TOKENS
~~~zig
OpBang(1:1-1:2),NoSpaceOpenRound(1:2-1:3),UpperIdent(1:3-1:4),NoSpaceOpenRound(1:4-1:5),Int(1:5-1:6),CloseRound(1:6-1:7),CloseRound(1:7-1:8),EndOfFile(1:8-1:8),
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
