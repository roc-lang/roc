# META
~~~ini
description=Unary minus and boolean not (should error)
type=expr
~~~
# SOURCE
~~~roc
-!h
~~~
# EXPECTED
UNEXPECTED TOKEN IN EXPRESSION - minus_not_h.md:1:1:1:2
# PROBLEMS
**UNEXPECTED TOKEN IN EXPRESSION**
The token **-** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**minus_not_h.md:1:1:1:2:**
```roc
-!h
```
^


# TOKENS
~~~zig
OpBinaryMinus(1:1-1:2),OpBang(1:2-1:3),LowerIdent(1:3-1:4),
EndOfFile(2:1-2:1),
~~~
# PARSE
~~~clojure
(e-malformed @1.1-1.2 (reason "expr_unexpected_token"))
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
