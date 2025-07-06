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
UNEXPECTED TOKEN IN EXPRESSION - minus_not_h.md:1:1:1:3
# PROBLEMS
**UNEXPECTED TOKEN IN EXPRESSION**
The token **-!** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**minus_not_h.md:1:1:1:3:**
```roc
-!h
```
^^


# TOKENS
~~~zig
OpUnaryMinus(1:1-1:2),OpBang(1:2-1:3),LowerIdent(1:3-1:4),EndOfFile(1:4-1:4),
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
