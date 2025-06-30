# META
~~~ini
description=neg_inf_float
type=expr
~~~
# SOURCE
~~~roc
-inf
~~~
# PROBLEMS
**UNEXPECTED TOKEN IN EXPRESSION**
The token **-inf** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**neg_inf_float.md:1:1:1:5:**
```roc
-inf
```
^^^^


# TOKENS
~~~zig
OpUnaryMinus(1:1-1:2),LowerIdent(1:2-1:5),EndOfFile(1:5-1:5),
~~~
# PARSE
~~~clojure
(e-malformed @1.1-1.5 (reason "expr_unexpected_token"))
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
