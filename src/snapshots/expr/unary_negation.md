# META
~~~ini
description=unary_negation
type=expr
~~~
# SOURCE
~~~roc
-foo
~~~
# EXPECTED
UNEXPECTED TOKEN IN EXPRESSION - unary_negation.md:1:1:1:2
# PROBLEMS
**UNEXPECTED TOKEN IN EXPRESSION**
The token **-** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**unary_negation.md:1:1:1:2:**
```roc
-foo
```
^


# TOKENS
~~~zig
OpUnaryMinus(1:1-1:2),LowerIdent(1:2-1:5),EndOfFile(1:5-1:5),
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
