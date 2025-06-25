# META
~~~ini
description=Unary not operation expression
type=expr
~~~
# SOURCE
~~~roc
!isValid
~~~
# PROBLEMS
**UNEXPECTED TOKEN IN EXPRESSION**
The token **!isValid** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**unary_op_not.md:1:1:1:9:**
```roc
!isValid
```


# TOKENS
~~~zig
OpBang(1:1-1:2),LowerIdent(1:2-1:9),EndOfFile(1:9-1:9),
~~~
# PARSE
~~~clojure
(e-malformed @1-1-1-9 (reason "expr_unexpected_token"))
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