# META
~~~ini
description=unary_negation_access
type=expr
~~~
# SOURCE
~~~roc
-rec1.field
~~~
# PROBLEMS
**UNEXPECTED TOKEN IN EXPRESSION**
The token **-rec1** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**unary_negation_access.md:1:1:1:6:**
```roc
-rec1.field
```
^^^^^


# TOKENS
~~~zig
OpUnaryMinus(1:1-1:2),LowerIdent(1:2-1:6),NoSpaceDotLowerIdent(1:6-1:12),EndOfFile(1:12-1:12),
~~~
# PARSE
~~~clojure
(e-malformed @1.1-1.6 (reason "expr_unexpected_token"))
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
