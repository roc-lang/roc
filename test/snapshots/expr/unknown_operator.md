# META
~~~ini
description=Unknown operator, should produce an error
type=expr
~~~
# SOURCE
~~~roc
1 ++ 2
~~~
# EXPECTED
UNEXPECTED TOKEN IN EXPRESSION - unknown_operator.md:1:4:1:5
# PROBLEMS
**UNEXPECTED TOKEN IN EXPRESSION**
The token **+** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**unknown_operator.md:1:4:1:5:**
```roc
1 ++ 2
```
   ^


# TOKENS
~~~zig
Int,OpPlus,OpPlus,Int,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-binop (op "+")
	(e-int (raw "1"))
	(e-malformed (reason "expr_unexpected_token")))
~~~
# FORMATTED
~~~roc
1 + 
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
