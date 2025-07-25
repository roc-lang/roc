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

Here is the problematic code:
**unknown_operator.md:1:4:1:5:**
```roc
1 ++ 2
```
   ^


# TOKENS
~~~zig
Int(1:1-1:2),OpPlus(1:3-1:4),OpPlus(1:4-1:5),Int(1:6-1:7),EndOfFile(1:7-1:7),
~~~
# PARSE
~~~clojure
(e-binop @1.1-1.5 (op "+")
	(e-int @1.1-1.2 (raw "1"))
	(e-malformed @1.4-1.5 (reason "expr_unexpected_token")))
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
