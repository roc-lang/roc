# META
~~~ini
description=single_underscore_closure
type=expr
~~~
# SOURCE
~~~roc
\_ -> 42
~~~
# EXPECTED
NIL
# PROBLEMS
**UNEXPECTED TOKEN IN EXPRESSION**
The token **\_** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**single_underscore_closure.md:1:1:1:3:**
```roc
\_ -> 42
```
^^


# TOKENS
~~~zig
OpBackslash(1:1-1:2),Underscore(1:2-1:3),OpArrow(1:4-1:6),Int(1:7-1:9),EndOfFile(1:9-1:9),
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
