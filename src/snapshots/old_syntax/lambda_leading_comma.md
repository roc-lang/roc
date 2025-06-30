# META
~~~ini
description=lambda_leading_comma fail
type=expr
~~~
# SOURCE
~~~roc
\,b -> 1
~~~
# PROBLEMS
**UNEXPECTED TOKEN IN EXPRESSION**
The token **\,** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**lambda_leading_comma.md:1:1:1:3:**
```roc
\,b -> 1
```
^^


# TOKENS
~~~zig
OpBackslash(1:1-1:2),Comma(1:2-1:3),LowerIdent(1:3-1:4),OpArrow(1:5-1:7),Int(1:8-1:9),EndOfFile(1:9-1:9),
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
