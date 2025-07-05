# META
~~~ini
description=opaque_reference_expr
type=expr
~~~
# SOURCE
~~~roc
@Age
~~~
~~~
# EXPECTED
UNEXPECTED TOKEN IN EXPRESSION - opaque_reference_expr.md:1:1:1:1
# PROBLEMS
**UNEXPECTED TOKEN IN EXPRESSION**
The token  is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**opaque_reference_expr.md:1:1:1:1:**
```roc
@Age
```



# TOKENS
~~~zig
OpaqueName(1:1-1:5),Newline(1:1-1:1),
MalformedUnknownToken(2:1-2:2),MalformedUnknownToken(2:2-2:3),MalformedUnknownToken(2:3-2:4),EndOfFile(2:4-2:4),
~~~
# PARSE
~~~clojure
(e-malformed @1.1-1.1 (reason "expr_unexpected_token"))
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
