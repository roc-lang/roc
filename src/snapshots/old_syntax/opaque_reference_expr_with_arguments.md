# META
~~~ini
description=opaque_reference_expr_with_arguments
type=expr
~~~
# SOURCE
~~~roc
@Age m n
~~~
~~~
# EXPECTED
UNEXPECTED TOKEN IN EXPRESSION - opaque_reference_expr_with_arguments.md:1:1:1:7
# PROBLEMS
**UNEXPECTED TOKEN IN EXPRESSION**
The token **@Age m** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**opaque_reference_expr_with_arguments.md:1:1:1:7:**
```roc
@Age m n
```
^^^^^^


# TOKENS
~~~zig
OpaqueName(1:1-1:5),LowerIdent(1:6-1:7),LowerIdent(1:8-1:9),Newline(1:1-1:1),
MalformedUnknownToken(2:1-2:2),MalformedUnknownToken(2:2-2:3),MalformedUnknownToken(2:3-2:4),EndOfFile(2:4-2:4),
~~~
# PARSE
~~~clojure
(e-malformed @1.1-1.7 (reason "expr_unexpected_token"))
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
