# META
~~~ini
description=expr_to_pattern_fail fail
type=expr
~~~
# SOURCE
~~~roc
.e,
~~~
~~~
# EXPECTED
UNEXPECTED TOKEN IN EXPRESSION - expr_to_pattern_fail.md:1:1:1:4
# PROBLEMS
**UNEXPECTED TOKEN IN EXPRESSION**
The token **.e,** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**expr_to_pattern_fail.md:1:1:1:4:**
```roc
.e,
```
^^^


# TOKENS
~~~zig
DotLowerIdent(1:1-1:3),Comma(1:3-1:4),Newline(1:1-1:1),
MalformedUnknownToken(2:1-2:2),MalformedUnknownToken(2:2-2:3),MalformedUnknownToken(2:3-2:4),EndOfFile(2:4-2:4),
~~~
# PARSE
~~~clojure
(e-malformed @1.1-1.4 (reason "expr_unexpected_token"))
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
