# META
~~~ini
description=lambda_double_comma fail
type=expr
~~~
# SOURCE
~~~roc
\a,,b -> 1
~~~
~~~
# EXPECTED
UNEXPECTED TOKEN IN EXPRESSION - lambda_double_comma.md:1:1:1:3
# PROBLEMS
**UNEXPECTED TOKEN IN EXPRESSION**
The token **\a** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**lambda_double_comma.md:1:1:1:3:**
```roc
\a,,b -> 1
```
^^


# TOKENS
~~~zig
OpBackslash(1:1-1:2),LowerIdent(1:2-1:3),Comma(1:3-1:4),Comma(1:4-1:5),LowerIdent(1:5-1:6),OpArrow(1:7-1:9),Int(1:10-1:11),Newline(1:1-1:1),
MalformedUnknownToken(2:1-2:2),MalformedUnknownToken(2:2-2:3),MalformedUnknownToken(2:3-2:4),EndOfFile(2:4-2:4),
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
