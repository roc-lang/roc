# META
~~~ini
description=min_parens_number
type=expr
~~~
# SOURCE
~~~roc
-(8)
~~~
~~~
# EXPECTED
UNEXPECTED TOKEN IN EXPRESSION - min_parens_number.md:1:1:1:3
# PROBLEMS
**UNEXPECTED TOKEN IN EXPRESSION**
The token **-(** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**min_parens_number.md:1:1:1:3:**
```roc
-(8)
```
^^


# TOKENS
~~~zig
OpUnaryMinus(1:1-1:2),NoSpaceOpenRound(1:2-1:3),Int(1:3-1:4),CloseRound(1:4-1:5),Newline(1:1-1:1),
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
