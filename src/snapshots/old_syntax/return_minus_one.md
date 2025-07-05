# META
~~~ini
description=return_minus_one
type=expr
~~~
# SOURCE
~~~roc
return-r
1
~~~
~~~
# EXPECTED
UNEXPECTED TOKEN IN EXPRESSION - return_minus_one.md:1:1:1:8
# PROBLEMS
**UNEXPECTED TOKEN IN EXPRESSION**
The token **return-** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**return_minus_one.md:1:1:1:8:**
```roc
return-r
```
^^^^^^^


# TOKENS
~~~zig
KwReturn(1:1-1:7),OpBinaryMinus(1:7-1:8),LowerIdent(1:8-1:9),Newline(1:1-1:1),
Int(2:1-2:2),Newline(1:1-1:1),
MalformedUnknownToken(3:1-3:2),MalformedUnknownToken(3:2-3:3),MalformedUnknownToken(3:3-3:4),EndOfFile(3:4-3:4),
~~~
# PARSE
~~~clojure
(e-malformed @1.1-1.8 (reason "expr_unexpected_token"))
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
