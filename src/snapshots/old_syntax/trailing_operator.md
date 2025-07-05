# META
~~~ini
description=trailing_operator fail
type=expr
~~~
# SOURCE
~~~roc
J-
~~~
~~~
# EXPECTED
UNEXPECTED TOKEN IN EXPRESSION - trailing_operator.md:2:1:2:3
# PROBLEMS
**UNEXPECTED TOKEN IN EXPRESSION**
The token **~~** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**trailing_operator.md:2:1:2:3:**
```roc
~~~
```
^^


**UNKNOWN OPERATOR**
This looks like an operator, but it's not one I recognize!
Check the spelling and make sure you're using a valid Roc operator.

# TOKENS
~~~zig
UpperIdent(1:1-1:2),OpBinaryMinus(1:2-1:3),Newline(1:1-1:1),
MalformedUnknownToken(2:1-2:2),MalformedUnknownToken(2:2-2:3),MalformedUnknownToken(2:3-2:4),EndOfFile(2:4-2:4),
~~~
# PARSE
~~~clojure
(e-binop @1.1-2.3 (op "-")
	(e-tag @1.1-1.2 (raw "J"))
	(e-malformed @2.1-2.3 (reason "expr_unexpected_token")))
~~~
# FORMATTED
~~~roc
J -
	
~~~
# CANONICALIZE
~~~clojure
(e-binop @1.1-2.3 (op "sub")
	(e-tag @1.1-1.2 (name "J"))
	(e-runtime-error (tag "expr_not_canonicalized")))
~~~
# TYPES
~~~clojure
(expr @1.1-2.3 (type "*"))
~~~
