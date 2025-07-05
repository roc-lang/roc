# META
~~~ini
description=minus_newline_minus_minus
type=expr
~~~
# SOURCE
~~~roc
p-
 -t-1
~~~
# EXPECTED
UNEXPECTED TOKEN IN EXPRESSION - minus_newline_minus_minus.md:2:2:2:4
# PROBLEMS
**UNEXPECTED TOKEN IN EXPRESSION**
The token **-t** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**minus_newline_minus_minus.md:2:2:2:4:**
```roc
 -t-1
```
 ^^


**UNDEFINED VARIABLE**
Nothing is named `p` in this scope.
Is there an `import` or `exposing` missing up-top?

**UNKNOWN OPERATOR**
This looks like an operator, but it's not one I recognize!
Check the spelling and make sure you're using a valid Roc operator.

# TOKENS
~~~zig
LowerIdent(1:1-1:2),OpBinaryMinus(1:2-1:3),Newline(1:1-1:1),
OpUnaryMinus(2:2-2:3),LowerIdent(2:3-2:4),OpBinaryMinus(2:4-2:5),Int(2:5-2:6),EndOfFile(2:6-2:6),
~~~
# PARSE
~~~clojure
(e-binop @1.1-2.4 (op "-")
	(e-ident @1.1-1.2 (raw "p"))
	(e-malformed @2.2-2.4 (reason "expr_unexpected_token")))
~~~
# FORMATTED
~~~roc
p -
	
~~~
# CANONICALIZE
~~~clojure
(e-binop @1.1-2.4 (op "sub")
	(e-runtime-error (tag "ident_not_in_scope"))
	(e-runtime-error (tag "expr_not_canonicalized")))
~~~
# TYPES
~~~clojure
(expr @1.1-2.4 (type "*"))
~~~
