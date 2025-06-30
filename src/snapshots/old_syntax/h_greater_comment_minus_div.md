# META
~~~ini
description=h_greater_comment_minus_div
type=expr
~~~
# SOURCE
~~~roc
h>#
 -h/d
~~~
# PROBLEMS
**UNEXPECTED TOKEN IN EXPRESSION**
The token **-h** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**h_greater_comment_minus_div.md:2:2:2:4:**
```roc
 -h/d
```
 ^^


**UNDEFINED VARIABLE**
Nothing is named `h` in this scope.
Is there an `import` or `exposing` missing up-top?

**UNKNOWN OPERATOR**
This looks like an operator, but it's not one I recognize!
Check the spelling and make sure you're using a valid Roc operator.

# TOKENS
~~~zig
LowerIdent(1:1-1:2),OpGreaterThan(1:2-1:3),Newline(1:4-1:4),
OpUnaryMinus(2:2-2:3),LowerIdent(2:3-2:4),OpSlash(2:4-2:5),LowerIdent(2:5-2:6),EndOfFile(2:6-2:6),
~~~
# PARSE
~~~clojure
(e-binop @1.1-2.4 (op ">")
	(e-ident @1.1-1.2 (qaul "") (raw "h"))
	(e-malformed @2.2-2.4 (reason "expr_unexpected_token")))
~~~
# FORMATTED
~~~roc
h >
	
~~~
# CANONICALIZE
~~~clojure
(e-binop @1.1-2.4 (op "gt") (id 77)
	(e-runtime-error (tag "ident_not_in_scope"))
	(e-runtime-error (tag "expr_not_canonicalized")))
~~~
# TYPES
~~~clojure
(expr (id 77) (type "*"))
~~~
