# META
~~~ini
description=i_over_not_g
type=expr
~~~
# SOURCE
~~~roc
i/
 !
 g
~~~
# PROBLEMS
**UNEXPECTED TOKEN IN EXPRESSION**
The token  is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**i_over_not_g.md:2:2:2:2:**
```roc
 !
```
 


**UNDEFINED VARIABLE**
Nothing is named `i` in this scope.
Is there an `import` or `exposing` missing up-top?

**UNKNOWN OPERATOR**
This looks like an operator, but it's not one I recognize!
Check the spelling and make sure you're using a valid Roc operator.

# TOKENS
~~~zig
LowerIdent(1:1-1:2),OpSlash(1:2-1:3),Newline(1:1-1:1),
OpBang(2:2-2:3),Newline(1:1-1:1),
LowerIdent(3:2-3:3),EndOfFile(3:3-3:3),
~~~
# PARSE
~~~clojure
(e-binop @1.1-1.1 (op "/")
	(e-ident @1.1-1.2 (qaul "") (raw "i"))
	(e-malformed @1.1-1.1 (reason "expr_unexpected_token")))
~~~
# FORMATTED
~~~roc
i /
	
~~~
# CANONICALIZE
~~~clojure
(e-binop @1.1-1.1 (op "div") (id 77)
	(e-runtime-error (tag "ident_not_in_scope"))
	(e-runtime-error (tag "expr_not_canonicalized")))
~~~
# TYPES
~~~clojure
(expr (id 77) (type "*"))
~~~
