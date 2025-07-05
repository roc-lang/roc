# META
~~~ini
description=dbg_bang_neg_bang_if_bang fail
type=expr
~~~
# SOURCE
~~~roc
dbg!-!if!
~~~
# PROBLEMS
**UNEXPECTED TOKEN IN EXPRESSION**
The token **!if!** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**dbg_bang_neg_bang_if_bang.md:1:6:1:10:**
```roc
dbg!-!if!
```
     ^^^^


**UNDEFINED VARIABLE**
Nothing is named `dbg!` in this scope.
Is there an `import` or `exposing` missing up-top?

**UNKNOWN OPERATOR**
This looks like an operator, but it's not one I recognize!
Check the spelling and make sure you're using a valid Roc operator.

# TOKENS
~~~zig
LowerIdent(1:1-1:5),OpBinaryMinus(1:5-1:6),OpBang(1:6-1:7),LowerIdent(1:7-1:10),EndOfFile(1:10-1:10),
~~~
# PARSE
~~~clojure
(e-binop @1.1-1.10 (op "-")
	(e-ident @1.1-1.5 (raw "dbg!"))
	(e-malformed @1.6-1.10 (reason "expr_unexpected_token")))
~~~
# FORMATTED
~~~roc
dbg! - 
~~~
# CANONICALIZE
~~~clojure
(e-binop @1.1-1.10 (op "sub")
	(e-runtime-error (tag "ident_not_in_scope"))
	(e-runtime-error (tag "expr_not_canonicalized")))
~~~
# TYPES
~~~clojure
(expr @1.1-1.10 (type "*"))
~~~
