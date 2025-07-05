# META
~~~ini
description=binop_closure_apply
type=expr
~~~
# SOURCE
~~~roc
d+
 \w->x
 x
~~~
~~~
# EXPECTED
UNEXPECTED TOKEN IN EXPRESSION - binop_closure_apply.md:2:2:2:4
# PROBLEMS
**UNEXPECTED TOKEN IN EXPRESSION**
The token **\w** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**binop_closure_apply.md:2:2:2:4:**
```roc
 \w->x
```
 ^^


**UNDEFINED VARIABLE**
Nothing is named `d` in this scope.
Is there an `import` or `exposing` missing up-top?

**UNKNOWN OPERATOR**
This looks like an operator, but it's not one I recognize!
Check the spelling and make sure you're using a valid Roc operator.

# TOKENS
~~~zig
LowerIdent(1:1-1:2),OpPlus(1:2-1:3),Newline(1:1-1:1),
OpBackslash(2:2-2:3),LowerIdent(2:3-2:4),OpArrow(2:4-2:6),LowerIdent(2:6-2:7),Newline(1:1-1:1),
LowerIdent(3:2-3:3),Newline(1:1-1:1),
MalformedUnknownToken(4:1-4:2),MalformedUnknownToken(4:2-4:3),MalformedUnknownToken(4:3-4:4),EndOfFile(4:4-4:4),
~~~
# PARSE
~~~clojure
(e-binop @1.1-2.4 (op "+")
	(e-ident @1.1-1.2 (raw "d"))
	(e-malformed @2.2-2.4 (reason "expr_unexpected_token")))
~~~
# FORMATTED
~~~roc
d +
	
~~~
# CANONICALIZE
~~~clojure
(e-binop @1.1-2.4 (op "add")
	(e-runtime-error (tag "ident_not_in_scope"))
	(e-runtime-error (tag "expr_not_canonicalized")))
~~~
# TYPES
~~~clojure
(expr @1.1-2.4 (type "*"))
~~~
