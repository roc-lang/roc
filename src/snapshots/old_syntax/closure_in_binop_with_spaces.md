# META
~~~ini
description=closure_in_binop_with_spaces
type=expr
~~~
# SOURCE
~~~roc
i>\s->s
-a
~~~
# PROBLEMS
**UNEXPECTED TOKEN IN EXPRESSION**
The token **\s** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**closure_in_binop_with_spaces.md:1:3:1:5:**
```roc
i>\s->s
```
  ^^


**UNDEFINED VARIABLE**
Nothing is named `i` in this scope.
Is there an `import` or `exposing` missing up-top?

**UNKNOWN OPERATOR**
This looks like an operator, but it's not one I recognize!
Check the spelling and make sure you're using a valid Roc operator.

# TOKENS
~~~zig
LowerIdent(1:1-1:2),OpGreaterThan(1:2-1:3),OpBackslash(1:3-1:4),LowerIdent(1:4-1:5),OpArrow(1:5-1:7),LowerIdent(1:7-1:8),Newline(1:1-1:1),
OpUnaryMinus(2:1-2:2),LowerIdent(2:2-2:3),EndOfFile(2:3-2:3),
~~~
# PARSE
~~~clojure
(e-binop @1.1-1.5 (op ">")
	(e-ident @1.1-1.2 (qaul "") (raw "i"))
	(e-malformed @1.3-1.5 (reason "expr_unexpected_token")))
~~~
# FORMATTED
~~~roc
i > 
~~~
# CANONICALIZE
~~~clojure
(e-binop @1.1-1.5 (op "gt")
	(e-runtime-error (tag "ident_not_in_scope"))
	(e-runtime-error (tag "expr_not_canonicalized")))
~~~
# TYPES
~~~clojure
(expr @1.1-1.5 (type "a"))
~~~
