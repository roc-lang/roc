# META
~~~ini
description=suffixed_question
type=expr
~~~
# SOURCE
~~~roc
Stdout.line???
~~~
# PROBLEMS
**UNEXPECTED TOKEN IN EXPRESSION**
The token **?** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**suffixed_question.md:1:14:1:15:**
```roc
Stdout.line???
```
             ^


**UNDEFINED VARIABLE**
Nothing is named `line` in this scope.
Is there an `import` or `exposing` missing up-top?

**UNKNOWN OPERATOR**
This looks like an operator, but it's not one I recognize!
Check the spelling and make sure you're using a valid Roc operator.

# TOKENS
~~~zig
UpperIdent(1:1-1:7),NoSpaceDotLowerIdent(1:7-1:12),OpDoubleQuestion(1:12-1:14),NoSpaceOpQuestion(1:14-1:15),EndOfFile(1:15-1:15),
~~~
# PARSE
~~~clojure
(e-binop @1.1-1.15 (op "??")
	(e-ident @1.1-1.12 (qaul "Stdout") (raw ".line"))
	(e-malformed @1.14-1.15 (reason "expr_unexpected_token")))
~~~
# FORMATTED
~~~roc
Stdout.line ?? 
~~~
# CANONICALIZE
~~~clojure
(e-binop @1.1-1.15 (op "null_coalesce") (id 77)
	(e-runtime-error (tag "ident_not_in_scope"))
	(e-runtime-error (tag "expr_not_canonicalized")))
~~~
# TYPES
~~~clojure
(expr (id 77) (type "*"))
~~~
