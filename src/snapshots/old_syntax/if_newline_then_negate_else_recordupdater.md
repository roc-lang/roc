# META
~~~ini
description=if_newline_then_negate_else_recordupdater
type=expr
~~~
# SOURCE
~~~roc
if
h
then!f#
else&m
~~~
# EXPECTED
UNEXPECTED TOKEN IN EXPRESSION - if_newline_then_negate_else_recordupdater.md:4:5:4:7
# PROBLEMS
**UNEXPECTED TOKEN IN EXPRESSION**
The token **&m** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**if_newline_then_negate_else_recordupdater.md:4:5:4:7:**
```roc
else&m
```
    ^^


**UNDEFINED VARIABLE**
Nothing is named `h` in this scope.
Is there an `import` or `exposing` missing up-top?

**UNDEFINED VARIABLE**
Nothing is named `then!f` in this scope.
Is there an `import` or `exposing` missing up-top?

**INVALID IF BRANCH**
The `else` branch of this `if` expression could not be processed.

The `else` branch must contain a valid expression. Check for syntax errors or missing values.

Note: Every `if` expression in Roc must have an `else` branch, and both branches must have the same type.

# TOKENS
~~~zig
KwIf(1:1-1:3),Newline(1:1-1:1),
LowerIdent(2:1-2:2),Newline(1:1-1:1),
LowerIdent(3:1-3:7),Newline(3:8-3:8),
KwElse(4:1-4:5),OpAmpersand(4:5-4:6),LowerIdent(4:6-4:7),EndOfFile(4:7-4:7),
~~~
# PARSE
~~~clojure
(e-if-then-else @1.1-4.7
	(e-ident @2.1-2.2 (raw "h"))
	(e-ident @3.1-3.7 (raw "then!f"))
	(e-malformed @4.5-4.7 (reason "expr_unexpected_token")))
~~~
# FORMATTED
~~~roc
if
	h
		then!f
			else 
~~~
# CANONICALIZE
~~~clojure
(e-if @1.1-4.7
	(if-branches
		(if-branch
			(e-runtime-error (tag "ident_not_in_scope"))
			(e-runtime-error (tag "ident_not_in_scope"))))
	(if-else
		(e-runtime-error (tag "if_else_not_canonicalized"))))
~~~
# TYPES
~~~clojure
(expr @1.1-4.7 (type "Error"))
~~~
