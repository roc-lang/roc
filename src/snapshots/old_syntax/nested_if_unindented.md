# META
~~~ini
description=nested_if_unindented
type=expr
~~~
# SOURCE
~~~roc
if""then-p else
if""then-p else.e
~~~
# PROBLEMS
**UNEXPECTED TOKEN IN EXPRESSION**
The token **.e** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**nested_if_unindented.md:2:16:2:18:**
```roc
if""then-p else.e
```
               ^^


**UNDEFINED VARIABLE**
Nothing is named `then` in this scope.
Is there an `import` or `exposing` missing up-top?

**UNDEFINED VARIABLE**
Nothing is named `p` in this scope.
Is there an `import` or `exposing` missing up-top?

**UNDEFINED VARIABLE**
Nothing is named `then` in this scope.
Is there an `import` or `exposing` missing up-top?

**UNDEFINED VARIABLE**
Nothing is named `p` in this scope.
Is there an `import` or `exposing` missing up-top?

**INVALID IF BRANCH**
The `else` branch of this `if` expression could not be processed.

The `else` branch must contain a valid expression. Check for syntax errors or missing values.

Note: Every `if` expression in Roc must have an `else` branch, and both branches must have the same type.

**INVALID IF CONDITION**
This `if` condition needs to be a _Bool_:
**nested_if_unindented.md:1:3:**
```roc
if""then-p else
```
  ^^

Right now, it has the type:
    _Str_

Every `if` condition must evaluate to a _Bool_–either `True` or `False`.

**INVALID IF CONDITION**
This `if` condition needs to be a _Bool_:
**nested_if_unindented.md:2:3:**
```roc
if""then-p else.e
```
  ^^

Right now, it has the type:
    _Str_

Every `if` condition must evaluate to a _Bool_–either `True` or `False`.

# TOKENS
~~~zig
KwIf(1:1-1:3),StringStart(1:3-1:4),StringPart(1:4-1:4),StringEnd(1:4-1:5),LowerIdent(1:5-1:9),OpBinaryMinus(1:9-1:10),LowerIdent(1:10-1:11),KwElse(1:12-1:16),Newline(1:1-1:1),
KwIf(2:1-2:3),StringStart(2:3-2:4),StringPart(2:4-2:4),StringEnd(2:4-2:5),LowerIdent(2:5-2:9),OpBinaryMinus(2:9-2:10),LowerIdent(2:10-2:11),KwElse(2:12-2:16),NoSpaceDotLowerIdent(2:16-2:18),EndOfFile(2:18-2:18),
~~~
# PARSE
~~~clojure
(e-if-then-else @1.1-2.18
	(e-string @1.3-1.5
		(e-string-part @1.4-1.4 (raw "")))
	(e-binop @1.5-1.16 (op "-")
		(e-ident @1.5-1.9 (qaul "") (raw "then"))
		(e-ident @1.10-1.11 (qaul "") (raw "p")))
	(e-if-then-else @2.1-2.18
		(e-string @2.3-2.5
			(e-string-part @2.4-2.4 (raw "")))
		(e-binop @2.5-2.16 (op "-")
			(e-ident @2.5-2.9 (qaul "") (raw "then"))
			(e-ident @2.10-2.11 (qaul "") (raw "p")))
		(e-malformed @2.16-2.18 (reason "expr_unexpected_token"))))
~~~
# FORMATTED
~~~roc
if "" then - p
	else
		if "" then - p else 
~~~
# CANONICALIZE
~~~clojure
(e-if @1.1-2.18 (id 91)
	(if-branches
		(if-branch
			(e-string @1.3-1.5
				(e-literal @1.4-1.4 (string "")))
			(e-binop @1.5-1.16 (op "sub")
				(e-runtime-error (tag "ident_not_in_scope"))
				(e-runtime-error (tag "ident_not_in_scope"))))
		(if-branch
			(e-string @2.3-2.5
				(e-literal @2.4-2.4 (string "")))
			(e-binop @2.5-2.16 (op "sub")
				(e-runtime-error (tag "ident_not_in_scope"))
				(e-runtime-error (tag "ident_not_in_scope")))))
	(if-else
		(e-runtime-error (tag "if_else_not_canonicalized"))))
~~~
# TYPES
~~~clojure
(expr (id 91) (type "Error"))
~~~
