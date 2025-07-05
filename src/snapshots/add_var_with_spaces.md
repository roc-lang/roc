# META
~~~ini
description=Add a variable with spaces
type=file
~~~
# SOURCE
~~~roc
module [add2]

add2 = x +      2
~~~
~~~
# EXPECTED
UNEXPECTED TOKEN IN EXPRESSION - add_var_with_spaces.md:4:1:4:3
UNEXPECTED TOKEN IN EXPRESSION - add_var_with_spaces.md:4:2:4:4
UNEXPECTED TOKEN IN EXPRESSION - add_var_with_spaces.md:4:3:4:4
# PROBLEMS
**UNEXPECTED TOKEN IN EXPRESSION**
The token **~~** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**add_var_with_spaces.md:4:1:4:3:**
```roc
~~~
```
^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **~~** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**add_var_with_spaces.md:4:2:4:4:**
```roc
~~~
```
 ^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **~** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**add_var_with_spaces.md:4:3:4:4:**
```roc
~~~
```
  ^


**UNDEFINED VARIABLE**
Nothing is named `x` in this scope.
Is there an `import` or `exposing` missing up-top?

**INVALID STATEMENT**
The statement **expression** is not allowed at the top level.
Only definitions, type annotations, and imports are allowed at the top level.

**INVALID STATEMENT**
The statement **expression** is not allowed at the top level.
Only definitions, type annotations, and imports are allowed at the top level.

**INVALID STATEMENT**
The statement **expression** is not allowed at the top level.
Only definitions, type annotations, and imports are allowed at the top level.

# TOKENS
~~~zig
KwModule(1:1-1:7),OpenSquare(1:8-1:9),LowerIdent(1:9-1:13),CloseSquare(1:13-1:14),Newline(1:1-1:1),
Newline(1:1-1:1),
LowerIdent(3:1-3:5),OpAssign(3:6-3:7),LowerIdent(3:8-3:9),OpPlus(3:10-3:11),Int(3:17-3:18),Newline(1:1-1:1),
MalformedUnknownToken(4:1-4:2),MalformedUnknownToken(4:2-4:3),MalformedUnknownToken(4:3-4:4),EndOfFile(4:4-4:4),
~~~
# PARSE
~~~clojure
(file @1.1-4.4
	(module @1.1-1.14
		(exposes @1.8-1.14
			(exposed-lower-ident (text "add2"))))
	(statements
		(s-decl @3.1-4.2
			(p-ident @3.1-3.5 (raw "add2"))
			(e-binop @3.8-4.2 (op "+")
				(e-ident @3.8-3.9 (raw "x"))
				(e-int @3.17-3.18 (raw "2"))))
		(e-malformed @4.1-4.3 (reason "expr_unexpected_token"))
		(e-malformed @4.2-4.4 (reason "expr_unexpected_token"))
		(e-malformed @4.3-4.4 (reason "expr_unexpected_token"))))
~~~
# FORMATTED
~~~roc
module [add2]

add2 = x + 2

~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign @3.1-3.5 (ident "add2"))
		(e-binop @3.8-4.2 (op "add")
			(e-runtime-error (tag "ident_not_in_scope"))
			(e-int @3.17-3.18 (value "2")))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @3.1-3.5 (type "*")))
	(expressions
		(expr @3.8-4.2 (type "*"))))
~~~
