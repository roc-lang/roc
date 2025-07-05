# META
~~~ini
description=
type=file
~~~
# SOURCE
~~~roc
module []

foo = if tru then 0
~~~
~~~
# EXPECTED
PARSE ERROR - expr_if_missing_else.md:3:19:3:19
UNEXPECTED TOKEN IN EXPRESSION - expr_if_missing_else.md:4:1:4:3
UNEXPECTED TOKEN IN EXPRESSION - expr_if_missing_else.md:4:2:4:4
UNEXPECTED TOKEN IN EXPRESSION - expr_if_missing_else.md:4:3:4:4
# PROBLEMS
**PARSE ERROR**
A parsing error occurred: `no_else`
This is an unexpected parsing error. Please check your syntax.

Here is the problematic code:
**expr_if_missing_else.md:3:19:3:19:**
```roc
foo = if tru then 0
```
                  


**UNEXPECTED TOKEN IN EXPRESSION**
The token **~~** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**expr_if_missing_else.md:4:1:4:3:**
```roc
~~~
```
^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **~~** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**expr_if_missing_else.md:4:2:4:4:**
```roc
~~~
```
 ^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **~** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**expr_if_missing_else.md:4:3:4:4:**
```roc
~~~
```
  ^


**UNKNOWN OPERATOR**
This looks like an operator, but it's not one I recognize!
Check the spelling and make sure you're using a valid Roc operator.

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
KwModule(1:1-1:7),OpenSquare(1:8-1:9),CloseSquare(1:9-1:10),Newline(1:1-1:1),
Newline(1:1-1:1),
LowerIdent(3:1-3:4),OpAssign(3:5-3:6),KwIf(3:7-3:9),LowerIdent(3:10-3:13),LowerIdent(3:14-3:18),Int(3:19-3:20),Newline(1:1-1:1),
MalformedUnknownToken(4:1-4:2),MalformedUnknownToken(4:2-4:3),MalformedUnknownToken(4:3-4:4),EndOfFile(4:4-4:4),
~~~
# PARSE
~~~clojure
(file @1.1-4.4
	(module @1.1-1.10
		(exposes @1.8-1.10))
	(statements
		(s-decl @1.1-1.1
			(p-ident @3.1-3.4 (raw "foo"))
			(e-malformed @1.1-1.1 (reason "no_else")))
		(e-malformed @4.1-4.3 (reason "expr_unexpected_token"))
		(e-malformed @4.2-4.4 (reason "expr_unexpected_token"))
		(e-malformed @4.3-4.4 (reason "expr_unexpected_token"))))
~~~
# FORMATTED
~~~roc
module []

foo = 

~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign @3.1-3.4 (ident "foo"))
		(e-runtime-error (tag "expr_not_canonicalized"))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @3.1-3.4 (type "Error")))
	(expressions
		(expr @1.1-1.1 (type "Error"))))
~~~
