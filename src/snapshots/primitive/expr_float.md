# META
~~~ini
description=A primitive
type=file
~~~
# SOURCE
~~~roc
module [foo]
foo = 12.34
~~~
~~~
# EXPECTED
UNEXPECTED TOKEN IN EXPRESSION - expr_float.md:3:1:3:3
UNEXPECTED TOKEN IN EXPRESSION - expr_float.md:3:2:3:4
UNEXPECTED TOKEN IN EXPRESSION - expr_float.md:3:3:3:4
# PROBLEMS
**UNEXPECTED TOKEN IN EXPRESSION**
The token **~~** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**expr_float.md:3:1:3:3:**
```roc
~~~
```
^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **~~** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**expr_float.md:3:2:3:4:**
```roc
~~~
```
 ^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **~** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**expr_float.md:3:3:3:4:**
```roc
~~~
```
  ^


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
KwModule(1:1-1:7),OpenSquare(1:8-1:9),LowerIdent(1:9-1:12),CloseSquare(1:12-1:13),Newline(1:1-1:1),
LowerIdent(2:1-2:4),OpAssign(2:5-2:6),Float(2:7-2:12),Newline(1:1-1:1),
MalformedUnknownToken(3:1-3:2),MalformedUnknownToken(3:2-3:3),MalformedUnknownToken(3:3-3:4),EndOfFile(3:4-3:4),
~~~
# PARSE
~~~clojure
(file @1.1-3.4
	(module @1.1-1.13
		(exposes @1.8-1.13
			(exposed-lower-ident (text "foo"))))
	(statements
		(s-decl @2.1-2.12
			(p-ident @2.1-2.4 (raw "foo"))
			(e-frac @2.7-2.12 (raw "12.34")))
		(e-malformed @3.1-3.3 (reason "expr_unexpected_token"))
		(e-malformed @3.2-3.4 (reason "expr_unexpected_token"))
		(e-malformed @3.3-3.4 (reason "expr_unexpected_token"))))
~~~
# FORMATTED
~~~roc
module [foo]
foo = 12.34

~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign @2.1-2.4 (ident "foo"))
		(e-dec-small @2.7-2.12 (numerator "1234") (denominator-power-of-ten "2") (value "12.34"))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @2.1-2.4 (type "Frac(*)")))
	(expressions
		(expr @2.7-2.12 (type "Frac(*)"))))
~~~
