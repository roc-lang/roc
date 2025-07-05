# META
~~~ini
description=Float literal type inference
type=file
~~~
# SOURCE
~~~roc
module []

x = 3.14
y = 1.23e45
z = 0.5
~~~
~~~
# EXPECTED
UNEXPECTED TOKEN IN EXPRESSION - can_frac_literal.md:6:1:6:3
UNEXPECTED TOKEN IN EXPRESSION - can_frac_literal.md:6:2:6:4
UNEXPECTED TOKEN IN EXPRESSION - can_frac_literal.md:6:3:6:4
# PROBLEMS
**UNEXPECTED TOKEN IN EXPRESSION**
The token **~~** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**can_frac_literal.md:6:1:6:3:**
```roc
~~~
```
^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **~~** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**can_frac_literal.md:6:2:6:4:**
```roc
~~~
```
 ^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **~** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**can_frac_literal.md:6:3:6:4:**
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
KwModule(1:1-1:7),OpenSquare(1:8-1:9),CloseSquare(1:9-1:10),Newline(1:1-1:1),
Newline(1:1-1:1),
LowerIdent(3:1-3:2),OpAssign(3:3-3:4),Float(3:5-3:9),Newline(1:1-1:1),
LowerIdent(4:1-4:2),OpAssign(4:3-4:4),Float(4:5-4:12),Newline(1:1-1:1),
LowerIdent(5:1-5:2),OpAssign(5:3-5:4),Float(5:5-5:8),Newline(1:1-1:1),
MalformedUnknownToken(6:1-6:2),MalformedUnknownToken(6:2-6:3),MalformedUnknownToken(6:3-6:4),EndOfFile(6:4-6:4),
~~~
# PARSE
~~~clojure
(file @1.1-6.4
	(module @1.1-1.10
		(exposes @1.8-1.10))
	(statements
		(s-decl @3.1-3.9
			(p-ident @3.1-3.2 (raw "x"))
			(e-frac @3.5-3.9 (raw "3.14")))
		(s-decl @4.1-4.12
			(p-ident @4.1-4.2 (raw "y"))
			(e-frac @4.5-4.12 (raw "1.23e45")))
		(s-decl @5.1-5.8
			(p-ident @5.1-5.2 (raw "z"))
			(e-frac @5.5-5.8 (raw "0.5")))
		(e-malformed @6.1-6.3 (reason "expr_unexpected_token"))
		(e-malformed @6.2-6.4 (reason "expr_unexpected_token"))
		(e-malformed @6.3-6.4 (reason "expr_unexpected_token"))))
~~~
# FORMATTED
~~~roc
module []

x = 3.14
y = 1.23e45
z = 0.5

~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign @3.1-3.2 (ident "x"))
		(e-dec-small @3.5-3.9 (numerator "314") (denominator-power-of-ten "2") (value "3.14")))
	(d-let
		(p-assign @4.1-4.2 (ident "y"))
		(e-frac-f64 @4.5-4.12 (value "1.23e45")))
	(d-let
		(p-assign @5.1-5.2 (ident "z"))
		(e-dec-small @5.5-5.8 (numerator "5") (denominator-power-of-ten "1") (value "0.5"))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @3.1-3.2 (type "Frac(*)"))
		(patt @4.1-4.2 (type "Frac(*)"))
		(patt @5.1-5.2 (type "Frac(*)")))
	(expressions
		(expr @3.5-3.9 (type "Frac(*)"))
		(expr @4.5-4.12 (type "Frac(*)"))
		(expr @5.5-5.8 (type "Frac(*)"))))
~~~
