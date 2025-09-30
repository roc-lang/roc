# META
~~~ini
description=Float literal type inference
type=file
~~~
# SOURCE
~~~roc
x = 3.14
y = 1.23e45
z = 0.5
~~~
# EXPECTED
MISSING MAIN! FUNCTION - can_frac_literal.md:1:1:3:8
# PROBLEMS
**MISSING MAIN! FUNCTION**
Default app modules must have a `main!` function.

No `main!` function was found.

Add a main! function like:
`main! = |arg| { ... }`
**can_frac_literal.md:1:1:3:8:**
```roc
x = 3.14
y = 1.23e45
z = 0.5
```


# TOKENS
~~~zig
LowerIdent(1:1-1:2),OpAssign(1:3-1:4),Float(1:5-1:9),
LowerIdent(2:1-2:2),OpAssign(2:3-2:4),Float(2:5-2:12),
LowerIdent(3:1-3:2),OpAssign(3:3-3:4),Float(3:5-3:8),
EndOfFile(4:1-4:1),
~~~
# PARSE
~~~clojure
(file @1.1-3.8
	(type-module @1.1-1.2)
	(statements
		(s-decl @1.1-1.9
			(p-ident @1.1-1.2 (raw "x"))
			(e-frac @1.5-1.9 (raw "3.14")))
		(s-decl @2.1-2.12
			(p-ident @2.1-2.2 (raw "y"))
			(e-frac @2.5-2.12 (raw "1.23e45")))
		(s-decl @3.1-3.8
			(p-ident @3.1-3.2 (raw "z"))
			(e-frac @3.5-3.8 (raw "0.5")))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign @1.1-1.2 (ident "x"))
		(e-dec-small @1.5-1.9 (numerator "314") (denominator-power-of-ten "2") (value "3.14")))
	(d-let
		(p-assign @2.1-2.2 (ident "y"))
		(e-frac-f64 @2.5-2.12 (value "1.23e45")))
	(d-let
		(p-assign @3.1-3.2 (ident "z"))
		(e-dec-small @3.5-3.8 (numerator "5") (denominator-power-of-ten "1") (value "0.5"))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @1.1-1.2 (type "Frac(_size)"))
		(patt @2.1-2.2 (type "Frac(_size)"))
		(patt @3.1-3.2 (type "Frac(_size)")))
	(expressions
		(expr @1.5-1.9 (type "Frac(_size)"))
		(expr @2.5-2.12 (type "Frac(_size)"))
		(expr @3.5-3.8 (type "Frac(_size)"))))
~~~
