# META
~~~ini
description=All fractional type annotations
type=snippet
~~~
# SOURCE
~~~roc
a : F32
a = 3.14

b : F64
b = 2.71828

c : Dec
c = 123.456
~~~
# EXPECTED
NIL
# PROBLEMS
**TYPE MISMATCH**
This expression is used in an unexpected way:
**all_frac_types.md:2:5:2:9:**
```roc
a = 3.14
```
    ^^^^

It has the type:
    __size_

But the type annotation says it should have the type:
    _F32_

**TYPE MISMATCH**
This expression is used in an unexpected way:
**all_frac_types.md:5:5:5:12:**
```roc
b = 2.71828
```
    ^^^^^^^

It has the type:
    __size_

But the type annotation says it should have the type:
    _F64_

**TYPE MISMATCH**
This expression is used in an unexpected way:
**all_frac_types.md:8:5:8:12:**
```roc
c = 123.456
```
    ^^^^^^^

It has the type:
    __size_

But the type annotation says it should have the type:
    _Dec_

# TOKENS
~~~zig
LowerIdent,OpColon,UpperIdent,
LowerIdent,OpAssign,Float,
LowerIdent,OpColon,UpperIdent,
LowerIdent,OpAssign,Float,
LowerIdent,OpColon,UpperIdent,
LowerIdent,OpAssign,Float,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-module)
	(statements
		(s-type-anno (name "a")
			(ty (name "F32")))
		(s-decl
			(p-ident (raw "a"))
			(e-frac (raw "3.14")))
		(s-type-anno (name "b")
			(ty (name "F64")))
		(s-decl
			(p-ident (raw "b"))
			(e-frac (raw "2.71828")))
		(s-type-anno (name "c")
			(ty (name "Dec")))
		(s-decl
			(p-ident (raw "c"))
			(e-frac (raw "123.456")))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "a"))
		(e-dec-small (numerator "314") (denominator-power-of-ten "2") (value "3.14"))
		(annotation
			(ty-lookup (name "F32") (builtin))))
	(d-let
		(p-assign (ident "b"))
		(e-frac-dec (value "2.71828"))
		(annotation
			(ty-lookup (name "F64") (builtin))))
	(d-let
		(p-assign (ident "c"))
		(e-frac-dec (value "123.456"))
		(annotation
			(ty-lookup (name "Dec") (builtin)))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Error"))
		(patt (type "Error"))
		(patt (type "Error")))
	(expressions
		(expr (type "Error"))
		(expr (type "Error"))
		(expr (type "Error"))))
~~~
