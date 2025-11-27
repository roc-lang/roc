# META
~~~ini
description=Dec type annotation
type=snippet
~~~
# SOURCE
~~~roc
x : Dec
x = 123.456
~~~
# EXPECTED
TYPE MISMATCH - dec_annotation.md:2:5:2:12
TYPE MISMATCH - dec_annotation.md:2:5:2:12
# PROBLEMS
**TYPE MISMATCH**
This expression is used in an unexpected way:
**dec_annotation.md:2:5:2:12:**
```roc
x = 123.456
```
    ^^^^^^^

It has the type:
    _Numeral_

But I expected it to be:
    _Num.Numeral_

**TYPE MISMATCH**
This expression is used in an unexpected way:
**dec_annotation.md:2:5:2:12:**
```roc
x = 123.456
```
    ^^^^^^^

It has the type:
    _Try(Dec, [InvalidNumeral(Str)])_

But I expected it to be:
    _Try(Dec, [InvalidNumeral(Str)])_

# TOKENS
~~~zig
LowerIdent,OpColon,UpperIdent,
LowerIdent,OpAssign,Float,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-module)
	(statements
		(s-type-anno (name "x")
			(ty (name "Dec")))
		(s-decl
			(p-ident (raw "x"))
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
		(p-assign (ident "x"))
		(e-frac-dec (value "123.456"))
		(annotation
			(ty-lookup (name "Dec") (builtin)))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Error")))
	(expressions
		(expr (type "Error"))))
~~~
