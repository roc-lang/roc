# META
~~~ini
description=string interpolation with non-Str type should fail
type=snippet
~~~
# SOURCE
~~~roc
x : U8
x = 42

y = "value: ${x}"
~~~
# EXPECTED
TYPE MISMATCH - string_interpolation_type_mismatch.md:4:15:4:16
# PROBLEMS
**TYPE MISMATCH**
This expression is used in an unexpected way:
**string_interpolation_type_mismatch.md:4:15:4:16:**
```roc
y = "value: ${x}"
```
              ^

It has the type:

    U8

But I expected it to be:

    Str

# TOKENS
~~~zig
LowerIdent,OpColon,UpperIdent,
LowerIdent,OpAssign,Int,
LowerIdent,OpAssign,StringStart,StringPart,OpenStringInterpolation,LowerIdent,CloseStringInterpolation,StringPart,StringEnd,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-module)
	(statements
		(s-type-anno (name "x")
			(ty (name "U8")))
		(s-decl
			(p-ident (raw "x"))
			(e-int (raw "42")))
		(s-decl
			(p-ident (raw "y"))
			(e-string
				(e-string-part (raw "value: "))
				(e-ident (raw "x"))
				(e-string-part (raw ""))))))
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
		(e-num (value "42"))
		(annotation
			(ty-lookup (name "U8") (builtin))))
	(d-let
		(p-assign (ident "y"))
		(e-string
			(e-literal (string "value: "))
			(e-lookup-local
				(p-assign (ident "x")))
			(e-literal (string "")))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "U8"))
		(patt (type "Error")))
	(expressions
		(expr (type "U8"))
		(expr (type "Error"))))
~~~
