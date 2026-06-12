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
TYPE MISMATCH - string_interpolation_type_mismatch.md:1:1:1:1
# PROBLEMS
**TYPE MISMATCH**
The `from_interpolation` method on `Str` has an incompatible type:
**string_interpolation_type_mismatch.md:1:1:1:1:**
```roc
x : U8
```
^

The method `from_interpolation` has the type:

    Str, Iter((Str, Str)) -> Str

But I need it to have the type:

    Str, Iter((U8, Str)) -> Str

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
		(e-block
			(s-let
				(p-assign (ident "#interp_0"))
				(e-lookup-local
					(p-assign (ident "x"))))
			(e-dispatch-call (method "from_interpolation") (constraint-fn-var 258)
				(receiver
					(e-string
						(e-literal (string "value: "))))
				(args
					(e-dispatch-call (method "prepended") (constraint-fn-var 216)
						(receiver
							(e-dispatch-call (method "iter") (constraint-fn-var 152)
								(receiver
									(e-empty_list))
								(args)))
						(args
							(e-tuple
								(elems
									(e-lookup-local
										(p-assign (ident "#interp_0")))
									(e-string
										(e-literal (string ""))))))))))))
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
