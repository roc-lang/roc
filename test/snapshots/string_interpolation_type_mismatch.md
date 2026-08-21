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
~~~clojure
(reports
	(report
		(severity runtime_error)
		(title "Type Mismatch")
		(region (start 4 15) (end 4 16))
		(headline
			(reflow "This expression is used in an unexpected way."))
		(document
			(source-region (file "string_interpolation_type_mismatch.md") (start 4 15) (end 4 16) (annotation error) (line-text "y = \"value: ${x}\""))
			(line-break)
			(reflow "It has the type:")
			(line-break)
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "U8")
			(annotation-end)
			(line-break)
			(line-break)
			(reflow "But you are trying to use it as:")
			(line-break)
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "Str")
			(annotation-end))))
~~~
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
	(type-mod)
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
			(e-runtime-error (tag "erroneous_value_expr")))))
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
