# META
~~~ini
description=F32 literal whose correctly-rounded value overflows F32 is reported as an invalid number
type=snippet
~~~
# SOURCE
~~~roc
value : F32
value = 340282356779733661637539395458142568448
~~~
# EXPECTED
INVALID NUMBER - f32_out_of_range_integer.md:2:9:2:48
# PROBLEMS
~~~clojure
(reports
	(report
		(severity runtime_error)
		(title "Invalid Number")
		(region (start 2 9) (end 2 48))
		(headline
			(reflow "This number literal does not fit in the inferred type."))
		(document
			(source-region (file "f32_out_of_range_integer.md") (start 2 9) (end 2 48) (annotation error) (line-text "value = 340282356779733661637539395458142568448"))
			(line-break)
			(reflow "The inferred type is:")
			(line-break)
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "F32")
			(annotation-end))))
~~~
# TOKENS
~~~zig
LowerIdent,OpColon,UpperIdent,
LowerIdent,OpAssign,Int,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-mod)
	(statements
		(s-type-anno (name "value")
			(ty (name "F32")))
		(s-decl
			(p-ident (raw "value"))
			(e-int (raw "340282356779733661637539395458142568448")))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "value"))
		(e-runtime-error (tag "erroneous_value_expr"))
		(annotation
			(ty-lookup (name "F32") (builtin)))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "F32")))
	(expressions
		(expr (type "F32"))))
~~~
