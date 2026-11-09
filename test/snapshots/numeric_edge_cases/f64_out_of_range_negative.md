# META
~~~ini
description=F64 literal whose correctly-rounded value overflows F64 is reported as an invalid number
type=snippet
~~~
# SOURCE
~~~roc
value : F64
value = -1e400
~~~
# EXPECTED
INVALID NUMBER - f64_out_of_range_negative.md:2:9:2:15
# PROBLEMS
~~~clojure
(reports
	(report
		(severity runtime_error)
		(title "Invalid Number")
		(region (start 2 9) (end 2 15))
		(headline
			(reflow "This number literal does not fit in the inferred type."))
		(document
			(source-region (file "f64_out_of_range_negative.md") (start 2 9) (end 2 15) (annotation error) (line-text "value = -1e400"))
			(line-break)
			(reflow "The inferred type is:")
			(line-break)
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "F64")
			(annotation-end))))
~~~
# TOKENS
~~~zig
LowerIdent,OpColon,UpperIdent,
LowerIdent,OpAssign,Float,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-mod)
	(statements
		(s-type-anno (name "value")
			(ty (name "F64")))
		(s-decl
			(p-ident (raw "value"))
			(e-frac (raw "-1e400")))))
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
			(ty-lookup (name "F64") (builtin)))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "F64")))
	(expressions
		(expr (type "F64"))))
~~~
