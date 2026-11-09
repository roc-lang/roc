# META
~~~ini
description=F64 literal whose exponent exceeds the F64 range is reported as an invalid number rather than silently becoming inf (repro for https://github.com/roc-lang/roc/issues/12177)
type=snippet
~~~
# SOURCE
~~~roc
gravity : F64
gravity = 1e400
~~~
# EXPECTED
INVALID NUMBER - f64_out_of_range_scientific.md:2:11:2:16
# PROBLEMS
~~~clojure
(reports
	(report
		(severity runtime_error)
		(title "Invalid Number")
		(region (start 2 11) (end 2 16))
		(headline
			(reflow "This number literal does not fit in the inferred type."))
		(document
			(source-region (file "f64_out_of_range_scientific.md") (start 2 11) (end 2 16) (annotation error) (line-text "gravity = 1e400"))
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
		(s-type-anno (name "gravity")
			(ty (name "F64")))
		(s-decl
			(p-ident (raw "gravity"))
			(e-frac (raw "1e400")))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "gravity"))
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
