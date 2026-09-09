# META
~~~ini
description=Bare underscores in nominal and opaque type headers are rejected because declaration parameters must be named.
type=snippet
~~~
# SOURCE
~~~roc
Nominal(_) := Str

Opaque(_) :: Str
~~~
# EXPECTED
UNDERSCORE IN NOMINAL TYPE - bare_underscore_in_nominal_headers.md:1:9:1:10
UNDERSCORE IN OPAQUE TYPE - bare_underscore_in_nominal_headers.md:3:8:3:9
# PROBLEMS
~~~clojure
(reports
	(report
		(severity runtime_error)
		(title "Underscore In Nominal Type")
		(region (start 1 9) (end 1 10))
		(headline
			(reflow "A bare underscore is not allowed in nominal type declarations."))
		(document
			(source-region (file "bare_underscore_in_nominal_headers.md") (start 1 9) (end 1 10) (annotation error) (line-text "Nominal(_) := Str"))
			(line-break)
			(reflow "A bare underscore in a type annotation means \"I don't care about this type\", so it does not declare a type parameter. If this parameter is intentionally phantom, give it an underscore-prefixed name like `_a` instead.")))
	(report
		(severity runtime_error)
		(title "Underscore In Opaque Type")
		(region (start 3 8) (end 3 9))
		(headline
			(reflow "A bare underscore is not allowed in opaque type declarations."))
		(document
			(source-region (file "bare_underscore_in_nominal_headers.md") (start 3 8) (end 3 9) (annotation error) (line-text "Opaque(_) :: Str"))
			(line-break)
			(reflow "A bare underscore in a type annotation means \"I don't care about this type\", so it does not declare a type parameter. If this parameter is intentionally phantom, give it an underscore-prefixed name like `_a` instead."))))
~~~
# TOKENS
~~~zig
UpperIdent,NoSpaceOpenRound,Underscore,CloseRound,OpColonEqual,UpperIdent,
UpperIdent,NoSpaceOpenRound,Underscore,CloseRound,OpDoubleColon,UpperIdent,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-mod)
	(statements
		(s-type-decl
			(header (name "Nominal")
				(args
					(_)))
			(ty (name "Str")))
		(s-type-decl
			(header (name "Opaque")
				(args
					(_)))
			(ty (name "Str")))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(s-nominal-decl
		(ty-header (name "Nominal")
			(ty-args
				(ty-malformed)))
		(ty-lookup (name "Str") (builtin)))
	(s-nominal-decl
		(ty-header (name "Opaque")
			(ty-args
				(ty-malformed)))
		(ty-lookup (name "Str") (builtin))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs)
	(type_decls
		(nominal (type "Error")
			(ty-header (name "Nominal")
				(ty-args
					(ty-malformed))))
		(nominal (type "Error")
			(ty-header (name "Opaque")
				(ty-args
					(ty-malformed)))))
	(expressions))
~~~
