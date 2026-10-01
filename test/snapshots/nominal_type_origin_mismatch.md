# META
~~~ini
description=Type mismatch showing nominal type origin from different mod
type=snippet
~~~
# SOURCE
~~~roc
import Data exposing [Person]

expectsPerson : Person -> Str
expectsPerson = |p| "Got a person"

main =
    # This will cause a type mismatch
    expectsPerson("not a person")
~~~
# EXPECTED
UNUSED VARIABLE - nominal_type_origin_mismatch.md:4:18:4:19
MOD NOT FOUND - nominal_type_origin_mismatch.md:1:1:1:30
MOD NOT FOUND - nominal_type_origin_mismatch.md:3:17:3:23
# PROBLEMS
~~~clojure
(reports
	(report
		(severity warning)
		(title "Unused Variable")
		(region (start 4 18) (end 4 19))
		(headline
			(reflow "Variable ")
			(annotated symbol-unqualified "p")
			(reflow " is defined here and then never used:"))
		(document
			(reflow "If you don't need this variable, prefix it with an underscore like ")
			(annotated symbol-unqualified "_p")
			(reflow " to suppress this warning.")
			(line-break)
			(source-region (file "nominal_type_origin_mismatch.md") (start 4 18) (end 4 19) (annotation error) (line-text "expectsPerson = |p| \"Got a person\""))))
	(report
		(severity runtime_error)
		(title "Mod Not Found")
		(region (start 1 1) (end 1 30))
		(headline
			(text "The mod ")
			(annotated code "Data")
			(reflow " was not found in this Roc project."))
		(document
			(source-region (file "nominal_type_origin_mismatch.md") (start 1 1) (end 1 30) (annotation error) (line-text "import Data exposing [Person]"))))
	(report
		(severity runtime_error)
		(title "Mod Not Found")
		(region (start 3 17) (end 3 23))
		(headline
			(text "This ")
			(annotated code "Person")
			(reflow " type is declared to be in ")
			(annotated code "Data")
			(reflow ", which does not exist."))
		(document
			(source-region (file "nominal_type_origin_mismatch.md") (start 3 17) (end 3 23) (annotation error) (line-text "expectsPerson : Person -> Str")))))
~~~
# TOKENS
~~~zig
KwImport,UpperIdent,KwExposing,OpenSquare,UpperIdent,CloseSquare,
LowerIdent,OpColon,UpperIdent,OpArrow,UpperIdent,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,StringStart,StringPart,StringEnd,
LowerIdent,OpAssign,
LowerIdent,NoSpaceOpenRound,StringStart,StringPart,StringEnd,CloseRound,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-mod)
	(statements
		(s-import (raw "Data")
			(exposing
				(exposed-upper-ident (text "Person"))))
		(s-type-anno (name "expectsPerson")
			(ty-fn
				(ty (name "Person"))
				(ty (name "Str"))))
		(s-decl
			(p-ident (raw "expectsPerson"))
			(e-lambda
				(args
					(p-ident (raw "p")))
				(e-string
					(e-string-part (raw "Got a person")))))
		(s-decl
			(p-ident (raw "main"))
			(e-apply
				(e-ident (raw "expectsPerson"))
				(e-string
					(e-string-part (raw "not a person")))))))
~~~
# FORMATTED
~~~roc
import Data exposing [Person]

expectsPerson : Person -> Str
expectsPerson = |p| "Got a person"

main =
# This will cause a type mismatch
	expectsPerson("not a person")
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "expectsPerson"))
		(e-runtime-error (tag "erroneous_value_expr"))
		(annotation
			(ty-fn (effectful false)
				(ty-malformed)
				(ty-lookup (name "Str") (builtin)))))
	(d-let
		(p-assign (ident "main"))
		(e-call (constraint-fn-var 253)
			(e-runtime-error (tag "erroneous_value_expr"))
			(e-string
				(e-literal (string "not a person")))))
	(s-import (mod "Data")
		(exposes
			(exposed (name "Person") (wildcard false)))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Error -> Str"))
		(patt (type "Str")))
	(expressions
		(expr (type "Error -> Str"))
		(expr (type "Str"))))
~~~
