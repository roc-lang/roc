# META
~~~ini
description=A host cannot instantiate an unannotated polymorphic entrypoint
type=file
~~~
# SOURCE
~~~roc
platform ""
    requires {}
    exposes []
    packages {}
    provides { "roc_release": release_for_host }

release_for_host = |_model| {}
~~~
# EXPECTED
POLYMORPHIC PROVIDED FUNCTION - provides_inferred_polymorphism_issue_13113.md:7:1:7:17
# PROBLEMS
~~~clojure
(reports
	(report
		(severity runtime_error)
		(title "Polymorphic Provided Function")
		(region (start 7 1) (end 7 17))
		(headline
			(reflow "A function provided to the host cannot have type variables."))
		(document
			(source-region (file "provides_inferred_polymorphism_issue_13113.md") (start 7 1) (end 7 17) (annotation error) (line-text "release_for_host = |_model| {}"))
			(line-break)
			(line-break)
			(reflow "Its type is:")
			(line-break)
			(annotated code-block "_arg -> {}")
			(line-break)
			(line-break)
			(reflow "The host cannot instantiate a polymorphic Roc function. Give this entrypoint a monomorphic type; platform abstract aliases are allowed."))))
~~~
# TOKENS
~~~zig
KwPlatform,StringStart,StringPart,StringEnd,
KwRequires,OpenCurly,CloseCurly,
KwExposes,OpenSquare,CloseSquare,
KwPackages,OpenCurly,CloseCurly,
KwProvides,OpenCurly,StringStart,StringPart,StringEnd,OpColon,LowerIdent,CloseCurly,
LowerIdent,OpAssign,OpBar,NamedUnderscore,OpBar,OpenCurly,CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(platform (name "")
		(requires)
		(exposes)
		(packages)
		(provides
			(symbol-map-entry (symbol "roc_release") (func "release_for_host"))))
	(statements
		(s-decl
			(p-ident (raw "release_for_host"))
			(e-lambda
				(args
					(p-ident (raw "_model")))
				(e-record)))))
~~~
# FORMATTED
~~~roc
platform ""
	requires {}
	exposes []
	packages {}
	provides { "roc_release": release_for_host }

release_for_host = |_model| {}
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "release_for_host"))
		(e-runtime-error (tag "erroneous_value_expr"))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Error")))
	(expressions
		(expr (type "Error"))))
~~~
