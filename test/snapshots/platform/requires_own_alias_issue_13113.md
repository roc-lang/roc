# META
~~~ini
description=A requirement must use its lowercase binder, not the abstract alias it introduces
type=file
~~~
# SOURCE
~~~roc
platform ""
    requires { [Model : model] for program : { init : {} -> Box(Model) } }
    exposes []
    packages {}
    provides { "roc_release": release_for_host }

release_for_host : Box(Model) -> {}
release_for_host = |_model| {}
~~~
# EXPECTED
UNDECLARED TYPE - requires_own_alias_issue_13113.md:2:65:2:70
# PROBLEMS
~~~clojure
(reports
	(report
		(severity runtime_error)
		(title "Undeclared Type")
		(region (start 2 65) (end 2 70))
		(headline
			(reflow "The type ")
			(annotated code "Model")
			(reflow " is not declared in this scope."))
		(document
			(source-region (file "requires_own_alias_issue_13113.md") (start 2 65) (end 2 70) (annotation error) (line-text "    requires { [Model : model] for program : { init : {} -> Box(Model) } }"))
			(line-break)
			(line-break)
			(reflow "Use the type variable ")
			(annotated code "model")
			(reflow " in the requirement that introduces it. The alias ")
			(annotated code "Model")
			(reflow " is available outside this signature, including in functions provided to the host."))))
~~~
# TOKENS
~~~zig
KwPlatform,StringStart,StringPart,StringEnd,
KwRequires,OpenCurly,OpenSquare,UpperIdent,OpColon,LowerIdent,CloseSquare,KwFor,LowerIdent,OpColon,OpenCurly,LowerIdent,OpColon,OpenCurly,CloseCurly,OpArrow,UpperIdent,NoSpaceOpenRound,UpperIdent,CloseRound,CloseCurly,CloseCurly,
KwExposes,OpenSquare,CloseSquare,
KwPackages,OpenCurly,CloseCurly,
KwProvides,OpenCurly,StringStart,StringPart,StringEnd,OpColon,LowerIdent,CloseCurly,
LowerIdent,OpColon,UpperIdent,NoSpaceOpenRound,UpperIdent,CloseRound,OpArrow,OpenCurly,CloseCurly,
LowerIdent,OpAssign,OpBar,NamedUnderscore,OpBar,OpenCurly,CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(platform (name "")
		(requires
			(requires-entry
				(type-aliases
					(alias (name "Model") (rigid "model")))
				(entrypoint "program")
				(ty-record
					(anno-record-field (name "init")
						(ty-fn
							(ty-record)
							(ty-apply
								(ty (name "Box"))
								(ty (name "Model"))))))))
		(exposes)
		(packages)
		(provides
			(symbol-map-entry (symbol "roc_release") (func "release_for_host"))))
	(statements
		(s-type-anno (name "release_for_host")
			(ty-fn
				(ty-apply
					(ty (name "Box"))
					(ty (name "Model")))
				(ty-record)))
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
	requires {
		[Model : model] for program : { init : {} -> Box(Model) }
	}
	exposes []
	packages {}
	provides { "roc_release": release_for_host }

release_for_host : Box(Model) -> {}
release_for_host = |_model| {}
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "release_for_host"))
		(e-lambda
			(args
				(p-assign (ident "_model")))
			(e-empty_record))
		(annotation
			(ty-fn (effectful false)
				(ty-apply (name "Box") (builtin)
					(ty-lookup (name "Model") (local)))
				(ty-record)))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Box(Model) -> {}")))
	(expressions
		(expr (type "Box(Model) -> {}"))))
~~~
