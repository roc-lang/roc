# META
~~~ini
description=A provided function must use the abstract alias rather than a fresh type variable
type=file
~~~
# SOURCE
~~~roc
platform ""
    requires { [Model : model] for program : { init : {} -> Box(model) } }
    exposes []
    packages {}
    provides { "roc_init": init_for_host, "roc_release": release_for_host }

init_for_host = program.init
release_for_host : Box(model) -> {}
release_for_host = |_model| {}
~~~
# EXPECTED
POLYMORPHIC PROVIDED FUNCTION - provides_type_variable_issue_13113.md:9:1:9:17
# PROBLEMS
~~~clojure
(reports
	(report
		(severity runtime_error)
		(title "Polymorphic Provided Function")
		(region (start 9 1) (end 9 17))
		(headline
			(reflow "A function provided to the host cannot have type variables."))
		(document
			(source-region (file "provides_type_variable_issue_13113.md") (start 9 1) (end 9 17) (annotation error) (line-text "release_for_host = |_model| {}"))
			(line-break)
			(line-break)
			(reflow "Its type is:")
			(line-break)
			(annotated code-block "Box(model) -> {}")
			(line-break)
			(line-break)
			(reflow "The host cannot instantiate a polymorphic Roc function. Give this entrypoint a monomorphic type; platform abstract aliases are allowed.")
			(line-break)
			(line-break)
			(reflow "Use")
			(reflow " ")
			(annotated code "Model")
			(reflow " ")
			(reflow "instead of")
			(reflow " ")
			(annotated code "model")
			(reflow " ")
			(reflow "here. The lowercase variable in this function's type is independent of the platform requirement's binder."))))
~~~
# TOKENS
~~~zig
KwPlatform,StringStart,StringPart,StringEnd,
KwRequires,OpenCurly,OpenSquare,UpperIdent,OpColon,LowerIdent,CloseSquare,KwFor,LowerIdent,OpColon,OpenCurly,LowerIdent,OpColon,OpenCurly,CloseCurly,OpArrow,UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,CloseCurly,CloseCurly,
KwExposes,OpenSquare,CloseSquare,
KwPackages,OpenCurly,CloseCurly,
KwProvides,OpenCurly,StringStart,StringPart,StringEnd,OpColon,LowerIdent,Comma,StringStart,StringPart,StringEnd,OpColon,LowerIdent,CloseCurly,
LowerIdent,OpAssign,LowerIdent,NoSpaceDotLowerIdent,
LowerIdent,OpColon,UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,OpArrow,OpenCurly,CloseCurly,
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
								(ty-var (raw "model"))))))))
		(exposes)
		(packages)
		(provides
			(symbol-map-entry (symbol "roc_init") (func "init_for_host"))
			(symbol-map-entry (symbol "roc_release") (func "release_for_host"))))
	(statements
		(s-decl
			(p-ident (raw "init_for_host"))
			(e-field-access
				(receiver
					(e-ident (raw "program")))
				(segment (mode "required") (field "init"))))
		(s-type-anno (name "release_for_host")
			(ty-fn
				(ty-apply
					(ty (name "Box"))
					(ty-var (raw "model")))
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
		[Model : model] for program : { init : {} -> Box(model) }
	}
	exposes []
	packages {}
	provides { "roc_init": init_for_host, "roc_release": release_for_host }

init_for_host = program.init

release_for_host : Box(model) -> {}
release_for_host = |_model| {}
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "init_for_host"))
		(e-field-access
			(receiver
				(e-lookup-required
					(required-ident "program")))
			(segments
				(segment (name "init") (mode "required")))))
	(d-let
		(p-assign (ident "release_for_host"))
		(e-runtime-error (tag "erroneous_value_expr"))
		(annotation
			(ty-fn (effectful false)
				(ty-apply (name "Box") (builtin)
					(ty-rigid-var (name "model")))
				(ty-record)))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "{} -> Box(model)"))
		(patt (type "Error")))
	(expressions
		(expr (type "{} -> Box(model)"))
		(expr (type "Error"))))
~~~
