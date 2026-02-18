# META
~~~ini
description=the int test platform with for-clause syntax
type=file
~~~
# SOURCE
~~~roc
platform ""
		requires {
			[Model : model] for main : {
				init : model,
				update : model, I64 -> Model,
				render : model -> I64
			}
		}
    exposes []
    packages {}
    provides { main: "main" }

main : { init : Model, update : Model, I64 -> Model, render : Model -> I64 }
main = { crash "todo" }
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
KwPlatform,StringStart,StringPart,StringEnd,
KwRequires,OpenCurly,
OpenSquare,UpperIdent,OpColon,LowerIdent,CloseSquare,KwFor,LowerIdent,OpColon,OpenCurly,
LowerIdent,OpColon,LowerIdent,Comma,
LowerIdent,OpColon,LowerIdent,Comma,UpperIdent,OpArrow,UpperIdent,Comma,
LowerIdent,OpColon,LowerIdent,OpArrow,UpperIdent,
CloseCurly,
CloseCurly,
KwExposes,OpenSquare,CloseSquare,
KwPackages,OpenCurly,CloseCurly,
KwProvides,OpenCurly,LowerIdent,OpColon,StringStart,StringPart,StringEnd,CloseCurly,
LowerIdent,OpColon,OpenCurly,LowerIdent,OpColon,UpperIdent,Comma,LowerIdent,OpColon,UpperIdent,Comma,UpperIdent,OpArrow,UpperIdent,Comma,LowerIdent,OpColon,UpperIdent,OpArrow,UpperIdent,CloseCurly,
LowerIdent,OpAssign,OpenCurly,KwCrash,StringStart,StringPart,StringEnd,CloseCurly,
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
				(entrypoint "main")
				(ty-record
					(anno-record-field (name "init")
						(ty-var (raw "model")))
					(anno-record-field (name "update")
						(ty-fn
							(ty-var (raw "model"))
							(ty (name "I64"))
							(ty (name "Model"))))
					(anno-record-field (name "render")
						(ty-fn
							(ty-var (raw "model"))
							(ty (name "I64")))))))
		(exposes)
		(packages)
		(provides
			(record-field (name "main")
				(e-string
					(e-string-part (raw "main"))))))
	(statements
		(s-type-anno (name "main")
			(ty-record
				(anno-record-field (name "init")
					(ty (name "Model")))
				(anno-record-field (name "update")
					(ty-fn
						(ty (name "Model"))
						(ty (name "I64"))
						(ty (name "Model"))))
				(anno-record-field (name "render")
					(ty-fn
						(ty (name "Model"))
						(ty (name "I64"))))))
		(s-decl
			(p-ident (raw "main"))
			(e-block
				(statements
					(s-crash
						(e-string
							(e-string-part (raw "todo")))))))))
~~~
# FORMATTED
~~~roc
platform ""
	requires {
		[Model : model] for main : {
			init : model,
			update : model, I64 -> Model,
			render : model -> I64,
		}
	}
	exposes []
	packages {}
	provides { main: "main" }

main : { init : Model, update : Model, I64 -> Model, render : Model -> I64 }
main = {
	crash "todo"
}
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "main"))
		(e-block
			(e-crash (msg "todo")))
		(annotation
			(ty-record
				(field (field "init")
					(ty-lookup (name "Model") (local)))
				(field (field "update")
					(ty-fn (effectful false)
						(ty-lookup (name "Model") (local))
						(ty-lookup (name "I64") (builtin))
						(ty-lookup (name "Model") (local))))
				(field (field "render")
					(ty-fn (effectful false)
						(ty-lookup (name "Model") (local))
						(ty-lookup (name "I64") (builtin))))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "{ init: model, render: model -> I64, update: model, I64 -> model }")))
	(expressions
		(expr (type "{ init: model, render: model -> I64, update: model, I64 -> model }"))))
~~~
