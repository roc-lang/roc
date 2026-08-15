# META
~~~ini
description=A where alias naming an effectful method is applied outside any expect, so discharging it reports nothing (issue 10661)
type=snippet
~~~
# SOURCE
~~~roc
a.Runner : where [a.run! : a => {}]

Task := [Task].{
	run! : Task => {}
	run! = |_| {}
}

go! : a => {} where [a.Runner]
go! = |value| value.run!()

main! = |_| go!(Task.Task)
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
LowerIdent,NoSpaceDotUpperIdent,OpColon,KwWhere,OpenSquare,LowerIdent,NoSpaceDotLowerIdent,OpColon,LowerIdent,OpFatArrow,OpenCurly,CloseCurly,CloseSquare,
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,Dot,OpenCurly,
LowerIdent,OpColon,UpperIdent,OpFatArrow,OpenCurly,CloseCurly,
LowerIdent,OpAssign,OpBar,Underscore,OpBar,OpenCurly,CloseCurly,
CloseCurly,
LowerIdent,OpColon,LowerIdent,OpFatArrow,OpenCurly,CloseCurly,KwWhere,OpenSquare,LowerIdent,NoSpaceDotUpperIdent,CloseSquare,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,LowerIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,CloseRound,
LowerIdent,OpAssign,OpBar,Underscore,OpBar,LowerIdent,NoSpaceOpenRound,UpperIdent,NoSpaceDotUpperIdent,CloseRound,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-mod)
	(statements
		(s-type-decl
			(header (name ".Runner")
				(args))
			(ty-var (raw "a"))
			(where
				(method (mod-of "a") (name "run!") (effectful true)
					(args
						(ty-var (raw "a")))
					(ty-record))))
		(s-type-decl
			(header (name "Task")
				(args))
			(ty-tag-union
				(tags
					(ty (name "Task"))))
			(associated
				(s-type-anno (name "run!")
					(ty-fn
						(ty (name "Task"))
						(ty-record)))
				(s-decl
					(p-ident (raw "run!"))
					(e-lambda
						(args
							(p-underscore))
						(e-record)))))
		(s-type-anno (name "go!")
			(ty-fn
				(ty-var (raw "a"))
				(ty-record))
			(where
				(alias (mod-of "a")
					(ty (name "Runner")))))
		(s-decl
			(p-ident (raw "go!"))
			(e-lambda
				(args
					(p-ident (raw "value")))
				(e-method-call (method ".run!")
					(receiver
						(e-ident (raw "value")))
					(args))))
		(s-decl
			(p-ident (raw "main!"))
			(e-lambda
				(args
					(p-underscore))
				(e-apply
					(e-ident (raw "go!"))
					(e-tag (raw "Task.Task")))))))
~~~
# FORMATTED
~~~roc
a.Runner :  where [a.run! : a => {}]

Task := [Task].{
	run! : Task => {}
	run! = |_| {}
}

go! : a => {} where [a.Runner]
go! = |value| value.run!()

main! = |_| go!(Task.Task)
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "echo!"))
		(e-hosted-lambda (symbol "echo!")
			(args
				(p-assign (ident "_echo_arg"))))
		(annotation
			(ty-fn (effectful true)
				(ty-lookup (name "Str") (builtin))
				(ty-record))))
	(d-let
		(p-assign (ident "where_alias_effectful_method_issue_10661.Task.run!"))
		(e-lambda
			(args
				(p-underscore))
			(e-empty_record))
		(annotation
			(ty-fn (effectful true)
				(ty-lookup (name "Task") (local))
				(ty-record))))
	(d-let
		(p-assign (ident "go!"))
		(e-lambda
			(args
				(p-assign (ident "value")))
			(e-dispatch-call (method "run!") (constraint-fn-var 302)
				(receiver
					(e-lookup-local
						(p-assign (ident "value"))))
				(args)))
		(annotation
			(ty-fn (effectful true)
				(ty-rigid-var (name "a"))
				(ty-record))
			(where
				(alias
					(ty-rigid-var-lookup (ty-rigid-var (name "a")))
					(ty-lookup (name "Runner") (local))))))
	(d-let
		(p-assign (ident "main!"))
		(e-lambda
			(args
				(p-underscore))
			(e-call (constraint-fn-var 313)
				(e-lookup-local
					(p-assign (ident "go!")))
				(e-nominal (nominal "Task")
					(e-tag (name "Task"))))))
	(s-where-alias-decl
		(ty-header (name "Runner"))
		(ty-rigid-var (name "a"))
		(where
			(method (ty-rigid-var-lookup (ty-rigid-var (name "a"))) (name "run!") (effectful true)
				(args
					(ty-rigid-var-lookup (ty-rigid-var (name "a"))))
				(ty-record))))
	(s-nominal-decl
		(ty-header (name "Task"))
		(ty-tag-union
			(ty-tag-name (name "Task")))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Str => {}"))
		(patt (type "Task => {}"))
		(patt (type "a => {} where [a.run! : a => {}]"))
		(patt (type "_arg => {}")))
	(type_decls
		(where-alias (type "a where [a.run! : a => {}]")
			(ty-header (name "Runner")))
		(nominal (type "Task")
			(ty-header (name "Task"))))
	(expressions
		(expr (type "Str => {}"))
		(expr (type "Task => {}"))
		(expr (type "a => {} where [a.run! : a => {}]"))
		(expr (type "_arg => {}"))))
~~~
