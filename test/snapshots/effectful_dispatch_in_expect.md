# META
~~~ini
description=An effectful static dispatch in an expect condition is rejected
type=snippet
~~~
# SOURCE
~~~roc
Task := [Task].{
	run! : Task => Bool
	run! = |_| Bool.True
}

expect {
	task = Task.Task
	task.run!()
}
~~~
# EXPECTED
EFFECTFUL EXPECT - effectful_dispatch_in_expect.md:6:1:9:2
# PROBLEMS
── ✗ effectful expect ────────────────────── effectful_dispatch_in_expect.md:6:1

This expect performs an effect while evaluating its condition.

expect {
    task = Task.Task
    task.run!()
}

Keep expect conditions pure, and test effectful behavior from a function body
instead.

# TOKENS
~~~zig
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,Dot,OpenCurly,
LowerIdent,OpColon,UpperIdent,OpFatArrow,UpperIdent,
LowerIdent,OpAssign,OpBar,Underscore,OpBar,UpperIdent,NoSpaceDotUpperIdent,
CloseCurly,
KwExpect,OpenCurly,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotUpperIdent,
LowerIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,CloseRound,
CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-mod)
	(statements
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
						(ty (name "Bool"))))
				(s-decl
					(p-ident (raw "run!"))
					(e-lambda
						(args
							(p-underscore))
						(e-tag (raw "Bool.True"))))))
		(s-expect
			(e-block
				(statements
					(s-decl
						(p-ident (raw "task"))
						(e-tag (raw "Task.Task")))
					(e-method-call (method ".run!")
						(receiver
							(e-ident (raw "task")))
						(args)))))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "effectful_dispatch_in_expect.Task.run!"))
		(e-lambda
			(args
				(p-underscore))
			(e-nominal-external
				(builtin)
				(e-tag (name "True"))))
		(annotation
			(ty-fn (effectful true)
				(ty-lookup (name "Task") (local))
				(ty-lookup (name "Bool") (builtin)))))
	(s-nominal-decl
		(ty-header (name "Task"))
		(ty-tag-union
			(ty-tag-name (name "Task"))))
	(s-expect
		(e-block
			(s-let
				(p-assign (ident "task"))
				(e-nominal (nominal "Task")
					(e-tag (name "Task"))))
			(e-dispatch-call (method "run!") (constraint-fn-var 248)
				(receiver
					(e-lookup-local
						(p-assign (ident "task"))))
				(args)))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Task => Bool")))
	(type_decls
		(nominal (type "Task")
			(ty-header (name "Task"))))
	(expressions
		(expr (type "Task => Bool"))))
~~~
