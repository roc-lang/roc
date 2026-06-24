# META
~~~ini
description=A constructor wrapping an expansive call is generalized when annotated (expansiveness no longer blocks generalization), usable at two concrete types
type=file
~~~
# SOURCE
~~~roc
app [main!] { pf: platform "../basic-cli/main.roc" }

identity : a -> a
identity = |x| x

made : [Wrap(List(a))]
made = Wrap(identity([]))

nums : [Wrap(List(U64))]
nums = made

strs : [Wrap(List(Str))]
strs = made

main! = |_| {}
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
KwApp,OpenSquare,LowerIdent,CloseSquare,OpenCurly,LowerIdent,OpColon,KwPlatform,StringStart,StringPart,StringEnd,CloseCurly,
LowerIdent,OpColon,LowerIdent,OpArrow,LowerIdent,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,LowerIdent,
LowerIdent,OpColon,OpenSquare,UpperIdent,NoSpaceOpenRound,UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,CloseRound,CloseSquare,
LowerIdent,OpAssign,UpperIdent,NoSpaceOpenRound,LowerIdent,NoSpaceOpenRound,OpenSquare,CloseSquare,CloseRound,CloseRound,
LowerIdent,OpColon,OpenSquare,UpperIdent,NoSpaceOpenRound,UpperIdent,NoSpaceOpenRound,UpperIdent,CloseRound,CloseRound,CloseSquare,
LowerIdent,OpAssign,LowerIdent,
LowerIdent,OpColon,OpenSquare,UpperIdent,NoSpaceOpenRound,UpperIdent,NoSpaceOpenRound,UpperIdent,CloseRound,CloseRound,CloseSquare,
LowerIdent,OpAssign,LowerIdent,
LowerIdent,OpAssign,OpBar,Underscore,OpBar,OpenCurly,CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(app
		(provides
			(exposed-lower-ident
				(text "main!")))
		(record-field (name "pf")
			(e-string
				(e-string-part (raw "../basic-cli/main.roc"))))
		(packages
			(record-field (name "pf")
				(e-string
					(e-string-part (raw "../basic-cli/main.roc"))))))
	(statements
		(s-type-anno (name "identity")
			(ty-fn
				(ty-var (raw "a"))
				(ty-var (raw "a"))))
		(s-decl
			(p-ident (raw "identity"))
			(e-lambda
				(args
					(p-ident (raw "x")))
				(e-ident (raw "x"))))
		(s-type-anno (name "made")
			(ty-tag-union
				(tags
					(ty-apply
						(ty (name "Wrap"))
						(ty-apply
							(ty (name "List"))
							(ty-var (raw "a")))))))
		(s-decl
			(p-ident (raw "made"))
			(e-apply
				(e-tag (raw "Wrap"))
				(e-apply
					(e-ident (raw "identity"))
					(e-list))))
		(s-type-anno (name "nums")
			(ty-tag-union
				(tags
					(ty-apply
						(ty (name "Wrap"))
						(ty-apply
							(ty (name "List"))
							(ty (name "U64")))))))
		(s-decl
			(p-ident (raw "nums"))
			(e-ident (raw "made")))
		(s-type-anno (name "strs")
			(ty-tag-union
				(tags
					(ty-apply
						(ty (name "Wrap"))
						(ty-apply
							(ty (name "List"))
							(ty (name "Str")))))))
		(s-decl
			(p-ident (raw "strs"))
			(e-ident (raw "made")))
		(s-decl
			(p-ident (raw "main!"))
			(e-lambda
				(args
					(p-underscore))
				(e-record)))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "identity"))
		(e-lambda
			(args
				(p-assign (ident "x")))
			(e-lookup-local
				(p-assign (ident "x"))))
		(annotation
			(ty-fn (effectful false)
				(ty-rigid-var (name "a"))
				(ty-rigid-var-lookup (ty-rigid-var (name "a"))))))
	(d-let
		(p-assign (ident "made"))
		(e-tag (name "Wrap")
			(args
				(e-call (constraint-fn-var 68)
					(e-lookup-local
						(p-assign (ident "identity")))
					(e-empty_list))))
		(annotation
			(ty-tag-union
				(ty-tag-name (name "Wrap")
					(ty-apply (name "List") (builtin)
						(ty-rigid-var (name "a")))))))
	(d-let
		(p-assign (ident "nums"))
		(e-lookup-local
			(p-assign (ident "made")))
		(annotation
			(ty-tag-union
				(ty-tag-name (name "Wrap")
					(ty-apply (name "List") (builtin)
						(ty-lookup (name "U64") (builtin)))))))
	(d-let
		(p-assign (ident "strs"))
		(e-lookup-local
			(p-assign (ident "made")))
		(annotation
			(ty-tag-union
				(ty-tag-name (name "Wrap")
					(ty-apply (name "List") (builtin)
						(ty-lookup (name "Str") (builtin)))))))
	(d-let
		(p-assign (ident "main!"))
		(e-lambda
			(args
				(p-underscore))
			(e-empty_record))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "a -> a"))
		(patt (type "[Wrap(List(a))]"))
		(patt (type "[Wrap(List(U64))]"))
		(patt (type "[Wrap(List(Str))]"))
		(patt (type "_arg -> {}")))
	(expressions
		(expr (type "a -> a"))
		(expr (type "[Wrap(List(a))]"))
		(expr (type "[Wrap(List(U64))]"))
		(expr (type "[Wrap(List(Str))]"))
		(expr (type "_arg -> {}"))))
~~~
