# META
~~~ini
description=An annotation cannot make an expansive value (a call result) polymorphic: it is rejected, and its uses at two types report nothing further
type=file
~~~
# SOURCE
~~~roc
app [main!] { pf: platform "../basic-cli/main.roc" }

identity : a -> a
identity = |x| x

made : List(a)
made = identity([])

nums : List(U64)
nums = made

strs : List(Str)
strs = made

main! = |_| {}
~~~
# EXPECTED
VALUE IS NOT POLYMORPHIC - annotated_value_not_polymorphic_expansive.md:6:1:6:15
# PROBLEMS
~~~clojure
(reports
	(report
		(severity runtime_error)
		(title "Value Is Not Polymorphic")
		(region (start 6 1) (end 6 15))
		(headline
			(reflow "The type annotation on")
			(reflow " ")
			(annotated code "made")
			(reflow " ")
			(reflow "says it can be used at many types, but")
			(reflow " ")
			(annotated code "made")
			(reflow " ")
			(reflow "isn't defined as a function (like")
			(reflow " ")
			(annotated code "|x| ...")
			(reflow "), so it can only have one type."))
		(document
			(source-region (file "annotated_value_not_polymorphic_expansive.md") (start 6 1) (end 6 15) (annotation error) (line-text "made : List(a)"))
			(line-break)
			(line-break)
			(reflow "If you want me to infer its type, write")
			(reflow " ")
			(annotated code "_")
			(reflow " ")
			(reflow "in place of each type variable, or write a concrete type.")
			(line-break)
			(line-break)
			(reflow "If you want to use it at many types, make it a function that takes")
			(reflow " ")
			(annotated code "{}")
			(reflow ":")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "made : {} -> List(a)")
			(line-break)
			(indent 1)
			(text "made = |{}| identity([])")
			(annotation-end)
			(line-break)
			(reflow "Then call it as")
			(reflow " ")
			(annotated code "made({})")
			(reflow " ")
			(reflow "wherever you use it."))))
~~~
# TOKENS
~~~zig
KwApp,OpenSquare,LowerIdent,CloseSquare,OpenCurly,LowerIdent,OpColon,KwPlatform,StringStart,StringPart,StringEnd,CloseCurly,
LowerIdent,OpColon,LowerIdent,OpArrow,LowerIdent,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,LowerIdent,
LowerIdent,OpColon,UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,
LowerIdent,OpAssign,LowerIdent,NoSpaceOpenRound,OpenSquare,CloseSquare,CloseRound,
LowerIdent,OpColon,UpperIdent,NoSpaceOpenRound,UpperIdent,CloseRound,
LowerIdent,OpAssign,LowerIdent,
LowerIdent,OpColon,UpperIdent,NoSpaceOpenRound,UpperIdent,CloseRound,
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
			(ty-apply
				(ty (name "List"))
				(ty-var (raw "a"))))
		(s-decl
			(p-ident (raw "made"))
			(e-apply
				(e-ident (raw "identity"))
				(e-list)))
		(s-type-anno (name "nums")
			(ty-apply
				(ty (name "List"))
				(ty (name "U64"))))
		(s-decl
			(p-ident (raw "nums"))
			(e-ident (raw "made")))
		(s-type-anno (name "strs")
			(ty-apply
				(ty (name "List"))
				(ty (name "Str"))))
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
		(e-runtime-error (tag "erroneous_value_expr"))
		(annotation
			(ty-apply (name "List") (builtin)
				(ty-rigid-var (name "a")))))
	(d-let
		(p-assign (ident "nums"))
		(e-runtime-error (tag "erroneous_value_expr"))
		(annotation
			(ty-apply (name "List") (builtin)
				(ty-lookup (name "U64") (builtin)))))
	(d-let
		(p-assign (ident "strs"))
		(e-runtime-error (tag "erroneous_value_expr"))
		(annotation
			(ty-apply (name "List") (builtin)
				(ty-lookup (name "Str") (builtin)))))
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
		(patt (type "List(_b)"))
		(patt (type "List(U64)"))
		(patt (type "List(Str)"))
		(patt (type "_arg -> {}")))
	(expressions
		(expr (type "a -> a"))
		(expr (type "List(_b)"))
		(expr (type "List(U64)"))
		(expr (type "List(Str)"))
		(expr (type "_arg -> {}"))))
~~~
