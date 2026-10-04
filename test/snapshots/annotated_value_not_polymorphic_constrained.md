# META
~~~ini
description=A top-level value annotation with a where-constrained type variable is rejected: a value that is not a function cannot be polymorphic
type=file
~~~
# SOURCE
~~~roc
app [main!] { pf: platform "../basic-cli/main.roc" }

items : List(a) where [a.to_str : a -> Str]
items = []

main! = |_| {}
~~~
# EXPECTED
VALUE IS NOT POLYMORPHIC - annotated_value_not_polymorphic_constrained.md:3:1:3:43
# PROBLEMS
~~~clojure
(reports
	(report
		(severity runtime_error)
		(title "Value Is Not Polymorphic")
		(region (start 3 1) (end 3 43))
		(headline
			(reflow "The type annotation on")
			(reflow " ")
			(annotated code "items")
			(reflow " ")
			(reflow "says it can be used at many types, but")
			(reflow " ")
			(annotated code "items")
			(reflow " ")
			(reflow "is not a function, so it can only have one type."))
		(document
			(source-region (file "annotated_value_not_polymorphic_constrained.md") (start 3 1) (end 3 43) (annotation error) (line-text "items : List(a) where [a.to_str : a -> Str]"))
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
			(text "items : {} -> List(a) where [a.to_str : a -> Str]")
			(line-break)
			(indent 1)
			(text "items = |{}| []")
			(annotation-end)
			(line-break)
			(reflow "Then call it as")
			(reflow " ")
			(annotated code "items({})")
			(reflow " ")
			(reflow "wherever you use it."))))
~~~
# TOKENS
~~~zig
KwApp,OpenSquare,LowerIdent,CloseSquare,OpenCurly,LowerIdent,OpColon,KwPlatform,StringStart,StringPart,StringEnd,CloseCurly,
LowerIdent,OpColon,UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,KwWhere,OpenSquare,LowerIdent,NoSpaceDotLowerIdent,OpColon,LowerIdent,OpArrow,UpperIdent,CloseSquare,
LowerIdent,OpAssign,OpenSquare,CloseSquare,
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
		(s-type-anno (name "items")
			(ty-apply
				(ty (name "List"))
				(ty-var (raw "a")))
			(where
				(method (mod-of "a") (name "to_str")
					(ty-fn
						(ty-var (raw "a"))
						(ty (name "Str"))))))
		(s-decl
			(p-ident (raw "items"))
			(e-list))
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
		(p-assign (ident "items"))
		(e-runtime-error (tag "erroneous_value_expr"))
		(annotation
			(ty-apply (name "List") (builtin)
				(ty-rigid-var (name "a")))
			(where
				(method (ty-rigid-var-lookup (ty-rigid-var (name "a"))) (name "to_str")
					(ty-fn (effectful false)
						(ty-rigid-var-lookup (ty-rigid-var (name "a")))
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
		(patt (type "List(_b)"))
		(patt (type "_arg -> {}")))
	(expressions
		(expr (type "List(_b)"))
		(expr (type "_arg -> {}"))))
~~~
