# META
~~~ini
description=An annotation cannot make a constructor wrapping a call polymorphic: it is rejected, and the use at a second type is an ordinary mismatch
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
VALUE IS NOT POLYMORPHIC - annotated_value_not_polymorphic_nested_expansive.md:6:1:6:23
TYPE MISMATCH - annotated_value_not_polymorphic_nested_expansive.md:13:8:13:12
# PROBLEMS
~~~clojure
(reports
	(report
		(severity runtime_error)
		(title "Value Is Not Polymorphic")
		(region (start 6 1) (end 6 23))
		(headline
			(reflow "The type annotation on")
			(reflow " ")
			(annotated code "made")
			(reflow " ")
			(reflow "says it can be used at many types, but")
			(reflow " ")
			(annotated code "made")
			(reflow " ")
			(reflow "is not a function, so it can only have one type."))
		(document
			(source-region (file "annotated_value_not_polymorphic_nested_expansive.md") (start 6 1) (end 6 23) (annotation error) (line-text "made : [Wrap(List(a))]"))
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
			(text "made : {} -> [Wrap(List(a))]")
			(line-break)
			(indent 1)
			(text "made = |{}| Wrap(identity([]))")
			(annotation-end)
			(line-break)
			(reflow "Then call it as")
			(reflow " ")
			(annotated code "made({})")
			(reflow " ")
			(reflow "wherever you use it.")))
	(report
		(severity runtime_error)
		(title "Type Mismatch")
		(region (start 13 8) (end 13 12))
		(headline
			(reflow "This expression is used in an unexpected way."))
		(document
			(source-region (file "annotated_value_not_polymorphic_nested_expansive.md") (start 13 8) (end 13 12) (annotation error) (line-text "strs = made"))
			(line-break)
			(reflow "It has the type:")
			(line-break)
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "[Wrap(List(U64))]")
			(annotation-end)
			(line-break)
			(line-break)
			(reflow "But the annotation says it should be:")
			(line-break)
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "[Wrap(List(Str))]")
			(annotation-end))))
~~~
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
		(e-runtime-error (tag "erroneous_value_expr"))
		(annotation
			(ty-tag-union
				(ty-tag-name (name "Wrap")
					(ty-apply (name "List") (builtin)
						(ty-rigid-var (name "a")))))))
	(d-let
		(p-assign (ident "nums"))
		(e-runtime-error (tag "erroneous_value_use"))
		(annotation
			(ty-tag-union
				(ty-tag-name (name "Wrap")
					(ty-apply (name "List") (builtin)
						(ty-lookup (name "U64") (builtin)))))))
	(d-let
		(p-assign (ident "strs"))
		(e-runtime-error (tag "erroneous_value_use"))
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
		(patt (type "[Wrap(List(U64))]"))
		(patt (type "[Wrap(List(U64))]"))
		(patt (type "[Wrap(List(Str))]"))
		(patt (type "_arg -> {}")))
	(expressions
		(expr (type "a -> a"))
		(expr (type "[Wrap(List(U64))]"))
		(expr (type "[Wrap(List(U64))]"))
		(expr (type "[Wrap(List(Str))]"))
		(expr (type "_arg -> {}"))))
~~~
