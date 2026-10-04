# META
~~~ini
description=An annotation cannot make a non-function value polymorphic: `empty : List(a)` on `empty = []` is rejected, and the use at a second type is an ordinary mismatch
type=file
~~~
# SOURCE
~~~roc
app [main!] { pf: platform "../basic-cli/main.roc" }

empty : List(a)
empty = []

nums : List(U64)
nums = empty

strs : List(Str)
strs = empty

main! = |_| {}
~~~
# EXPECTED
VALUE IS NOT POLYMORPHIC - annotated_value_not_polymorphic_multi_type.md:3:1:3:16
TYPE MISMATCH - annotated_value_not_polymorphic_multi_type.md:10:8:10:13
# PROBLEMS
~~~clojure
(reports
	(report
		(severity runtime_error)
		(title "Value Is Not Polymorphic")
		(region (start 3 1) (end 3 16))
		(headline
			(reflow "The type annotation on")
			(reflow " ")
			(annotated code "empty")
			(reflow " ")
			(reflow "says it can be used at many types, but")
			(reflow " ")
			(annotated code "empty")
			(reflow " ")
			(reflow "is not a function, so it can only have one type."))
		(document
			(source-region (file "annotated_value_not_polymorphic_multi_type.md") (start 3 1) (end 3 16) (annotation error) (line-text "empty : List(a)"))
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
			(text "empty : {} -> List(a)")
			(line-break)
			(indent 1)
			(text "empty = |{}| []")
			(annotation-end)
			(line-break)
			(reflow "Then call it as")
			(reflow " ")
			(annotated code "empty({})")
			(reflow " ")
			(reflow "wherever you use it.")))
	(report
		(severity runtime_error)
		(title "Type Mismatch")
		(region (start 10 8) (end 10 13))
		(headline
			(reflow "This expression is used in an unexpected way."))
		(document
			(source-region (file "annotated_value_not_polymorphic_multi_type.md") (start 10 8) (end 10 13) (annotation error) (line-text "strs = empty"))
			(line-break)
			(reflow "It has the type:")
			(line-break)
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "List(U64)")
			(annotation-end)
			(line-break)
			(line-break)
			(reflow "But the annotation says it should be:")
			(line-break)
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "List(Str)")
			(annotation-end))))
~~~
# TOKENS
~~~zig
KwApp,OpenSquare,LowerIdent,CloseSquare,OpenCurly,LowerIdent,OpColon,KwPlatform,StringStart,StringPart,StringEnd,CloseCurly,
LowerIdent,OpColon,UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,
LowerIdent,OpAssign,OpenSquare,CloseSquare,
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
		(s-type-anno (name "empty")
			(ty-apply
				(ty (name "List"))
				(ty-var (raw "a"))))
		(s-decl
			(p-ident (raw "empty"))
			(e-list))
		(s-type-anno (name "nums")
			(ty-apply
				(ty (name "List"))
				(ty (name "U64"))))
		(s-decl
			(p-ident (raw "nums"))
			(e-ident (raw "empty")))
		(s-type-anno (name "strs")
			(ty-apply
				(ty (name "List"))
				(ty (name "Str"))))
		(s-decl
			(p-ident (raw "strs"))
			(e-ident (raw "empty")))
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
		(p-assign (ident "empty"))
		(e-runtime-error (tag "erroneous_value_expr"))
		(annotation
			(ty-apply (name "List") (builtin)
				(ty-rigid-var (name "a")))))
	(d-let
		(p-assign (ident "nums"))
		(e-runtime-error (tag "erroneous_value_use"))
		(annotation
			(ty-apply (name "List") (builtin)
				(ty-lookup (name "U64") (builtin)))))
	(d-let
		(p-assign (ident "strs"))
		(e-runtime-error (tag "erroneous_value_use"))
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
		(patt (type "List(U64)"))
		(patt (type "List(U64)"))
		(patt (type "List(Str)"))
		(patt (type "_arg -> {}")))
	(expressions
		(expr (type "List(U64)"))
		(expr (type "List(U64)"))
		(expr (type "List(Str)"))
		(expr (type "_arg -> {}"))))
~~~
