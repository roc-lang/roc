# META
~~~ini
description=An annotation cannot make a record value polymorphic, even when a field is a lambda: it is rejected, and its uses at two types report nothing further
type=file
~~~
# SOURCE
~~~roc
app [main!] { pf: platform "../basic-cli/main.roc" }

rec : { f : a -> a, n : List(b) }
rec = { f: |x| x, n: [] }

r1 : { f : U64 -> U64, n : List(U64) }
r1 = rec

r2 : { f : Str -> Str, n : List(Str) }
r2 = rec

main! = |_| {}
~~~
# EXPECTED
VALUE IS NOT POLYMORPHIC - annotated_value_not_polymorphic_record_function_field.md:3:1:3:34
# PROBLEMS
~~~clojure
(reports
	(report
		(severity runtime_error)
		(title "Value Is Not Polymorphic")
		(region (start 3 1) (end 3 34))
		(headline
			(reflow "The type annotation on")
			(reflow " ")
			(annotated code "rec")
			(reflow " ")
			(reflow "says it can be used at many types, but")
			(reflow " ")
			(annotated code "rec")
			(reflow " ")
			(reflow "isn't defined as a function (like")
			(reflow " ")
			(annotated code "|x| ...")
			(reflow "), so it can only have one type."))
		(document
			(source-region (file "annotated_value_not_polymorphic_record_function_field.md") (start 3 1) (end 3 34) (annotation error) (line-text "rec : { f : a -> a, n : List(b) }"))
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
			(text "rec : {} -> { f : a -> a, n : List(b) }")
			(line-break)
			(indent 1)
			(text "rec = |{}| { f: |x| x, n: [] }")
			(annotation-end)
			(line-break)
			(reflow "Then call it as")
			(reflow " ")
			(annotated code "rec({})")
			(reflow " ")
			(reflow "wherever you use it."))))
~~~
# TOKENS
~~~zig
KwApp,OpenSquare,LowerIdent,CloseSquare,OpenCurly,LowerIdent,OpColon,KwPlatform,StringStart,StringPart,StringEnd,CloseCurly,
LowerIdent,OpColon,OpenCurly,LowerIdent,OpColon,LowerIdent,OpArrow,LowerIdent,Comma,LowerIdent,OpColon,UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,CloseCurly,
LowerIdent,OpAssign,OpenCurly,LowerIdent,OpColon,OpBar,LowerIdent,OpBar,LowerIdent,Comma,LowerIdent,OpColon,OpenSquare,CloseSquare,CloseCurly,
LowerIdent,OpColon,OpenCurly,LowerIdent,OpColon,UpperIdent,OpArrow,UpperIdent,Comma,LowerIdent,OpColon,UpperIdent,NoSpaceOpenRound,UpperIdent,CloseRound,CloseCurly,
LowerIdent,OpAssign,LowerIdent,
LowerIdent,OpColon,OpenCurly,LowerIdent,OpColon,UpperIdent,OpArrow,UpperIdent,Comma,LowerIdent,OpColon,UpperIdent,NoSpaceOpenRound,UpperIdent,CloseRound,CloseCurly,
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
		(s-type-anno (name "rec")
			(ty-record
				(anno-record-field (name "f")
					(ty-fn
						(ty-var (raw "a"))
						(ty-var (raw "a"))))
				(anno-record-field (name "n")
					(ty-apply
						(ty (name "List"))
						(ty-var (raw "b"))))))
		(s-decl
			(p-ident (raw "rec"))
			(e-record
				(field (field "f")
					(e-lambda
						(args
							(p-ident (raw "x")))
						(e-ident (raw "x"))))
				(field (field "n")
					(e-list))))
		(s-type-anno (name "r1")
			(ty-record
				(anno-record-field (name "f")
					(ty-fn
						(ty (name "U64"))
						(ty (name "U64"))))
				(anno-record-field (name "n")
					(ty-apply
						(ty (name "List"))
						(ty (name "U64"))))))
		(s-decl
			(p-ident (raw "r1"))
			(e-ident (raw "rec")))
		(s-type-anno (name "r2")
			(ty-record
				(anno-record-field (name "f")
					(ty-fn
						(ty (name "Str"))
						(ty (name "Str"))))
				(anno-record-field (name "n")
					(ty-apply
						(ty (name "List"))
						(ty (name "Str"))))))
		(s-decl
			(p-ident (raw "r2"))
			(e-ident (raw "rec")))
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
		(p-assign (ident "rec"))
		(e-runtime-error (tag "erroneous_value_expr"))
		(annotation
			(ty-record
				(field (field "f")
					(ty-fn (effectful false)
						(ty-rigid-var (name "a"))
						(ty-rigid-var-lookup (ty-rigid-var (name "a")))))
				(field (field "n")
					(ty-apply (name "List") (builtin)
						(ty-rigid-var (name "b")))))))
	(d-let
		(p-assign (ident "r1"))
		(e-runtime-error (tag "erroneous_value_expr"))
		(annotation
			(ty-record
				(field (field "f")
					(ty-fn (effectful false)
						(ty-lookup (name "U64") (builtin))
						(ty-lookup (name "U64") (builtin))))
				(field (field "n")
					(ty-apply (name "List") (builtin)
						(ty-lookup (name "U64") (builtin)))))))
	(d-let
		(p-assign (ident "r2"))
		(e-runtime-error (tag "erroneous_value_expr"))
		(annotation
			(ty-record
				(field (field "f")
					(ty-fn (effectful false)
						(ty-lookup (name "Str") (builtin))
						(ty-lookup (name "Str") (builtin))))
				(field (field "n")
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
		(patt (type "{ f: c -> c, n: List(_d) }"))
		(patt (type "{ f: U64 -> U64, n: List(U64) }"))
		(patt (type "{ f: Str -> Str, n: List(Str) }"))
		(patt (type "_arg -> {}")))
	(expressions
		(expr (type "{ f: c -> c, n: List(_d) }"))
		(expr (type "{ f: U64 -> U64, n: List(U64) }"))
		(expr (type "{ f: Str -> Str, n: List(Str) }"))
		(expr (type "_arg -> {}"))))
~~~
