# META
~~~ini
description=An annotation cannot make a value bound inside a block polymorphic: `empty : List(a)` on `empty = []` is rejected, the binding is checked without the annotation, and its uses at two types report nothing further
type=file
~~~
# SOURCE
~~~roc
app [main!] { pf: platform "../basic-cli/main.roc" }

main! = |_| {
    empty : List(a)
    empty = []

    nums : List(U64)
    nums = empty

    strs : List(Str)
    strs = empty

    _ = nums
    _ = strs
    {}
}
~~~
# EXPECTED
VALUE IS NOT POLYMORPHIC - annotated_value_not_polymorphic_block_local.md:4:5:4:20
# PROBLEMS
~~~clojure
(reports
	(report
		(severity runtime_error)
		(title "Value Is Not Polymorphic")
		(region (start 4 5) (end 4 20))
		(headline
			(reflow "The type annotation on")
			(reflow " ")
			(annotated code "empty")
			(reflow " ")
			(reflow "says it can be used at many types, but")
			(reflow " ")
			(annotated code "empty")
			(reflow " ")
			(reflow "isn't defined as a function (like")
			(reflow " ")
			(annotated code "|x| ...")
			(reflow "), so it can only have one type."))
		(document
			(source-region (file "annotated_value_not_polymorphic_block_local.md") (start 4 5) (end 4 20) (annotation error) (line-text "    empty : List(a)"))
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
			(reflow "wherever you use it."))))
~~~
# TOKENS
~~~zig
KwApp,OpenSquare,LowerIdent,CloseSquare,OpenCurly,LowerIdent,OpColon,KwPlatform,StringStart,StringPart,StringEnd,CloseCurly,
LowerIdent,OpAssign,OpBar,Underscore,OpBar,OpenCurly,
LowerIdent,OpColon,UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,
LowerIdent,OpAssign,OpenSquare,CloseSquare,
LowerIdent,OpColon,UpperIdent,NoSpaceOpenRound,UpperIdent,CloseRound,
LowerIdent,OpAssign,LowerIdent,
LowerIdent,OpColon,UpperIdent,NoSpaceOpenRound,UpperIdent,CloseRound,
LowerIdent,OpAssign,LowerIdent,
Underscore,OpAssign,LowerIdent,
Underscore,OpAssign,LowerIdent,
OpenCurly,CloseCurly,
CloseCurly,
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
		(s-decl
			(p-ident (raw "main!"))
			(e-lambda
				(args
					(p-underscore))
				(e-block
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
							(p-underscore)
							(e-ident (raw "nums")))
						(s-decl
							(p-underscore)
							(e-ident (raw "strs")))
						(e-record)))))))
~~~
# FORMATTED
~~~roc
app [main!] { pf: platform "../basic-cli/main.roc" }

main! = |_| {
	empty : List(a)
	empty = []

	nums : List(U64)
	nums = empty

	strs : List(Str)
	strs = empty

	_ = nums
	_ = strs
	{}
}
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "main!"))
		(e-lambda
			(args
				(p-underscore))
			(e-block
				(s-let
					(p-assign (ident "empty"))
					(e-runtime-error (tag "erroneous_value_expr")))
				(s-let
					(p-assign (ident "nums"))
					(e-runtime-error (tag "erroneous_value_expr")))
				(s-let
					(p-assign (ident "strs"))
					(e-runtime-error (tag "erroneous_value_expr")))
				(s-let
					(p-underscore)
					(e-lookup-local
						(p-assign (ident "nums"))))
				(s-let
					(p-underscore)
					(e-lookup-local
						(p-assign (ident "strs"))))
				(e-empty_record)))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "_arg -> {}")))
	(expressions
		(expr (type "_arg -> {}"))))
~~~
