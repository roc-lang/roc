# META
~~~ini
description=Stray expressions before declarations in a type mod's associated block are reported instead of being silently dropped (issue 12802)
type=file:Hello.roc
~~~
# SOURCE
~~~roc
Hello :: {}.{
    buncha random words num = 3
    more random words hello = |name| {
		"Hello ${name}!\n"
	}
}
~~~
# EXPECTED
UNEXPECTED ASSOCIATED EXPRESSION - assoc_stray_expressions.md:2:5:2:11
UNEXPECTED ASSOCIATED EXPRESSION - assoc_stray_expressions.md:2:12:2:18
UNEXPECTED ASSOCIATED EXPRESSION - assoc_stray_expressions.md:2:19:2:24
UNEXPECTED ASSOCIATED EXPRESSION - assoc_stray_expressions.md:3:5:3:9
UNEXPECTED ASSOCIATED EXPRESSION - assoc_stray_expressions.md:3:10:3:16
UNEXPECTED ASSOCIATED EXPRESSION - assoc_stray_expressions.md:3:17:3:22
# PROBLEMS
~~~clojure
(reports
	(report
		(severity runtime_error)
		(title "Unexpected Associated Expression")
		(region (start 2 5) (end 2 11))
		(headline
			(reflow "I was parsing associated items for a nominal type, and I found a plain expression."))
		(document
			(reflow "Associated item blocks can contain associated types and values. Remove this expression or turn it into a named associated value.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "Id := [Id(U64)].{")
			(line-break)
			(indent 1)
			(text "    zero = Id(0)")
			(line-break)
			(indent 1)
			(text "}")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code "buncha")
			(text " here.")
			(line-break)
			(reflow "Names that start with lowercase letters are value names or record field names, depending on the surrounding syntax.")
			(line-break)
			(line-break)
			(source-region (file "assoc_stray_expressions.md") (start 2 5) (end 2 11) (annotation error) (line-text "    buncha random words num = 3"))))
	(report
		(severity runtime_error)
		(title "Unexpected Associated Expression")
		(region (start 2 12) (end 2 18))
		(headline
			(reflow "I was parsing associated items for a nominal type, and I found a plain expression."))
		(document
			(reflow "Associated item blocks can contain associated types and values. Remove this expression or turn it into a named associated value.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "Id := [Id(U64)].{")
			(line-break)
			(indent 1)
			(text "    zero = Id(0)")
			(line-break)
			(indent 1)
			(text "}")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code "random")
			(text " here.")
			(line-break)
			(reflow "Names that start with lowercase letters are value names or record field names, depending on the surrounding syntax.")
			(line-break)
			(line-break)
			(source-region (file "assoc_stray_expressions.md") (start 2 12) (end 2 18) (annotation error) (line-text "    buncha random words num = 3"))))
	(report
		(severity runtime_error)
		(title "Unexpected Associated Expression")
		(region (start 2 19) (end 2 24))
		(headline
			(reflow "I was parsing associated items for a nominal type, and I found a plain expression."))
		(document
			(reflow "Associated item blocks can contain associated types and values. Remove this expression or turn it into a named associated value.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "Id := [Id(U64)].{")
			(line-break)
			(indent 1)
			(text "    zero = Id(0)")
			(line-break)
			(indent 1)
			(text "}")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code "words")
			(text " here.")
			(line-break)
			(reflow "Names that start with lowercase letters are value names or record field names, depending on the surrounding syntax.")
			(line-break)
			(line-break)
			(source-region (file "assoc_stray_expressions.md") (start 2 19) (end 2 24) (annotation error) (line-text "    buncha random words num = 3"))))
	(report
		(severity runtime_error)
		(title "Unexpected Associated Expression")
		(region (start 3 5) (end 3 9))
		(headline
			(reflow "I was parsing associated items for a nominal type, and I found a plain expression."))
		(document
			(reflow "Associated item blocks can contain associated types and values. Remove this expression or turn it into a named associated value.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "Id := [Id(U64)].{")
			(line-break)
			(indent 1)
			(text "    zero = Id(0)")
			(line-break)
			(indent 1)
			(text "}")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code "more")
			(text " here.")
			(line-break)
			(reflow "Names that start with lowercase letters are value names or record field names, depending on the surrounding syntax.")
			(line-break)
			(line-break)
			(source-region (file "assoc_stray_expressions.md") (start 3 5) (end 3 9) (annotation error) (line-text "    more random words hello = |name| {"))))
	(report
		(severity runtime_error)
		(title "Unexpected Associated Expression")
		(region (start 3 10) (end 3 16))
		(headline
			(reflow "I was parsing associated items for a nominal type, and I found a plain expression."))
		(document
			(reflow "Associated item blocks can contain associated types and values. Remove this expression or turn it into a named associated value.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "Id := [Id(U64)].{")
			(line-break)
			(indent 1)
			(text "    zero = Id(0)")
			(line-break)
			(indent 1)
			(text "}")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code "random")
			(text " here.")
			(line-break)
			(reflow "Names that start with lowercase letters are value names or record field names, depending on the surrounding syntax.")
			(line-break)
			(line-break)
			(source-region (file "assoc_stray_expressions.md") (start 3 10) (end 3 16) (annotation error) (line-text "    more random words hello = |name| {"))))
	(report
		(severity runtime_error)
		(title "Unexpected Associated Expression")
		(region (start 3 17) (end 3 22))
		(headline
			(reflow "I was parsing associated items for a nominal type, and I found a plain expression."))
		(document
			(reflow "Associated item blocks can contain associated types and values. Remove this expression or turn it into a named associated value.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "Id := [Id(U64)].{")
			(line-break)
			(indent 1)
			(text "    zero = Id(0)")
			(line-break)
			(indent 1)
			(text "}")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code "words")
			(text " here.")
			(line-break)
			(reflow "Names that start with lowercase letters are value names or record field names, depending on the surrounding syntax.")
			(line-break)
			(line-break)
			(source-region (file "assoc_stray_expressions.md") (start 3 17) (end 3 22) (annotation error) (line-text "    more random words hello = |name| {")))))
~~~
# TOKENS
~~~zig
UpperIdent,OpDoubleColon,OpenCurly,CloseCurly,Dot,OpenCurly,
LowerIdent,LowerIdent,LowerIdent,LowerIdent,OpAssign,Int,
LowerIdent,LowerIdent,LowerIdent,LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,OpenCurly,
StringStart,StringPart,OpenStringInterpolation,LowerIdent,CloseStringInterpolation,StringPart,StringEnd,
CloseCurly,
CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-mod)
	(statements
		(s-type-decl
			(header (name "Hello")
				(args))
			(ty-record)
			(associated
				(e-ident (raw "buncha"))
				(e-ident (raw "random"))
				(e-ident (raw "words"))
				(s-decl
					(p-ident (raw "num"))
					(e-int (raw "3")))
				(e-ident (raw "more"))
				(e-ident (raw "random"))
				(e-ident (raw "words"))
				(s-decl
					(p-ident (raw "hello"))
					(e-lambda
						(args
							(p-ident (raw "name")))
						(e-block
							(statements
								(e-string
									(e-string-part (raw "Hello "))
									(e-ident (raw "name"))
									(e-string-part (raw "!\n")))))))))))
~~~
# FORMATTED
~~~roc
Hello :: {}.{
	buncha
	random
	words
	num = 3
	more
	random
	words
	hello = |name| {
		"Hello ${name}!\n"
	}
}
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "Hello.num"))
		(e-num (value "3")))
	(d-let
		(p-assign (ident "Hello.hello"))
		(e-lambda
			(args
				(p-assign (ident "name")))
			(e-block
				(e-block
					(s-let
						(p-assign (ident "#interp_0"))
						(e-lookup-local
							(p-assign (ident "name"))))
					(e-interpolation (constraint-fn-var 239) (dispatcher-var 16)
						(first
							(e-literal (string "Hello ")))
						(parts
							(e-lookup-local
								(p-assign (ident "#interp_0")))
							(e-literal (string "!
"))))))))
	(s-nominal-decl
		(ty-header (name "Hello"))
		(ty-record)))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Dec"))
		(patt (type "item -> a where [a.from_interpolation : List(Str) -> Try(Iter(item) -> a, [InvalidInterpolation(Str)])]")))
	(type_decls
		(nominal (type "Hello")
			(ty-header (name "Hello"))))
	(expressions
		(expr (type "Dec"))
		(expr (type "item -> a where [a.from_interpolation : List(Str) -> Try(Iter(item) -> a, [InvalidInterpolation(Str)])]"))))
~~~
