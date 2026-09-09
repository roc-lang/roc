# META
~~~ini
description=fuzz crash
type=snippet
~~~
# SOURCE
~~~roc
C:[0]
~~~
# EXPECTED
UNEXPECTED TYPE SYNTAX - fuzz_crash_066.md:1:4:1:5
MALFORMED TYPE - fuzz_crash_066.md:1:4:1:5
# PROBLEMS
~~~clojure
(reports
	(report
		(severity runtime_error)
		(title "Unexpected Type Syntax")
		(region (start 1 4) (end 1 5))
		(headline
			(reflow "I was parsing a type annotation, and this token cannot start a type here."))
		(document
			(reflow "Types can be type variables, uppercase type names, function types, tuples, records, or tag unions.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "List(U64)")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code "0")
			(text " here.")
			(line-break)
			(line-break)
			(source-region (file "fuzz_crash_066.md") (start 1 4) (end 1 5) (annotation error) (line-text "C:[0]"))))
	(report
		(severity runtime_error)
		(title "Malformed Type")
		(region (start 1 4) (end 1 5))
		(headline
			(reflow "This type annotation is malformed or contains invalid syntax."))
		(document
			(source-region (file "fuzz_crash_066.md") (start 1 4) (end 1 5) (annotation error) (line-text "C:[0]")))))
~~~
# TOKENS
~~~zig
UpperIdent,OpColon,OpenSquare,Int,CloseSquare,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-mod)
	(statements
		(s-type-decl
			(header (name "C")
				(args))
			(ty-tag-union
				(tags
					(ty-malformed (tag "ty_anno_unexpected_token")))))))
~~~
# FORMATTED
~~~roc
C : []
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(s-alias-decl
		(ty-header (name "C"))
		(ty-tag-union
			(ty-malformed))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs)
	(type_decls
		(alias (type "Error")
			(ty-header (name "C"))))
	(expressions))
~~~
