# META
~~~ini
description=Canonicalize crash: type annotation node assumed where nominal declaration is not a type header
type=file
~~~
# SOURCE
~~~roc
T := [].{
	A ::T.A
}
~~~
# EXPECTED
INVALID RECURSIVE TYPE - fuzz_crash_100.md:2:2:2:9
# PROBLEMS
~~~clojure
(reports
	(report
		(severity runtime_error)
		(title "Invalid Recursive Type")
		(region (start 2 2) (end 2 9))
		(headline
			(reflow "The nominal type")
			(reflow " ")
			(annotated type "T.A")
			(reflow " ")
			(reflow "refers to itself in a way that would make it infinite."))
		(document
			(source-region (file "fuzz_crash_100.md") (start 2 2) (end 2 9) (annotation error) (line-text "\tA ::T.A"))
			(line-break)
			(reflow "Its definition is:")
			(line-break)
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "T.A")
			(annotation-end)
			(line-break)
			(line-break)
			(annotated emphasis "Hint:")
			(reflow " ")
			(reflow "Recursion in a nominal type is only allowed inside a tag union payload or record field—for example")
			(reflow " ")
			(annotated code "ConsList(a) := [Nil, Cons(a, ConsList(a))]")
			(reflow "."))))
~~~
# TOKENS
~~~zig
UpperIdent,OpColonEqual,OpenSquare,CloseSquare,Dot,OpenCurly,
UpperIdent,OpDoubleColon,UpperIdent,NoSpaceDotUpperIdent,
CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-mod)
	(statements
		(s-type-decl
			(header (name "T")
				(args))
			(ty-tag-union
				(tags))
			(associated
				(s-type-decl
					(header (name "A")
						(args))
					(ty (name "T.A")))))))
~~~
# FORMATTED
~~~roc
T := [].{
	A :: T.A
}
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(s-nominal-decl
		(ty-header (name "T"))
		(ty-tag-union))
	(s-nominal-decl
		(ty-header (name "fuzz_crash_100.T.A"))
		(ty-lookup (name "T.A") (local))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs)
	(type_decls
		(nominal (type "T")
			(ty-header (name "T")))
		(nominal (type "Error")
			(ty-header (name "fuzz_crash_100.T.A"))))
	(expressions))
~~~
