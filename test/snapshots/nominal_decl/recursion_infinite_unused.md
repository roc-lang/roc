# META
~~~ini
description=An invalid recursive nominal declaration is rejected even when nothing uses it
type=snippet
~~~
# SOURCE
~~~roc
T := (T, U64)

main = 0
~~~
# EXPECTED
INVALID RECURSIVE TYPE - recursion_infinite_unused.md:1:1:1:14
# PROBLEMS
~~~clojure
(reports
	(report
		(severity runtime_error)
		(title "Invalid Recursive Type")
		(region (start 1 1) (end 1 14))
		(headline
			(reflow "The nominal type")
			(reflow " ")
			(annotated type "T")
			(reflow " ")
			(reflow "refers to itself in a way that would make it infinite."))
		(document
			(source-region (file "recursion_infinite_unused.md") (start 1 1) (end 1 14) (annotation error) (line-text "T := (T, U64)"))
			(line-break)
			(reflow "Its definition is:")
			(line-break)
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "(T, U64)")
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
UpperIdent,OpColonEqual,OpenRound,UpperIdent,Comma,UpperIdent,CloseRound,
LowerIdent,OpAssign,Int,
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
			(ty-tuple
				(ty (name "T"))
				(ty (name "U64"))))
		(s-decl
			(p-ident (raw "main"))
			(e-int (raw "0")))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "main"))
		(e-num (value "0")))
	(s-nominal-decl
		(ty-header (name "T"))
		(ty-tuple
			(ty-lookup (name "T") (local))
			(ty-lookup (name "U64") (builtin)))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Dec")))
	(type_decls
		(nominal (type "Error")
			(ty-header (name "T"))))
	(expressions
		(expr (type "Dec"))))
~~~
